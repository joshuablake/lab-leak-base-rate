library(dplyr)
library(tidyr)
library(ggplot2)

lab_counts_raw = readr::read_csv("2023-08_lab-leaks/bsl4.csv")

lab_counts = lab_counts_raw |>
    group_by(year = floor(year)) |>
    summarise(labs = floor(min(labs))) 

fit_pois_bsl4 = glm(labs ~ year, family = "poisson", data = lab_counts)

fit_pois_bsl4 |>
    broom::augment(se_fit = TRUE) |>
    ggplot() +
    geom_line(aes(year, exp(.fitted))) +
    geom_ribbon(
        aes(year, ymin = exp(.fitted - 1.96 * .se.fit), ymax = exp(.fitted + 1.96 * .se.fit)),
        alpha = 0.3
    ) +
    geom_point(aes(year, labs)) +
    scale_y_log10(minor_breaks = 0:50) +
    theme_minimal()

fit_pois_bsl4 |>
    broom::tidy(conf.int = TRUE)

resid_labs = fit_pois_bsl4 |>
    broom::augment(se_fit = TRUE) |>
    pull(.resid)
acf(resid_labs)

new_labs = lab_counts |>
    mutate(new_labs = labs - lag(labs)) |>
    filter(!is.na(new_labs))

fit_pois_bsl4_new = glm(new_labs ~ year, family = "poisson", data = new_labs)

fit_pois_bsl4_new |>
    broom::augment(se_fit = TRUE) |>
    mutate(
        ymin = qpois(0.025, exp(.fitted)),
        ymax = qpois(0.975, exp(.fitted)),
    ) |>
    ggplot() +
    geom_line(aes(year, exp(.fitted))) +
    geom_ribbon(
        aes(year, ymin = exp(.fitted - 1.96 * .se.fit), ymax = exp(.fitted + 1.96 * .se.fit)),
        alpha = 0.3
    ) +
    geom_ribbon(
        aes(year, ymin = ymin, ymax = ymax),
        alpha = 0.3
    ) +
    geom_point(aes(year, new_labs)) +
    scale_y_log10(minor_breaks = 0:50) +
    theme_minimal()

plot(fit_pois_bsl4_new)
resid_counts = fit_pois_bsl4_new |>
    broom::augment(se_fit = TRUE) |>
    pull(.resid)
acf(resid_counts)

fit_pois_bsl4_new |>
    broom::tidy(conf.int = TRUE)

# Fitting counts looks much better,
# now extrapolate next 10 years


###########################################################################
# Three models: growth rate-based for two fitted models
# and total count of BSL-lab-years
lab_year_count = lab_counts |>
    pull(labs) |>
    sum()
num_labs_2018 = lab_counts |>
    filter(year == 2018) |>
    assertr::verify(length(labs) == 1) |>
    pull(labs)
# Exponential integrals assuming effort up to and including 2018 is 1.
# That means that the yearly effort is growth * exp(growth * year)
# which has indefinite integral exp(growth * year).
tribble(
    ~model, ~effort_to_date, ~effort_2019,
    "cum_counts", 1, coef(fit_pois_bsl4)[2],
    "yearly_count", 1, coef(fit_pois_bsl4_new)[2],
    "actual", lab_year_count, num_labs_2018,
) |>
    mutate(
        num_events_occurred = 1,
        mean_rate_2019 = effort_2019 * (1/3 + num_events_occurred) / effort_to_date,
        p_2019_leak = (effort_to_date / (effort_to_date + effort_2019*10)) ^ (1/3 + num_events_occurred),
    )
