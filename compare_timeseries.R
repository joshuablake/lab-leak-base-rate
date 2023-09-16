library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

incident_counts = readr::read_csv("2023-08_lab-leaks/lab-accidents.csv") |>
    filter(!is.na(Year))  |> # Extra rows added when copying from web
    mutate(
        year = case_match(
            Year,
            "Late 1970s" ~ 1977,
            .default = suppressWarnings(as.integer(stringr::str_sub(Year, end = 4)))
        )
    ) |>
    assertr::assert(assertr::not_na, year) |>
    count(year) |>
    complete(
        year = 1975:2016,
        fill = list(n = 0)
    ) |>
    mutate(type = "Lab accidents")

lab_counts = readr::read_csv("2023-08_lab-leaks/bsl4.csv") |>
    group_by(year = floor(year)) |>
    summarise(labs = floor(min(labs))) |>
    transmute(
        year,
        `New BSL4 labs` = labs - lag(labs),
        `Total BSL4 labs` = labs,
    ) |>
    pivot_longer(-year, names_to = "type", values_to = "n") |>
    filter(!is.na(n))

pubmed_counts = readr::read_csv("2023-08_lab-leaks/pubmed.csv", skip = 1) |>
    filter(Year <= 2022) |>
    rename(year = Year, n = Count) |>
    complete(
        year = 1900:2022,
        fill = list(n = 0)
    ) |>
    mutate(type = "Pubmed articles")

web_of_science_counts = readr::read_tsv("2023-08_lab-leaks/web-of-science.tsv") |>
    rename(year = `Publication Years`, n = `Record Count`) |>
    complete(
        year = 1953:2022,
        fill = list(n = 0)
    ) |>
    select(year, n) |>
    filter(year <= 2022) |>
    mutate(type = "Web of Science articles")

web_of_science_counts |>
    ggplot(aes(year, n)) +
    geom_point() +
    geom_smooth()


data = bind_rows(incident_counts, lab_counts, pubmed_counts) |>
    mutate(relative_n = n / max(n), .by = type) |>
    filter(year > 1945) |>
    mutate(rel_year = year - 1945)

fits = data |>
    nest(data = -type) |>
    mutate(
        fit = map(data, ~glm(n ~ year, family = "poisson", data = .x)),
        fit_aug = map(fit, broom::augment, se_fit = TRUE),
        fit_tidy = map(fit, broom::tidy, conf.int = TRUE),
    )
    
fits |>
    unnest(fit_aug) |>
    mutate(
        .fitted.n = exp(.fitted),
        ymin = qpois(0.025, exp(.fitted)),
        ymax = qpois(0.975, exp(.fitted)),
    ) |>
    mutate(across(c(ymin, ymax, n, .fitted.n), ~.x / max(n)), .by = type) |>
    ggplot(aes(year, .fitted.n)) +
    geom_line(aes(color = type)) +
    geom_ribbon(
        aes(year, ymin = ymin, ymax = ymax, fill = type),
        alpha = 0.3,
    ) +
    geom_point(aes(year, n)) +
    theme_minimal()

# Point estiamte and CI visualisation
fits |>
    unnest(fit_tidy) |>
    filter(term == "year") |>
    mutate(
        ymin = (estimate - 1.96 * std.error),
        ymax = (estimate + 1.96 * std.error),
    ) |>
    ggplot(aes(type, estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
    coord_flip() +
    theme_minimal()

joint_data = data |>
    filter(type != "Total BSL4 labs")

joint_fits = tibble::tribble(
    ~name, ~formula,
    "Base", n ~ year + type,
    "Interaction", n ~ year * type,
) |>
    mutate(
        fit = map(formula, ~glm(.x, family = "poisson", data = joint_data)),
        AIC = map_dbl(fit, AIC),
        BIC = map_dbl(fit, BIC),
        fit_tidy = map(fit, broom::tidy, conf.int = TRUE),
        fit_aug = map(fit, broom::augment, se_fit = TRUE),
    )

joint_fits |>
    unnest(fit_aug) |>
    mutate(
        .fitted.n = exp(.fitted),
        ymin = qpois(0.025, exp(.fitted)),
        ymax = qpois(0.975, exp(.fitted)),
    ) |>
    ggplot(aes(year, .fitted.n)) +
    geom_line(aes(color = type)) +
    geom_ribbon(
        aes(year, ymin = ymin, ymax = ymax, fill = type),
        alpha = 0.3,
    ) +
    geom_point(aes(year, n)) +
    facet_grid(type~name, scales = "free_y") +
    theme_minimal()

joint_fits |>
    unnest(fit_tidy) |>
    select(name, term, estimate, p.value)

lab_data = data |>
    filter(type == "New BSL4 labs" | type == "Lab accidents")
final_fit = glm(
    n ~ rel_year + type,
    family = "poisson",
    data = lab_data
)
final_fit_interact = glm(
    n ~ rel_year * type,
    family = "poisson",
    data = lab_data
)
BIC(final_fit, final_fit_interact)
final_fit_interact |>
    broom::tidy(conf.int = TRUE)
# No evidence for interaction
# Hierarchical fits failed

summary(final_fit)

# Exponential integrals assuming effort up to and including cutoff is 1.
# That means that the yearly effort is growth * exp(growth * year)
# which has indefinite integral exp(growth * year).
growth_rate = coef(final_fit)[2]
tribble(
    ~scenario, ~cutoff_date, ~events,
    "Pre-COVID", 2018, 1,
    "COVID leaked", 2022, 2,
    "COVID not leak", 2022, 1,
) |>
    mutate(
        rel_cutoff_date = 2023 - cutoff_date,
        rel_effort_next_decade = exp(growth_rate * (2023 - cutoff_date)) * (exp(growth_rate * 10) - 1),
        mean_leaks = rel_effort_next_decade * (1/3 + events),
        p_leak = 1 - (1 / (1 + rel_effort_next_decade)) ^ (1/3 + events),
    ) |>
    select(-rel_cutoff_date)
