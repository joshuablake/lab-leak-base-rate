
library(dplyr)
library(tidyr)
library(ggplot2)

incidents_raw = readr::read_csv("2023-08_lab-leaks/lab-accidents.csv")

incidents = incidents_raw |>
    filter(!is.na(Year))  |> # Extra rows added when copying from web
    mutate(
        year = case_match(
            Year,
            "Late 1970s" ~ 1977,
            .default = suppressWarnings(as.integer(stringr::str_sub(Year, end = 4)))
        )
    ) |>
    assertr::assert(assertr::not_na, year)

incident_counts = incidents |>
    count(year) |>
    complete(
        year = 1975:2016,
        fill = list(n = 0)
    )

fit_pois_incidents = glm(n ~ year, family = "poisson", data = incident_counts)

fit_pois_incidents |>
    broom::augment(se_fit = TRUE) |>
    ggplot() +
    geom_line(aes(year, exp(.fitted))) +
    geom_ribbon(
        aes(year, ymin = exp(.fitted - 1.96 * .se.fit), ymax = exp(.fitted + 1.96 * .se.fit)),
        alpha = 0.3
    ) +
    geom_point(aes(year, n)) +
    scale_y_log10(minor_breaks = 1:50, breaks = c(1, 3, 5, 10)) +
    theme_minimal()

fit_pois_incidents |>
    broom::tidy(conf.int = TRUE)

fit_pois_incidents |>

    broom::augment(se_fit = TRUE) |>
    ggplot() +
    geom_line(aes(year, exp(.fitted))) +
    geom_ribbon(
        aes(year, ymin = exp(.fitted - 1.96 * .se.fit), ymax = exp(.fitted + 1.96 * .se.fit)),
        alpha = 0.3
    ) +
    geom_point(aes(year, n)) +
    scale_y_log10(minor_breaks = 0:50) +
    theme_minimal()

fit_pois_incidents |>
    broom::tidy(conf.int = TRUE)
