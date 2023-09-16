
library(dplyr)
library(tidyr)
library(ggplot2)

pubmed_counts = readr::read_csv("2023-08_lab-leaks/pubmed.csv", skip = 1) |>
    filter(Year <= 2023)

fit_pois_pubmed = glm(Count ~ Year, family = "poisson", data = pubmed_counts)

fit_pois_pubmed |>
    broom::augment(se_fit = TRUE) |>
    mutate(
        ymin = qpois(0.025, exp(.fitted)),
        ymax = qpois(0.975, exp(.fitted)),
    ) |>
    ggplot() +
    geom_line(aes(Year, exp(.fitted))) +
    geom_ribbon(
        aes(Year, ymin = ymin, ymax = ymax),
        alpha = 0.3
    ) +
    geom_point(aes(Year, Count)) +
    scale_y_log10() +
    theme_minimal() +
    coord_cartesian(ylim = c(1, NA))

fit_pois_pubmed |>
    broom::tidy(conf.int = TRUE)

fit_pois_pubmed |>

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

fit_pois_pubmed |>
    broom::tidy(conf.int = TRUE)
