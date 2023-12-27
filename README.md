This repository contains the analysis of lab leaks behind the blogpost ["Forecasting future lab-leak pandemics"]().
The main content is technical details, code, and the data used in the analysis.
[View the compiled Rmd here.](https://joshuablake.co.uk/lab-leak-base-rate/lab-leak-base-rates.html)

Data included:

- bsl4.csv: data extracted from figure 1 of the [Global BioLabs report 2023](https://static1.squarespace.com/static/62fa334a3a6fe8320f5dcf7e/t/6412d3120ee69a4f4efbec1f/1678955285754/KCL0680_BioLabs+Report_Digital.pdf), on the cumulative number of BSL-4 labs
- lab-accidents.csv: data from [Manheim and Lewis (2022)](https://f1000research.com/articles/10-752) on publicly reported lab accidents.
- web-of-science.tsv: the number of virology papers published each year, extracted from the Web of Science database.

Reports included:

- lab-leak-base-rates.Rmd: the main Rmd file containing the analysis for the blogpost.
- complex-models.Rmd: some additional work-in-progress models based on sigmoids to account for the flattening rate of virology papers.

The folder `docs` is generated, and [is viewable online](https://joshuablake.co.uk/lab-leak-base-rate/lab-leak-base-rates.html).

All code is available under [the MIT license](https://choosealicense.com/licenses/mit/).
The data's status is determined by the original sources.