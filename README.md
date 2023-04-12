
<!-- README.md is generated from README.Rmd. Please edit that file -->

# saros: Semi-Automatic Reporting of Ordinary Surveys, using Tidy Syntax

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/sda030/saros/branch/main/graph/badge.svg)](https://app.codecov.io/gh/sda030/saros?branch=main)
\#\> [![CRAN
status](https://www.r-pkg.org/badges/version/saros)](https://CRAN.R-project.org/package=SAROS)
[![R-CMD-check](https://github.com/NIFU-NO/saros/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NIFU-NO/saros/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Introduction

saros (Semi-Automatic Reporting of Ordinary Surveys) is an R package
designed to handle repeating surveys within the same project that occur
annually or biannually. It aims to automate the process of summarizing
and reporting on survey data, helping researchers save time and maintain
consistency across survey iterations. Specifically, SAROS produces
highly customizable figures, text, analyses and reports for a batch of
possible dependent-independent relations of possible interest.

### Connection to the Astronomical concept of Saros

The term “saros” also refers to a cyclical phenomenon in astronomy known
as the Saros cycle, which represents a period of approximately 18 years,
11 days, and 8 hours. After one Saros, the Sun, Earth, and Moon return
to approximately the same relative positions, leading to a similar
eclipse.

- Cyclical occurrences: Both versions of SAROS refer to cyclical events
  that recur at regular intervals. In the case of the R package, it
  relates to surveys that are repeated annually or biannually, while in
  astronomy, it refers to the recurring pattern of eclipses.

- Consistency: The SAROS R package aims to maintain consistency in
  reporting and analysis across survey iterations, just as the
  astronomical Saros cycle represents a predictable pattern in the
  occurrence of solar and lunar eclipses.

- Time-saving: The SAROS R package is designed to automate and
  streamline the process of summarizing and reporting on survey data,
  saving researchers time and effort. Similarly, the Saros cycle is a
  useful tool for astronomers to predict and plan for future eclipses,
  making it easier to study these events without having to make complex
  calculations each time.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("sda030/saros")
```

## Initialize a social science project

``` r
#> saros::initialize(folder = getwd())
```

``` r
# Define temporary folder for storing the elements
library(saros)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
temp_folder <- tempdir()
data_overview <-
 ex_survey_ch_overview %>%
  saros::refine_data_overview(data = ex_survey1,
                              label_separator = " - ",
                              name_separator = "_")


index_filepath <-
  gen_qmd_report(
    data_overview = data_overview,
    elements = ex_survey_elements_list,
    path = temp_folder)
#> Creating file for chapter... '2 Ambivalence'
#> Creating file for chapter... '3 Big mysteries'
#> Creating file for chapter... '4 Confidence'

if(interactive()) {
  browseURL(fs::path(temp_folder, "index.qmd"))
  quarto::quarto_render(fs::path(temp_folder, "index.qmd"))
  browseURL(fs::path(temp_folder, "index.html"))
}
unlink(temp_folder)
```

## Documentation

Please see the [documentation](https://sda030.github.io/saros/).

## Code of Conduct

Please note that the saros project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
