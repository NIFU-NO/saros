# saros: Semi-Automatic Reporting of Ordinary Surveys

# Introduction

{saros} (Semi-Automatic Reporting of Ordinary Surveys) is an R package
designed to handle repeating surveys within the same project that occur
annually or biannually. It aims to automate the process of summarizing
and reporting on survey data, helping researchers save time and maintain
consistency across survey iterations. Specifically, {saros} produces
highly customizable figures, tables, analyses and complex interactive
reports for a batch of possible dependent-independent relations of
possible interest.

## Overview: tools for five stages of the report production

| What                                                      | Note                                                                                                                                                                           | Sub-package               |
|-----------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------|
| Project setup containing ready-made R-scripts and folders | Optional                                                                                                                                                                       | saros.base                |
| Data cleaning                                             | Only supplements [{tidyverse}](https://www.tidyverse.org/)/[{datawizard}](https://easystats.github.io/datawizard/)/ [{labelled}](https://larmarange.github.io/labelled/)-tools | saros.utils (not on CRAN) |
| Report drafting                                           |                                                                                                                                                                                | saros.base                |
| Easy content generation for common standardized outputs   | Standardized output types. Alternatively use your own functions                                                                                                                | saros                     |
| Web access restriction and distribution                   | Optional                                                                                                                                                                       | saros.base                |

## Why saros?

- *Simplicity*: Setting up a reporting system, in particular a
  semi-automated one in Quarto, can be daunting - in particular if some
  of the chapter authors/collaborators have little familiarity with
  R/Python or Quarto/RMarkdown.
  - [Created figures are
    minimal](https://nifu-no.github.io/saros/reference/makeme.html),
    meaning that you are given full power to adjust them post-hoc using
    usual
    [ggplot2::theme()](https://ggplot2.tidyverse.org/reference/theme.html)
    tools, including
    [ggplot2::set_theme()](https://ggplot2.tidyverse.org/reference/theme_get.html).
- *Flexibility*:
  - Several sets of chunk templates are built in, depending on the type
    of report you want. These templates are provided as data frames, so
    you can easily adjust these or create your own.
  - A multitude of settings with optional glue-templating of prefixes,
    infixes and suffixes allows translations and adaptations.
- *Consistency and reproducibility*: Ensure all your outputs within a
  chapter, a report, a project, or even an organization, use the same
  formatting and structure. Figures and plots are purposely simple in
  theming and aesthetics, so that the end-users can specify this
  themselves:
  - When editing/rendering reports, use global options (and override
    when necessary). For the drafted report chunks, the saros content
    functions for plots, tables, etc use inheritance for finding its
    settings:
    - If specified in the function, it will use that setting.
    - If not specified, it will check global options, which can be
      specified for the chapter (qmd-file), the entire report, the
      entire project, or the organization’s settings.
    - If none of the above is specified, function factory defaults are
      used.
  - Also link creation to automatically created
  - Convenience functions set, get and reset options
  - Instead of using probabilistic AI-tools, ensure your reports always
    come out as expected.
- Aesthetics and accessibility:
  - Opinionated, yet highly flexible, interactive {ggiraph}-based
    figures (building on ggplot2) and gt-compatible tables.
- *Performance*: The saros tools draft a report in 3-4 seconds. Spend
  the remaining time thinking about [what to
  write](https://nifu-no.github.io/saros/vignettes/PERSVEEP.qmd).
  - Although built-in plotting functions use (the somewhat slower
    performing) ggplot2, it is easier for the majority to modify such
    plot objects, and to expand with their own compliant plotting
    functions. One can also easily insert other base/lattice-plotting
    functions in the report-drafting templates.

## Workflow

### Preliminaries:

1.  Optionally set up your project directory for [a completely new
    project](https://nifu-no.github.io/saros.base/reference/create_email_credentials.html).
2.  Clean your raw data: - Variables should be stored in the data type
    that they should be displayed as (factor, ordered factor, integer,
    character, etc). Ordered factors will in certain outputs be kept in
    the given order, whereas a set of unordered factors may be
    e.g. `sorted_by = ".upper"` (e.g values of the upper-most
    categories). - Variables should have variable labels, and sets of
    variables should have the same variable label prefix. Prefix and
    suffix can be split by e.g ” - “. Use
    e.g. [{labelled}](https://larmarange.github.io/labelled/reference/index.html#manipulating-variable-labels)
    for most operations. For advanced cleaning, see
    [saros.utils](https://nifu-no.github.io/saros.utils/reference/index.html)

### {saros}-tools

- [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.html)
  makes most types of output for your report you would need for surveys.
  The function can be extended with S3-methods tailored for your needs.
  - The core idea behind this function is the reusability of global
    options, which makes it possible to globally adjust all outputs with
    a small switch.
- [`make_link()`](https://nifu-no.github.io/saros/reference/make_link.html)
  will upon rendering save a plot, dataset or any other object to disk
  and return a “download plot”-link with a unique (hashed) filename.
- [`n_range()`](https://nifu-no.github.io/saros/reference/n_range.html)
  returns the sample size range given a dataset, dependent variables and
  independent variables.
  - An alternative
    [`n_range2()`](https://nifu-no.github.io/saros/reference/n_range2.html)
    allows directly using a makeme() output.
- [`fig_height_h_barchart()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart.html)
  estimates the best figure height for a horizontal barchart, based on a
  data frame, dep and indep variables, and other arguments.
  - An alternative
    [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.html)
    takes a
    [`makeme(type="cat_plot")`](https://nifu-no.github.io/saros/reference/makeme.html).

### {saros.base}-tools

1.  Optionally specify chunk templates for what you want for each set of
    related variables. Or use among the built-in sets of templates.
2.  Optionally, create a chapter_overview (a compact description of
    which dependent (and independent) variables goes in which
    chapter-file). A data.frame where a row is a chapter. Must contain
    at least the columns ‘chapter’ and ‘dep’. ‘dep’ uses
    tidyselect-syntax in each cell. If not using any, all variables are
    processed and placed in the same qmd-file. [More
    details](https://nifu-no.github.io/saros.base/reference/refine_chapter_overview.html)
3.  [Combine the raw data, chunk templates and chapter_overview to make
    a chapter
    structure](https://nifu-no.github.io/saros.base/reference/refine_chapter_overview.html).
    This will create a data frame containing your report structure,
    which can be further tailored. Arguments to
    refine_chapter_overview() allows e.g ignoring:
    - non-significant bivariate relationships between dependent and
      independent variables,
    - low observation counts (sample size) for categories, variables or
      dependent-independent cell combinations, and/or
    - variables with all NA for a given “crowd” (a target group, all
      others combined, or all)
4.  [Draft the
    report](https://nifu-no.github.io/saros.base/reference/draft_report.html),
    using the output from refine_chapter_overview() and your raw data.
    Optionally provide a range of YAML-defaults and
    QMD-prefixes/suffixes to your chapter-files, index-files or full
    report-files, as well as creating chapter-datasets for
    compartmentalized authoring.
5.  After rendering your Quarto Project (using regular Quarto/RStudio
    tools), optionally [batch configure access
    restrictions](https://nifu-no.github.io/saros.base/reference/setup_access_restrictions.html)
    and [send out glue-tailored
    emails](https://nifu-no.github.io/saros.base/reference/create_email_credentials.html)
    to institutions that have participated in your survey, now receiving
    password-protected access to their own report.
6.  Having done this once for a report, you might want to create a
    project template for your organization. Folder structures can be
    [mapped to a YAML
    file](https://nifu-no.github.io/saros.base/reference/generate_yaml_from_directory.html)
    so that it can later be easily [created at once with your preferred
    directory numbering
    scheme](https://nifu-no.github.io/saros.base/reference/download_zip_to_folder.html).
    If located on e.g. Github this can be [downloaded, unzipped and
    placed in a new project
    folder](https://nifu-no.github.io/saros.base/reference/download_zip_to_folder.html).

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("saros")
# pak::pak("NIFU-NO/saros") # Alternatively, latest developer-version at Github
```

## Draft a simple report using defaults, in a temporary folder

``` r
library(saros)
refine_chapter_overview(data = ex_survey,
                        chapter_overview = ex_survey_ch_overview) |>
  draft_report(data = ex_survey)
```

## Draft a customized report

- Dropping

``` r
library(saros)
refine_chapter_overview(data = ex_survey,
                        chapter_overview = ex_survey_ch_overview,
                        chunk_templates = get_chunk_template_defaults(2),
                        always_show_bi_for_indep = "x1_sex",
                        hide_bi_entry_if_sig_above = .05) |>
  draft_report(data = ex_survey,
               prefix_heading_for_group = c(".variable_name_indep" = "---\n"),
               serialized_format = "qs" # qs-format, if installed, is faster than rds
               )
```

## Draft a customized report, one for each participating university

- The auxiliary variable f_uni is included in all chapter datasets so
  that the YAML-header in the QMD-file can refer to it, and chunks later
  refer to that global parameter:

``` markdown
---
params:
    mesos_var: "f_uni"
    mesos_group: "Uni of A" # This can be replaced in a command: `quarto render ch1.qmd -P mesos_group:'Uni of A'`
---
```

``` r
library(saros)
refine_chapter_overview(data = ex_survey,
                        chapter_overview = ex_survey_ch_overview,
                        chunk_templates = get_chunk_template_defaults(2),
                        always_show_bi_for_indep = "x1_sex",
                        hide_bi_entry_if_sig_above = .05) |>
  draft_report(data = ex_survey,
               prefix_heading_for_group = c(".variable_name_indep" = "---\n"),
               auxiliary_variables = "f_uni",
               serialized_format = "qs"
               )
```

## IMPORTANT: saros is free, except in Norway

Outside of Norway, this package can be used in accordance with the MIT
license. However, persons affiliated with Norwegian non-profit or
profit/commercial organizations must have the explicit written
permission from the author for use. Simply put, if your
department/organization competes with NIFU on commissioned or open
research calls within Norway, you are probably not going to get
permission unfortunately.

## Acknowledgements

- The internal code for reordering unordered factors in
  `saros:::make_contents.cat_plot_html()` has been copied (without
  permission) from Julia Silge’s and David Robinson’s excellent
  [{tidytext}](https://juliasilge.github.io/tidytext/)-package.
- The code for loading all saros-packages and resolving conflicts has
  been copied (without permission) from the tidyverse-package and
  adapted.

## Code of Conduct

Please note that the saros project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
