# Universal Output Function for Crowd Plots and Tables

Automatically detects the appropriate output method based on the
rendering context and input type. Simplifies workflows by providing a
single function that works for both Quarto/knitr rendering (HTML
tabsets/tables) and officer-based DOCX generation.

## Usage

``` r
crowd_output(
  plot_list,
  path = "crowd_output.docx",
  force_format = c("auto", "html", "docx"),
  ...
)
```

## Arguments

- plot_list:

  Either:

  - A named list of ggplot2 objects (for HTML plots)

  - A named list of mschart objects (for DOCX plots)

  - A `saros_officer_plots` object (from
    [`crowd_plots_as_officer()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_officer.md))

  - A data.frame or list of data.frames (for tables)

- path:

  Character. File path for DOCX output (e.g., `"output.docx"`). Only
  used when not in a knitr/Quarto rendering context. Default:
  `"crowd_output.docx"`.

- force_format:

  Character. Force a specific output format:

  - `"auto"` (default): Auto-detect based on context

  - `"html"`: Force HTML tabset output via
    [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)

  - `"docx"`: Force DOCX file output via
    [`crowd_plots_as_docx()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_docx.md)

- ...:

  Additional arguments passed to
  [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  or
  [`crowd_plots_as_docx()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_docx.md)
  depending on the detected/forced format.

## Value

- In knitr/Quarto context: Invisibly returns `NULL` (side effect: prints
  tabset markdown)

- Outside knitr context: Invisibly returns the DOCX file path

## Details

**Context Detection:** The function uses
`getOption("knitr.in.progress")` to detect if code is running within a
knitr/Quarto rendering context:

- **In knitr/Quarto** → Generates HTML tabset via
  [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)

- **Outside knitr** → Writes DOCX file via
  [`crowd_plots_as_docx()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_docx.md)

This allows the same code to work in multiple contexts:

- Quarto → HTML rendering

- Quarto → DOCX rendering (still uses HTML output in document)

- R script → Officer-based DOCX generation

**Input Type Detection:** The function automatically detects and handles
different input types:

- ggplot2 objects → Uses
  [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  or
  [`crowd_plots_as_docx()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_docx.md)

- mschart objects → Uses
  [`crowd_plots_as_docx()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_docx.md)

- `saros_officer_plots` → Uses
  [`crowd_plots_as_docx()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_docx.md)

- data.frame/tables → Uses
  [`crowd_tables_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_tables_as_tabset.md)
  or writes to DOCX

**Typical Workflow:**

    # In a Quarto document - works for both HTML and DOCX output formats
    plots <- makeme(
      data = ex_survey,
      dep = b_1:b_3,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A",
      type = if (is_rendering()) "cat_plot_html" else "cat_plot_docx"
    )
    crowd_output(plots)  # Auto-detects context and renders appropriately

## See also

- [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  for HTML tabset generation

- [`crowd_plots_as_docx()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_docx.md)
  for DOCX file creation

- [`is_rendering()`](https://nifu-no.github.io/saros/reference/is_rendering.md)
  for context detection helper

- [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) for
  creating plots with crowd parameter

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: In a Quarto document (auto-detects HTML context)
plots <- makeme(
  data = ex_survey,
  dep = b_1:b_3,
  crowd = c("target", "others"),
  mesos_var = "f_uni",
  mesos_group = "Uni of A",
  type = if (is_rendering()) "cat_plot_html" else "cat_plot_docx"
)
crowd_output(plots)

# Example 2: In an R script (auto-detects non-knitr context, writes DOCX)
plots <- makeme(
  data = ex_survey,
  dep = b_1:b_3,
  crowd = c("target", "others"),
  mesos_var = "f_uni",
  mesos_group = "Uni of A",
  type = "cat_plot_docx"
)
crowd_output(plots, path = "my_report.docx")

# Example 3: Force specific format
crowd_output(plots, force_format = "docx", path = "forced_output.docx")
} # }
```
