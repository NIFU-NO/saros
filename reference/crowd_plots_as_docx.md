# Write Plots to Word Document (DOCX)

High-level function to write a list of mschart plots directly to a Word
document file, with optional section headings and chart labels.
Simplifies the workflow for generating DOCX reports from
Quarto/RMarkdown by handling all officer boilerplate internally.

## Usage

``` r
crowd_plots_as_docx(
  plot_list,
  path,
  docx_template = NULL,
  heading_style = "heading 2",
  prefix_style = "Normal",
  add_dep_label_prefix = TRUE,
  chart_width = NULL,
  chart_height = NULL,
  extract_metadata = TRUE
)
```

## Arguments

- plot_list:

  Either:

  - A named list of mschart objects (from
    `makeme(..., type = "cat_plot_docx", docx_return_object = TRUE)`)

  - A `saros_officer_plots` object (from
    [`crowd_plots_as_officer()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_officer.md))

- path:

  Character. File path where the DOCX should be saved (e.g.,
  `"output.docx"`).

- docx_template:

  Optional template passed to saros' internal `use_docx()` helper. Can
  be a file path string, `NULL` for default template, or FALSE to skip
  template.

- heading_style:

  Character. officer paragraph style for section headings (plot names).
  Default is `"heading 2"`.

- prefix_style:

  Character. officer paragraph style for prefix text (main question
  labels). Default is `"Normal"`. Only used when
  `add_dep_label_prefix = TRUE`.

- add_dep_label_prefix:

  Logical. If `TRUE` (default), adds the main question text (from
  [`get_dep_label_prefix()`](https://nifu-no.github.io/saros/reference/get_dep_label_prefix.md))
  as a paragraph before each chart.

- chart_width:

  Numeric or `NULL`. Width in inches for charts. If `NULL` (default),
  uses saros' internal `get_docx_dims()` helper based on template page
  layout.

- chart_height:

  Numeric or `NULL`. Height in inches for charts. If `NULL` (default),
  uses saros' internal `get_docx_dims()` helper based on template page
  layout.

- extract_metadata:

  Logical. If `TRUE` (default), automatically extracts metadata from
  plots using
  [`crowd_plots_as_officer()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_officer.md)
  when `plot_list` is a plain list. Ignored if `plot_list` is already a
  `saros_officer_plots` object.

## Value

Invisible file path to the created DOCX file.

## Details

This function provides a simple, single-call interface for writing Word
documents from saros plots, ideal for Quarto workflows where you want:

- **One QMD file → One DOCX file** (no chapter merging)

- **Minimal code in .qmd chunks** (just
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) +
  `crowd_plots_as_docx()`)

- **Automatic layout handling** (page dimensions, chart sizing, heading
  styles)

**Typical Workflow:**

1.  Generate mschart objects with
    `makeme(..., type = "cat_plot_docx", docx_return_object = TRUE, crowd = ...)`

2.  Call `crowd_plots_as_docx(plots, path = "report.docx")` to write the
    file

**Empty Plot Lists:** If `plot_list` contains no valid mschart objects,
the function:

- Issues a warning via
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)

- Creates an empty DOCX file at the specified path

- Returns the path invisibly

## See also

- [`crowd_plots_as_officer()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_officer.md)
  for obtaining a structured plot object

- [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  for the Quarto/HTML equivalent

- [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) for
  creating plots with crowd parameter

- [`officer::read_docx()`](https://davidgohel.github.io/officer/reference/read_docx.html)
  for manual DOCX assembly

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate mschart objects for Word
plots <- makeme(
  data = ex_survey,
  dep = b_1:b_3,
  crowd = c("target", "others"),
  mesos_var = "f_uni",
  mesos_group = "Uni of A",
  type = "cat_plot_docx",
  docx_return_object = TRUE
)

# Write directly to a Word file
crowd_plots_as_docx(plots, path = "survey_results.docx")

# With custom template and styling
crowd_plots_as_docx(
  plots,
  path = "styled_report.docx",
  docx_template = "my_template.docx",
  heading_style = "Heading 1",
  prefix_style = "Body Text",
  chart_width = 6,
  chart_height = 4
)

# Without prefix labels
crowd_plots_as_docx(
  plots,
  path = "minimal_report.docx",
  add_dep_label_prefix = FALSE
)
} # }
```
