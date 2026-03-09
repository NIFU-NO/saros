# Detect if Running in knitr/Quarto Rendering Context

Helper function to detect if code is running within a knitr/Quarto
rendering context. Useful for creating unified code that works in both
interactive R sessions and when rendering documents.

## Usage

``` r
is_rendering()
```

## Value

Logical. `TRUE` if rendering a document, `FALSE` otherwise.

## Details

This function checks `getOption("knitr.in.progress")` which is set by
knitr during document rendering. This works for:

- Quarto documents (all output formats: HTML, DOCX, PDF, etc.)

- R Markdown documents

- Any knitr-based rendering systems

Returns `FALSE` when running in:

- Interactive R sessions (RStudio console, R terminal)

- R scripts executed outside knitr

- Shiny applications (unless explicitly using knitr)

## See also

[`crowd_output()`](https://nifu-no.github.io/saros/reference/crowd_output.md)
for automatic context-aware output generation

## Examples

``` r
# Check if rendering a document
if (is_rendering()) {
  message("Rendering a document with knitr/Quarto")
} else {
  message("Running in regular R session")
}
#> Running in regular R session

# Use for conditional logic
plot_type <- if (is_rendering()) "cat_plot_html" else "cat_plot_docx"
```
