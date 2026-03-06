# Quarto Post-Render: Enrich PDF Files with DOCX Titles

A post-render function for Quarto projects that processes rendered PDF
output files. For each PDF, it checks if a corresponding DOCX file with
the same base name exists, extracts the title from the DOCX document
properties, sets it as the PDF metadata title, and updates the link text
in the accompanying `index.html`.

## Usage

``` r
quarto_pdf_post_render(
  output_files = strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n")[[1L]]
)
```

## Arguments

- output_files:

  Character vector of output file paths from Quarto. Defaults to the
  `QUARTO_PROJECT_OUTPUT_FILES` environment variable (newline-separated
  paths set by Quarto during project render). If that variable is empty,
  falls back to reading from stdin as provided by Quarto's post-render
  hook.

## Value

Invisible `NULL`. Called for side effects.

## Details

To use as a Quarto post-render script, add to `_quarto.yml`:

    project:
      post-render:
        - "Rscript -e 'saros::quarto_pdf_post_render()'"

**Processing steps for each `.pdf` file:**

1.  Checks if a `.docx` with the same base name exists in the same
    directory

2.  Extracts the title from the DOCX document properties (via `officer`)

3.  Sets the extracted title as the PDF file's metadata title (requires
    Ghostscript)

4.  Locates `index.html` in the same directory as the PDF

5.  Replaces the `<a>` link text for that PDF with the extracted title

## System Requirements

Setting PDF metadata title requires Ghostscript to be installed and
available on the system PATH:

- **Linux/macOS**: `gs`

- **Windows**: `gswin64c` or `gswin32c`

If Ghostscript is not found, a warning is issued and only the HTML link
text update is performed.

## See also

[`extract_docx_title()`](https://nifu-no.github.io/saros/reference/extract_docx_title.md)
for the DOCX title extraction logic.

## Examples

``` r
if (FALSE) { # \dontrun{
# Called automatically by Quarto post-render, or manually:
quarto_pdf_post_render(c("_site/report/report.pdf"))
} # }
```
