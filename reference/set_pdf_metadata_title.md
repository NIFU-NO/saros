# Set PDF Metadata Title Using Ghostscript

Overwrites the PDF file with an updated version containing the specified
title in its document information dictionary. Uses a temporary
PostScript pdfmark file to avoid shell escaping issues.

## Usage

``` r
set_pdf_metadata_title(pdf_path, title, gs_bin)
```

## Arguments

- pdf_path:

  Path to the PDF file (modified in place).

- title:

  Title string to embed.

- gs_bin:

  Ghostscript binary name or path.

## Value

Invisible logical; `TRUE` on success, `FALSE` on failure.
