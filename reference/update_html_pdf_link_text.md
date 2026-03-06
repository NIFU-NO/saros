# Update Link Text for a PDF in an HTML File

Finds `<a>` elements whose `href` points to the given PDF filename and
replaces their inner text with the specified title.

## Usage

``` r
update_html_pdf_link_text(html_path, pdf_filename, title)
```

## Arguments

- html_path:

  Path to the HTML file.

- pdf_filename:

  PDF filename (basename only, e.g. `"report.pdf"`).

- title:

  New link text.

## Value

Invisible logical; `TRUE` if replacements were made, `FALSE` otherwise.
