# Check if current output is HTML or officer (non-paginated)

Uses
[`knitr::is_html_output()`](https://rdrr.io/pkg/knitr/man/output_type.html)
when knitr is in progress (covers revealjs, slidy, and other HTML-based
Pandoc formats). Outside of knitr (officer context), returns `TRUE` so
that page breaks are not inserted by default.

## Usage

``` r
is_html_output_or_officer()
```

## Value

Logical scalar.
