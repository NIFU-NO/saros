# Check if `\\newpage` emission should be suppressed

Returns `TRUE` when the current rendering context should **not** emit
`\\newpage` between content blocks. This covers HTML-based formats
(where page breaks are meaningless), Typst (where `\\newpage` is
converted to `#pagebreak()` which errors inside containers), and officer
contexts (where page breaks are handled differently).

## Usage

``` r
is_html_output_or_officer()
```

## Value

Logical scalar.

## Details

Uses
[`knitr::is_html_output()`](https://rdrr.io/pkg/knitr/man/output_type.html)
when knitr is in progress (covers revealjs, slidy, and other HTML-based
Pandoc formats). Also returns `TRUE` for Typst, since `#pagebreak()`
fails inside containers and the Quarto `pagebreak` shortcode lacks
native Typst support. Outside of knitr (officer context), returns `TRUE`
so that `\\newpage` is not emitted by default.
