# Detect the Current Output Format

Returns the output format of the current rendering context. When called
inside a Quarto/knitr document, delegates to
[`knitr::pandoc_to()`](https://rdrr.io/pkg/knitr/man/output_type.html).
When called outside of Quarto (e.g. in an officer-based script), returns
`"officer"`.

## Usage

``` r
output_format()
```

## Value

A character string: `"html"`, `"docx"`, `"typst"`, `"officer"`, or
another format reported by
[`knitr::pandoc_to()`](https://rdrr.io/pkg/knitr/man/output_type.html).

## Examples

``` r
if (FALSE) { # \dontrun{
output_format()
} # }
```
