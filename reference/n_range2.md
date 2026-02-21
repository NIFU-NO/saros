# Provides a range (or single value) for N in a plot object from `makeme()`

Takes a plot object from
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) and
returns the sample size (N) range as a formatted string. Works with both
`ggplot2` objects and `mschart` objects.

## Usage

``` r
n_range2(plot_obj, ...)

n_range2.ggplot(
  plot_obj,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
)

n_range2.ms_chart(
  plot_obj,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
)

n_range2.default(plot_obj, ...)
```

## Arguments

- plot_obj:

  A plot object from
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) -
  either a `ggplot2` object or an `ms_chart` object

- ...:

  Additional parameters passed to the specific method

- glue_template_1, glue_template_2:

  String, for the case of a single value (1) or a range with
  minimum-maximum of values (2).

## Value

String.

## Examples

``` r
# With ggplot2 (cat_plot_html)
n_range2(makeme(data = ex_survey, dep = b_1:b_3))
#> 300

# With mschart (cat_plot_docx)
if (FALSE) { # \dontrun{
n_range2(
  makeme(data = ex_survey, dep = b_1:b_3,
         type = "cat_plot_docx", docx_return_object = TRUE)
)
} # }
```
