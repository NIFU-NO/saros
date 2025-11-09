# Provides a range (or single value) for N in a `ggplot2`-object from `makeme()`

Provides a range (or single value) for N in a `ggplot2`-object from
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)

## Usage

``` r
n_range2(ggobj, glue_template_1 = "{n}", glue_template_2 = "[{n[1]}-{n[2]}]")
```

## Arguments

- ggobj:

  A `ggplot2`-object.

- glue_template_1, glue_template_2:

  String, for the case of a single value (1) or a range with
  minimum-maximum of values (2).

## Value

String.

## Examples

``` r
n_range2(makeme(data = ex_survey, dep = b_1:b_3))
#> 300
```
