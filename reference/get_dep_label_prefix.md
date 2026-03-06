# Retrieve the dep label prefix from a saros output object

Retrieves the `"dep_label_prefix"` attribute that saros attaches to
every object returned by
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) /
`make_content.*()`. This is the main question text — the shared label
prefix of all dependent variables used to produce the object.

## Usage

``` r
get_dep_label_prefix(obj)
```

## Arguments

- obj:

  Any object returned by
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) or a
  `make_content.*()` method (ggplot, data.frame, mschart, …).

## Value

A character scalar: the dep label prefix if present and non-empty,
otherwise `""`.

## Details

Storage location by class:

- **ggplot / gg** and **ms_barchart**: attribute is stored on `obj$data`
  (when `obj$data` is a data.frame) so that it survives further `+`
  operations. This function reads from `obj$data` first for both
  classes.

- **data.frame and other objects**: attribute stored directly on `obj`.

## Examples

``` r
p <- makeme(data = ex_survey, dep = b_1:b_3)
get_dep_label_prefix(p)
#> [1] "How much do you like living in"
```
