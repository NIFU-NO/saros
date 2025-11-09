# Embed Reactable Table (DEPRECATED!)

This function has been deprecated. Use instead
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)

## Usage

``` r
embed_cat_table(
  data,
  ...,
  dep = tidyselect::everything(),
  indep = NULL,
  mesos_group = NULL
)
```

## Arguments

- data:

  `data.frame`, `tibble` or potentially a `srvyr`-object.

- ...:

  Dynamic dots, arguments forwarded to underlying function(s).

- dep:

  `tidyselect`-syntax for dependent variable(s).

- indep:

  `tidyselect`-syntax for an optional independent variable.

- mesos_group:

  String
