# Embed Interactive Categorical Plot (DEPRECATED!)

This function has been deprecated. Use instead
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)

## Usage

``` r
embed_cat_prop_plot(
  data,
  ...,
  dep = tidyselect::everything(),
  indep = NULL,
  colour_palette = NULL,
  mesos_group = NULL,
  html_interactive = TRUE,
  inverse = FALSE
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

- colour_palette:

  Character vector. Avoid using this.

- mesos_group:

  String

- html_interactive:

  Flag, whether to include interactivity.

- inverse:

  Flag, whether to flip plot or table.
