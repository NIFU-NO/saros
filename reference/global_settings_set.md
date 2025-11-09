# Get Global Options for saros-functions

Get Global Options for saros-functions

## Usage

``` r
global_settings_set(
  new,
  fn_name = "makeme",
  quiet = FALSE,
  null_deletes = FALSE
)
```

## Arguments

- new:

  List of arguments (see `?make_link()`, `?makeme()`,
  [`fig_height_h_barchart()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart.md))

- fn_name:

  String, one of `"make_link"`, `"fig_height_h_barchart"` and
  `"makeme"`.

- quiet:

  Flag. If `FALSE` (default), informs about what has been set.

- null_deletes:

  Flag. If `FALSE` (default), `NULL` elements in `new` become `NULL`
  elements in the option. Otherwise, the corresponding element, if
  present, is deleted from the option.

## Value

Invisibly returned list of old and new values.

## Examples

``` r
global_settings_set(new=list(digits=2))
#> `options('saros')$makeme_defaults` has now been set.
```
