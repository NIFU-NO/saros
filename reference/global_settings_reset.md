# Reset Global Options for saros-functions

Reset Global Options for saros-functions

## Usage

``` r
global_settings_reset(fn_name = "makeme", quiet = FALSE)
```

## Arguments

- fn_name:

  String, one of `"make_link"`, `"fig_height_h_barchart"` and
  `"makeme"`.

- quiet:

  Flag. If `FALSE` (default), informs about what has been set.

## Value

Invisibly returned list of old and new values.

## Examples

``` r
global_settings_reset()
#> `options('saros')$makeme_defaults` has now been reset to package defaults.
```
