# Reset Global Options for saros-functions

Reset Global Options for saros-functions

## Usage

``` r
global_settings_reset(fn_name = "makeme")
```

## Arguments

- fn_name:

  String, one of `"make_link"`, `"fig_height_h_barchart"` and
  `"makeme"`.

## Value

Invisibly returned list of old and new values.

## Examples

``` r
global_settings_reset()
#> "options('saros')$makeme_defaults" has now been reset to factory defaults.
```
