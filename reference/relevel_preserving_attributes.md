# Reorder a factor's levels without losing its other attributes

[`factor()`](https://rdrr.io/r/base/factor.html) returns a value
carrying only `levels` and `class`, so it silently drops the variable
`label` that
[`get_raw_labels()`](https://nifu-no.github.io/saros/reference/get_raw_labels.md)
and the plot titles read. Everything
[`factor()`](https://rdrr.io/r/base/factor.html) does not set itself is
copied back.

## Usage

``` r
relevel_preserving_attributes(x, levels)
```

## Arguments

- x:

  A factor or vector to reorder

- levels:

  Desired level order. Observed values it does not mention keep their
  place at the end rather than becoming `NA`.

## Value

`x` with its levels reordered and its attributes intact
