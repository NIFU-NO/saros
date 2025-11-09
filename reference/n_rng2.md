# Obtain range of N for a given `ggobj`.

Obtain range of N for a given `ggobj`.

## Usage

``` r
n_rng2(ggobj, glue_template_1 = "{n}", glue_template_2 = "[{n[1]}-{n[2]}]")
```

## Arguments

- ggobj:

  A `ggplot2`-object.

- glue_template_1, glue_template_2:

  String, for the case of a single value (1) or a range with
  minimum-maximum of values (2).

## Value

Always a string.
