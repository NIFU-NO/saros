# Obtain range of N for a given `ggobj`.

Internal workhorse function for calculating N range from plot data

## Usage

``` r
.n_rng2_impl(
  data,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
)
```

## Arguments

- data:

  A data frame from a plot object

- glue_template_1, glue_template_2:

  String, for the case of a single value (1) or a range with
  minimum-maximum of values (2).

## Value

Always a string.
