# Process Independent Variable Factor Levels

Reverses factor levels for independent variables, but only for unordered
factors. Preserves the natural ordering of ordered factors.

## Usage

``` r
process_indep_factor_levels(data, indep = NULL)
```

## Arguments

- data:

  Data frame containing the data

- indep:

  Character string naming the independent variable (or NULL)

## Value

Modified data frame with reversed factor levels for unordered factors
