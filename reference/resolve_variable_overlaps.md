# Resolve Variable Overlaps Between Dependent and Independent Variables

Internal helper function that handles cases where variables are selected
for both dependent and independent roles. Automatically removes
overlapping variables from the dependent list and provides user
feedback.

## Usage

``` r
resolve_variable_overlaps(dep, indep)
```

## Arguments

- dep:

  Character vector of dependent variable names

- indep:

  Character vector of independent variable names

## Value

Character vector of dependent variable names with overlaps removed

## Details

If overlapping variables are found:

- Informs user about the overlap via cli::cli_inform()

- Removes overlapping variables from dep vector

- Throws error if no dependent variables remain after removal
