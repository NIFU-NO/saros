# Validate single dependent variable requirement

Common validation pattern for functions that require exactly one
dependent variable.

## Usage

``` r
validate_single_dep_var(dep, function_name)
```

## Arguments

- dep:

  Vector of dependent variables

- function_name:

  Name of the function requiring validation (for error message)

## Value

Nothing if valid, throws error if invalid
