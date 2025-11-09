# Evaluate Variable Selection

Internal helper function that evaluates tidyselect expressions for
dependent and independent variables, returning their column positions in
the data frame.

## Usage

``` r
evaluate_variable_selection(data, dep, indep)
```

## Arguments

- data:

  A data frame containing the variables to be selected

- dep:

  Quosure or tidyselect expression for dependent variables

- indep:

  Quosure or tidyselect expression for independent variables

## Value

A list with two named elements:

- `dep_pos`: Named integer vector of column positions for dependent
  variables

- `indep_pos`: Named integer vector of column positions for independent
  variables
