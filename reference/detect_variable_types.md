# Detect Variable Types for Dependent and Independent Variables

Internal helper function that examines the class of variables in the
subset data to determine their types (factor, numeric, character, etc.).

## Usage

``` r
detect_variable_types(subset_data, dep_crwd, indep_crwd)
```

## Arguments

- subset_data:

  Data frame subset containing the relevant variables

- dep_crwd:

  Character vector of dependent variable names for current crowd

- indep_crwd:

  Character vector of independent variable names for current crowd

## Value

List with two elements:

- `dep`: Character vector of classes for dependent variables

- `indep`: Character vector of classes for independent variables (empty
  if none)
