# Perform Type-Specific Validation Checks

Internal helper function that validates arguments based on the specific
output type requested. Different types have different constraints.

## Usage

``` r
validate_type_specific_constraints(args, data, indep, dep_pos)
```

## Arguments

- args:

  List of makeme function arguments

- data:

  Data frame being analyzed

- indep:

  Character vector of independent variable names

- dep_pos:

  Named integer vector of dependent variable positions

## Value

NULL (function used for side effects - validation errors)

## Details

Current type-specific validations:

- `chr_table_html`: Requires exactly one dependent variable
