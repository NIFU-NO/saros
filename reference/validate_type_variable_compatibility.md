# Validate Variable Type Compatibility with Requested Output Type

Checks that the dependent variable types are compatible with the
requested output type. For example, categorical types (`cat_plot_*`,
`cat_table_*`) require factor/ordered/character variables, not
numeric/integer.

## Usage

``` r
validate_type_variable_compatibility(type, dep_types, dep_names)
```

## Arguments

- type:

  Character string of the requested output type

- dep_types:

  Character vector of classes for dependent variables

- dep_names:

  Character vector of dependent variable names

## Value

NULL (function used for side effects - validation errors)
