# Validate and Initialize Arguments

Internal helper function that finalizes the arguments list by adding
resolved variable names and normalizing multi-value arguments.

## Usage

``` r
initialize_arguments(data, dep_pos, indep_pos, args)
```

## Arguments

- data:

  Data frame being analyzed

- dep_pos:

  Named integer vector of dependent variable positions

- indep_pos:

  Named integer vector of independent variable positions

- args:

  List of makeme function arguments

## Value

Modified args list with additional elements:

- `data`: The input data frame

- `dep`: Character vector of dependent variable names (from dep_pos)

- `indep`: Character vector of independent variable names (from
  indep_pos)

- Normalized single-value arguments (showNA, data_label, type)
