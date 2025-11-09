# Summarize Data Based on Variable Types

Internal helper function that determines the appropriate data
summarization approach based on variable types and calls the
corresponding function.

## Usage

``` r
summarize_data_by_type(args, subset_data, dep_crwd, indep_crwd, ...)
```

## Arguments

- args:

  List of makeme function arguments

- subset_data:

  Data frame subset for the current crowd

- dep_crwd:

  Character vector of dependent variable names for current crowd

- indep_crwd:

  Character vector of independent variable names for current crowd

- ...:

  Additional arguments passed to summarization functions

## Value

Modified args list with data_summary element added:

- For integer/numeric variables: calls summarize_int_cat_data()

- For factor/ordered variables: calls summarize_cat_cat_data() with full
  argument set
