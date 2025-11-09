# Generate Appropriate Data Summary Based on Variable Types

Internal helper function that routes to the appropriate data
summarization function based on the detected variable types (categorical
vs continuous).

## Usage

``` r
generate_data_summary(
  variable_types,
  subset_data,
  dep_crwd,
  indep_crwd,
  args,
  ...
)
```

## Arguments

- variable_types:

  List with dep and indep variable type information

- subset_data:

  Data frame subset for the current crowd

- dep_crwd:

  Character vector of dependent variable names for current crowd

- indep_crwd:

  Character vector of independent variable names for current crowd

- args:

  List of makeme function arguments

- ...:

  Additional arguments passed to summarization functions

## Value

Data summary object (type depends on variable types):

- For integer/numeric dep + factor/character indep: calls
  summarize_int_cat_data()

- For factor/character dep: calls summarize_cat_cat_data()

- For mixed types: throws error
