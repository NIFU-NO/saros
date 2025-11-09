# Process data with standard table operations

Apply column selection, renaming, and independent variable handling

## Usage

``` r
process_table_data(
  data,
  col_basis,
  indep_vars = NULL,
  indep_label = character(),
  main_question = "",
  use_header = FALSE,
  stat_columns = NULL,
  column_mappings = NULL
)
```

## Arguments

- data:

  Data frame to process

- col_basis:

  Column basis for variables

- indep_vars:

  Independent variable columns

- indep_label:

  Independent variable labels

- main_question:

  Main question for headers

- use_header:

  Whether to use main question as header

- stat_columns:

  Statistical columns to include

- column_mappings:

  Additional column mappings

## Value

Processed data frame
