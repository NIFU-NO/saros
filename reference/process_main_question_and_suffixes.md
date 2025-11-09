# Process main question and extract suffixes

Handle label separation and suffix extraction for table functions

## Usage

``` r
process_main_question_and_suffixes(data, dots, col_basis)
```

## Arguments

- data:

  Data frame to process

- dots:

  List from rlang::list2(...)

- col_basis:

  Current column basis (.variable_label or .variable_name)

## Value

List with processed data, main_question, and updated col_basis
