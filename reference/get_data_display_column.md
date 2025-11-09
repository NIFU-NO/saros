# Determine display column based on data availability

Checks if .variable_label column exists and has non-NA values to
determine whether to use .variable_label or .variable_name for display.

## Usage

``` r
get_data_display_column(data)
```

## Arguments

- data:

  Data frame containing variable information

## Value

Character string indicating which column to use
