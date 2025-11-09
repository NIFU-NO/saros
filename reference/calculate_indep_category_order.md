# Calculate independent variable ordering based on a specific category value

Calculate independent variable ordering based on a specific category
value

## Usage

``` r
calculate_indep_category_order(
  data,
  category_value,
  indep_col,
  descend_indep = FALSE
)
```

## Arguments

- data:

  Dataset with independent variable columns

- category_value:

  The category value to sort by (e.g., "Not at all")

- indep_col:

  Name of the independent variable column

- descend_indep:

  Logical indicating if sorting should be descending

## Value

Numeric vector of ordering values
