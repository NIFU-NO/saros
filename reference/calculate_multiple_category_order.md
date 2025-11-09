# Calculate ordering based on multiple category values

Calculate ordering based on multiple category values

## Usage

``` r
calculate_multiple_category_order(data, category_values, descend = FALSE)
```

## Arguments

- data:

  Dataset with .category and .count columns

- category_values:

  Vector of category values to sum (e.g., c("A bit", "A lot"))

- descend:

  Logical indicating if sorting should be descending

## Value

Numeric vector of ordering values
