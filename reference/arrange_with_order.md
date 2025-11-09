# Apply sorting with optional descending order

Unified helper to consistently handle ascending/descending sort order
across all sorting functions.

## Usage

``` r
arrange_with_order(data, order_col, descend = FALSE)
```

## Arguments

- data:

  Dataset to arrange

- order_col:

  Symbol/name of the column to sort by

- descend:

  Whether to sort in descending order

## Value

Arranged dataset
