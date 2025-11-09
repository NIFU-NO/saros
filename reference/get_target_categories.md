# Get target categories for positional sorting

Uses subset_vector to determine which categories to include based on
positional methods like .top, .bottom, .upper, .lower, etc.

## Usage

``` r
get_target_categories(data, method)
```

## Arguments

- data:

  Dataset with .category column

- method:

  Positional method (.top, .bottom, .upper, .lower, etc.)

## Value

Character vector of target category names
