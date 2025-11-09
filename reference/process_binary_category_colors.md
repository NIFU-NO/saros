# Process Binary Category Colors

Reverses the .category variable for binary categories when a special
color condition is met. This is specific to categorical plot
functionality.

## Usage

``` r
process_binary_category_colors(
  data,
  showNA = "never",
  colour_2nd_binary_cat = NULL
)
```

## Arguments

- data:

  Data frame containing the data with .category column

- showNA:

  Character indicating how to handle NA values

- colour_2nd_binary_cat:

  Color specification for second binary category

## Value

Modified data frame with potentially reversed .category levels
