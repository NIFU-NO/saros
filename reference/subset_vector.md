# Given Ordered Integer Vector, Return Requested Set.

Useful for identifying which categories are to be collected.

## Usage

``` r
subset_vector(
  vec,
  set = c(".top", ".upper", ".mid_upper", ".lower", ".mid_lower", ".bottom", ".spread"),
  spread_n = NULL,
  sort = FALSE
)
```

## Arguments

- vec:

  A vector of any type.

- set:

  A character string, one of c(".top", ".upper", ".mid_upper", ".lower",
  ".mid_lower", ".bottom")

- spread_n:

  The number of values to extract when set is "spread".

- sort:

  Whether to sort the output, defaults to FALSE.

## Value

Selected set of vector.
