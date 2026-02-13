# Extract Fill Levels from ggplot Object

Evaluates the fill aesthetic mapping in the context of the plot data to
extract fill levels. Handles both simple column references (e.g.,
`fill = cyl`) and expressions (e.g., `fill = factor(cyl)`).

## Usage

``` r
get_fill_levels(ggobj)
```

## Arguments

- ggobj:

  A ggplot2 object

## Value

Character vector of fill levels, or NULL if no fill mapping exists
