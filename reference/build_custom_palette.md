# Build Custom Palette Function

Creates a palette function that matches colors to factor levels based on
palette_codes and priority_palette_codes.

## Usage

``` r
build_custom_palette(palette_codes, fct_levels, priority_palette_codes = NULL)
```

## Arguments

- palette_codes:

  List of character vectors containing colours. Vectors can optionally
  be named for exact level matching.

- fct_levels:

  Character vector of factor levels to assign colors to.

- priority_palette_codes:

  Optional named character vector where names are categories and values
  are colours to use first. Defaults to `NULL`.

## Value

A palette function that takes n (number of colors) and lvls (levels) and
returns a named character vector of colors.
