# Extract plotting dimensions from ggplot theme

Extracts the base font size and legend position from a ggplot2 object's
complete theme (global theme + plot-level overrides) to improve
automatic height estimation.

## Usage

``` r
extract_ggplot_theme_info(plot_obj)
```

## Arguments

- plot_obj:

  A ggplot2 object

## Value

A list with components:

- base_size:

  Numeric. The base text size in points from the theme.

- legend_adds_height:

  Logical. TRUE when the legend is positioned at the bottom or top
  (adding vertical space), FALSE when at the sides, inside the panel, or
  hidden.
