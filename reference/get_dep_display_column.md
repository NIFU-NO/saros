# Determine display column for dependent variables in int_plot_html

Checks if the number of dep variables matches the number of labels to
determine whether to use .variable_label or .variable_name for display.

## Usage

``` r
get_dep_display_column(dep_count, dep_labels)
```

## Arguments

- dep_count:

  Number of dependent variables

- dep_labels:

  Vector of dependency labels

## Value

Character string indicating which column to use
