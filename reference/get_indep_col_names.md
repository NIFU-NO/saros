# Identify the independent variable column(s) of a summary

A summary carries no marker for its independent variable: it is whatever
column is not one of the standard summary columns. Two companion columns
have to be left out of that by-exclusion test, since each sits next to
the column it derives from and would otherwise pass for a second
independent variable: `.value`, the `int_plot_html` data column, and
`<indep>_original`, which `add_n_to_label()` adds to hold the labels
before their `(N = ...)` suffix (#620).

## Usage

``` r
get_indep_col_names(data)
```

## Arguments

- data:

  A summary, or the data of a plot built from one

## Value

Character vector of column names, empty when there is none
