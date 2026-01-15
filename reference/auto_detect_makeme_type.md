# Auto-Detect Appropriate Output Type Based on Variable Types

Internal helper function that determines the most appropriate output
type based on the classes of dependent and independent variables.

## Usage

``` r
auto_detect_makeme_type(data, dep, indep = NULL)
```

## Arguments

- data:

  Data frame to analyze

- dep:

  Character vector of dependent variable names

- indep:

  Character vector of independent variable names (can be NULL/empty)

## Value

Character string with the detected type:

- `"int_plot_html"` for numeric/integer dependent variables

- `"cat_plot_html"` for factor/ordered/character dependent variables
