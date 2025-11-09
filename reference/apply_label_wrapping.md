# Apply label wrapping based on plot layout

Helper function to consistently wrap variable labels based on whether
they appear on facet strips or x-axis, and whether inverse layout is
used.

## Usage

``` r
apply_label_wrapping(
  data,
  indep_length,
  inverse,
  strip_width,
  x_axis_label_width
)
```

## Arguments

- data:

  Data frame containing .variable_label column

- indep_length:

  Number of independent variables (0 or 1)

- inverse:

  Logical, whether inverse layout is used

- strip_width:

  Width for facet strip labels

- x_axis_label_width:

  Width for x-axis labels

## Value

Data frame with wrapped .variable_label column
