# Estimate figure height for a horizontal bar chart

This function estimates the height of a figure for a horizontal bar
chart based on several parameters including the number of dependent and
independent variables, number of categories, maximum characters in the
labels, and legend properties.

## Usage

``` r
fig_height_h_barchart(
  n_y,
  n_cats_y,
  max_chars_labels_y = 20,
  max_chars_cats_y = 20,
  n_x = NULL,
  n_cats_x = NULL,
  max_chars_labels_x = NULL,
  max_chars_cats_x = NULL,
  freq = FALSE,
  x_axis_label_width = 20,
  strip_width = 20,
  strip_angle = 0,
  main_font_size = 7,
  legend_location = c("plot", "panel"),
  n_legend_lines = NULL,
  legend_key_chars_equivalence = 5,
  multiplier_per_horizontal_line = 1,
  multiplier_per_vertical_letter = 1,
  multiplier_per_facet = 1,
  multiplier_per_bar = 1,
  multiplier_per_legend_line = 1,
  multiplier_per_plot = 1,
  fixed_constant = 0,
  margin_in_cm = 0,
  figure_width_in_cm = 14,
  max = 12,
  min = 2,
  hide_axis_text_if_single_variable = FALSE,
  add_n_to_dep_label = FALSE,
  add_n_to_indep_label = FALSE,
  showNA = c("ifany", "never", "always")
)
```

## Arguments

- n_y, n_x:

  Integer. Number of dependent/independent variables.

- n_cats_y:

  Integer. Number of categories across the dependent variables.

- max_chars_labels_y:

  Integer. Maximum number of characters across the dependent variables'
  labels.

- max_chars_cats_y:

  Integer. Maximum number of characters across the dependent variables'
  response categories (levels).

- n_cats_x:

  Integer or NULL. Number of categories across the independent
  variables.

- max_chars_labels_x:

  Integer or NULL. Maximum number of characters across the independent
  variables' labels.

- max_chars_cats_x:

  Integer or NULL. Maximum number of characters across the independent
  variables' response categories (levels).

- freq:

  Logical. If TRUE, frequency plot with categories next to each other.
  If FALSE (default), proportion plot with stacked categories.

- x_axis_label_width, strip_width:

  Numeric. Width allocated for x-axis labels and strip labels
  respectively.

- strip_angle:

  Integer. Angle of the strip text.

- main_font_size:

  Numeric. Font size for the main text.

- legend_location:

  Character. Location of the legend. "plot" (default) or "panel".

- n_legend_lines:

  Integer. Number of lines in the legend.

- legend_key_chars_equivalence:

  Integer. Approximate number of characters the legend key equals.

- multiplier_per_horizontal_line:

  Numeric. Multiplier per horizontal line.

- multiplier_per_vertical_letter:

  Numeric. Multiplier per vertical letter.

- multiplier_per_facet:

  Numeric. Multiplier per facet height.

- multiplier_per_bar:

  Numeric. Multiplier per bar height (thickness).

- multiplier_per_legend_line:

  Numeric. Multiplier per legend line.

- multiplier_per_plot:

  Numeric. Multiplier for entire plot estimates.

- fixed_constant:

  Numeric. Fixed constant to be added to the height.

- margin_in_cm:

  Numeric. Margin in centimeters.

- figure_width_in_cm:

  Numeric. Width of the figure in centimeters.

- max:

  Numeric. Maximum height.

- min:

  Numeric. Minimum height.

- hide_axis_text_if_single_variable:

  Boolean. Whether the label is hidden for single dependent variable
  plots.

- add_n_to_dep_label, add_n_to_indep_label:

  Boolean. If TRUE, will add 10 characters to the max label lengths.
  This is primarily useful when obtaining these settings from the global
  environment, avoiding the need to compute this for each figure chunk.

- showNA:

  String, one of "ifany", "always" or "never". Not yet in use.

## Value

Numeric value representing the estimated height of the figure.

## Examples

``` r
fig_height_h_barchart(
  n_y = 5,
  n_cats_y = 3,
  max_chars_labels_y = 20,
  max_chars_cats_y = 8,
  n_x = 1,
  n_cats_x = 4,
  max_chars_labels_x = 12,
  freq = FALSE,
  x_axis_label_width = 20,
  strip_angle = 0,
  main_font_size = 8,
  legend_location = "panel",
  n_legend_lines = 2,
  legend_key_chars_equivalence = 5,
  multiplier_per_horizontal_line = 1,
  multiplier_per_vertical_letter = .15,
  multiplier_per_facet = .95,
  multiplier_per_legend_line = 1.5,
  figure_width_in_cm = 16
)
#> [1] 2
```
