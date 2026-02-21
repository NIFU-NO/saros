# Estimate figure height for a horizontal bar chart

Taking an object from
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md), this
function estimates the height of a figure for a horizontal bar chart.
Works with both ggplot2 and mschart objects.

## Usage

``` r
fig_height_h_barchart2(plot_obj, ...)

fig_height_h_barchart2.ggplot(
  plot_obj,
  main_font_size = 7,
  strip_angle = 0,
  freq = FALSE,
  x_axis_label_width = 20,
  strip_width = 20,
  legend_location = c("plot", "panel"),
  n_legend_lines = NULL,
  showNA = c("ifany", "never", "always"),
  legend_key_chars_equivalence = 5,
  multiplier_per_horizontal_line = NULL,
  multiplier_per_vertical_letter = 1,
  multiplier_per_facet = 1,
  multiplier_per_legend_line = 1,
  fixed_constant = 0,
  figure_width_in_cm = 14,
  margin_in_cm = 0,
  max = 12,
  min = 1,
  multiplier_hide_axis_single_var = 0.6
)

fig_height_h_barchart2.ms_chart(
  plot_obj,
  main_font_size = 7,
  strip_angle = 0,
  freq = FALSE,
  x_axis_label_width = 20,
  strip_width = 20,
  legend_location = c("plot", "panel"),
  n_legend_lines = NULL,
  showNA = c("ifany", "never", "always"),
  legend_key_chars_equivalence = 5,
  multiplier_per_horizontal_line = NULL,
  multiplier_per_vertical_letter = 1,
  multiplier_per_facet = 1,
  multiplier_per_legend_line = 1,
  fixed_constant = 0,
  figure_width_in_cm = 14,
  margin_in_cm = 0,
  max = 12,
  min = 1,
  multiplier_hide_axis_single_var = 0.6
)

fig_height_h_barchart2.default(plot_obj, ...)
```

## Arguments

- plot_obj:

  A plot object from
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) -
  either a `ggplot2` object or an `ms_chart` object

- ...:

  Additional parameters passed to the specific method
  (`fig_height_h_barchart2.ggplot` or `fig_height_h_barchart2.ms_chart`)

- main_font_size:

  Numeric. Font size for the main text.

- strip_angle:

  Numeric. Angle of the strip text.

- freq:

  Logical. If TRUE, frequency plot with categories next to each other.
  If FALSE (default), proportion plot with stacked categories.

- x_axis_label_width, strip_width:

  Numeric. Width allocated for x-axis labels and strip labels
  respectively.

- legend_location:

  Character. Location of the legend. "plot" (default) or "panel".

- n_legend_lines:

  Integer. Number of lines in the legend.

- showNA:

  String, one of "ifany", "always" or "never". Not yet in use.

- legend_key_chars_equivalence:

  Integer. Approximate number of characters the legend key equals.

- multiplier_per_horizontal_line:

  Numeric. Multiplier per horizontal line.

- multiplier_per_vertical_letter:

  Numeric. Multiplier per vertical letter.

- multiplier_per_facet:

  Numeric. Multiplier per facet height.

- multiplier_per_legend_line:

  Numeric. Multiplier per legend line.

- fixed_constant:

  Numeric. Fixed constant to be added to the height.

- figure_width_in_cm:

  Numeric. Width of the figure in centimeters.

- margin_in_cm:

  Numeric. Margin in centimeters.

- max:

  Numeric. Maximum height.

- min:

  Numeric. Minimum height.

- multiplier_hide_axis_single_var:

  Numeric. Multiplier to reduce panel height when hiding axis text for
  single variable (default 0.6).

## Value

Numeric value representing the estimated height of the figure.

## Examples

``` r
# With ggplot2 (cat_plot_html)
fig_height_h_barchart2(makeme(data = ex_survey, dep = b_1:b_2, indep = x1_sex))
#> [1] 2

# With mschart (cat_plot_docx)
if (FALSE) { # \dontrun{
fig_height_h_barchart2(
  makeme(data = ex_survey, dep = b_1:b_2, 
         type = "cat_plot_docx", docx_return_object = TRUE)
)
} # }
```
