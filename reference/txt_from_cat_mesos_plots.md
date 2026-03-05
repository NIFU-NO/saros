# Extract Text Summary from Categorical Mesos Plots

Generates text summaries comparing two groups from categorical mesos
plot data. The function identifies meaningful differences between groups
based on proportions of respondents selecting specific categories and
produces narrative text descriptions.

## Usage

``` r
txt_from_cat_mesos_plots(
  plots,
  min_prop_diff = 0.1,
  n_highest_categories = 1,
  flip_to_lowest_categories = FALSE,
  checked = NULL,
  not_checked = NULL,
  digits = 2,
  selected_categories_last_split = " or ",
  fallback_string = character(),
  reverse = FALSE,
  glue_str_pos =
    c(paste0("For {var}, the target group has a higher proportion of respondents ",
    "({group_1}) than all others ({group_2}) who answered {selected_categories}."),
    paste0("More respondents answered {selected_categories} for {var} in the ",
    "target group ({group_1}) than in other groups ({group_2})."),
    paste0("The statement {var} shows {selected_categories} responses are more ",
    "common in the target group ({group_1}) compared to others ({group_2}).")),
  glue_str_neg =
    c(paste0("For {var}, the target group has a lower proportion of respondents ",
    "({group_1}) than all others ({group_2}) who answered {selected_categories}."),
    paste0("Fewer respondents answered {selected_categories} for {var} in the ",
    "target group ({group_1}) than in other groups ({group_2})."),
    paste0("The statement {var} shows {selected_categories} responses are less ",
    "common in the target group ({group_1}) compared to others ({group_2})."))
)
```

## Arguments

- plots:

  A list of two plot objects (or data frames with plot data) to compare.
  Each must contain columns: `.variable_label`, `.category`,
  `.category_order`, `.proportion`.

- min_prop_diff:

  Numeric. Minimum proportion difference (default 0.10) required between
  groups to generate text. Differences below this threshold are ignored.

- n_highest_categories:

  Integer. Number of top categories to include in the comparison
  (default 1). Categories are selected based on `.category_order`. Only
  applied if the variable has more categories than this value.

- flip_to_lowest_categories:

  Logical. If TRUE, compare lowest categories instead of highest
  (default FALSE).

- checked, not_checked:

  Optional string. When the categories of a variable exactly match these
  two values, the comparison is always made on `checked` — mirroring the
  visual convention in the bar chart where the checked category is
  rendered in colour on the left. Defaults to `NULL`; when `NULL`, the
  function tries to auto-detect the values from
  `global_settings_get("girafe")$checked` / `$not_checked`; if those are
  also `NULL`, checkbox handling is disabled and normal order-based
  category selection applies.

- digits:

  Integer. Number of decimal places for rounding proportions (default
  2).

- selected_categories_last_split:

  Character. Separator for the last item when listing multiple
  categories (default " or ").

- fallback_string:

  Character. String to return when validation fails (default
  [`character()`](https://rdrr.io/r/base/character.html)).

- reverse:

  Logical. If TRUE, reverses the order of the output text summaries
  (default FALSE).

- glue_str_pos:

  Character vector. Templates for positive differences (group_1 \>
  group_2). Available placeholders: `{var}`, `{group_1}`, `{group_2}`,
  `{selected_categories}`.

- glue_str_neg:

  Character vector. Templates for negative differences (group_2 \>
  group_1). Same placeholders as `glue_str_pos`.

## Value

A character vector of text summaries, one per variable with meaningful
differences. Returns empty character vector if no plots provided or no
meaningful differences found.

## Details

The function compares proportions between two groups for each variable
in the plot data. One template is randomly selected from the provided
vectors for variety in output text.

**Checkbox (checked/not_checked) variables**: When `checked` and
`not_checked` are both strings, any variable whose categories exactly
match that pair is treated as a checkbox variable. For such variables
the comparison is always made on the `checked` category, regardless of
`flip_to_lowest_categories`. This mirrors the visual convention in the
bar chart where the checked category is rendered in colour on the left —
the semantically meaningful side — even though its `.category_order` may
not be the highest. If `checked`/`not_checked` are `NULL`, the function
tries to auto-detect them from `global_settings_get("girafe")$checked` /
`$not_checked`; if those are also `NULL`, checkbox handling is disabled.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample plot data
plot_data_1 <- data.frame(
  .variable_label = rep("Job satisfaction", 3),
  .category = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High")),
  .category_order = 1:3,
  .proportion = c(0.2, 0.3, 0.5)
)

plot_data_2 <- data.frame(
  .variable_label = rep("Job satisfaction", 3),
  .category = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High")),
  .category_order = 1:3,
  .proportion = c(0.3, 0.4, 0.3)
)

plots <- list(
  list(data = plot_data_1),
  list(data = plot_data_2)
)

# Generate text summaries
txt_from_cat_mesos_plots(plots, min_prop_diff = 0.10)

# Compare lowest categories instead
txt_from_cat_mesos_plots(
  plots,
  flip_to_lowest_categories = TRUE,
  min_prop_diff = 0.05
)
} # }
```
