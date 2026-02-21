# Get Global Options for saros-functions

Get Global Options for saros-functions

## Usage

``` r
global_settings_get(fn_name = "makeme")
```

## Arguments

- fn_name:

  String, one of `"make_link"`, `"fig_height_h_barchart"` and
  `"makeme"`.

## Value

List with options in R

## Examples

``` r
global_settings_get()
#> $type
#> [1] "auto"
#> 
#> $require_common_categories
#> [1] TRUE
#> 
#> $crowd
#> [1] "all"
#> 
#> $mesos_var
#> NULL
#> 
#> $mesos_group
#> NULL
#> 
#> $simplify_output
#> [1] TRUE
#> 
#> $hide_for_crowd_if_all_na
#> [1] TRUE
#> 
#> $hide_for_crowd_if_valid_n_below
#> [1] 0
#> 
#> $hide_for_crowd_if_category_k_below
#> [1] 2
#> 
#> $hide_for_crowd_if_category_n_below
#> [1] 0
#> 
#> $hide_for_crowd_if_cell_n_below
#> [1] 0
#> 
#> $hide_for_all_crowds_if_hidden_for_crowd
#> NULL
#> 
#> $hide_indep_cat_for_all_crowds_if_hidden_for_crowd
#> [1] FALSE
#> 
#> $add_n_to_dep_label
#> [1] FALSE
#> 
#> $add_n_to_indep_label
#> [1] FALSE
#> 
#> $add_n_to_label
#> [1] FALSE
#> 
#> $add_n_to_category
#> [1] FALSE
#> 
#> $totals
#> [1] FALSE
#> 
#> $categories_treated_as_na
#> NULL
#> 
#> $label_separator
#> [1] " - "
#> 
#> $error_on_duplicates
#> [1] TRUE
#> 
#> $showNA
#> [1] "ifany"
#> 
#> $data_label
#> [1] "percentage_bare"
#> 
#> $data_label_position
#> [1] "center" "bottom" "top"    "above" 
#> 
#> $html_interactive
#> [1] TRUE
#> 
#> $hide_axis_text_if_single_variable
#> [1] TRUE
#> 
#> $hide_label_if_prop_below
#> [1] 0.01
#> 
#> $inverse
#> [1] FALSE
#> 
#> $vertical
#> [1] FALSE
#> 
#> $digits
#> [1] 0
#> 
#> $data_label_decimal_symbol
#> [1] "."
#> 
#> $x_axis_label_width
#> [1] 25
#> 
#> $strip_width
#> [1] 25
#> 
#> $sort_dep_by
#> [1] ".variable_position"
#> 
#> $sort_indep_by
#> [1] ".factor_order"
#> 
#> $sort_by
#> NULL
#> 
#> $descend
#> [1] TRUE
#> 
#> $descend_indep
#> [1] FALSE
#> 
#> $labels_always_at_top
#> NULL
#> 
#> $labels_always_at_bottom
#> NULL
#> 
#> $table_wide
#> [1] TRUE
#> 
#> $table_main_question_as_header
#> [1] FALSE
#> 
#> $n_categories_limit
#> [1] 12
#> 
#> $translations
#> $translations$last_sep
#> [1] " and "
#> 
#> $translations$table_heading_N
#> [1] "Total (N)"
#> 
#> $translations$table_heading_data_label
#> [1] "%"
#> 
#> $translations$add_n_to_dep_label_prefix
#> [1] " (N = "
#> 
#> $translations$add_n_to_dep_label_suffix
#> [1] ")"
#> 
#> $translations$add_n_to_indep_label_prefix
#> [1] " (N = "
#> 
#> $translations$add_n_to_indep_label_suffix
#> [1] ")"
#> 
#> $translations$add_n_to_label_prefix
#> [1] " (N = "
#> 
#> $translations$add_n_to_label_suffix
#> [1] ")"
#> 
#> $translations$add_n_to_category_prefix
#> [1] " (N = ["
#> 
#> $translations$add_n_to_category_infix
#> [1] ","
#> 
#> $translations$add_n_to_category_suffix
#> [1] "])"
#> 
#> $translations$by_total
#> [1] "Everyone"
#> 
#> $translations$sigtest_variable_header_1
#> [1] "Var 1"
#> 
#> $translations$sigtest_variable_header_2
#> [1] "Var 2"
#> 
#> $translations$crowd_all
#> [1] "All"
#> 
#> $translations$crowd_target
#> [1] "Target"
#> 
#> $translations$crowd_others
#> [1] "Others"
#> 
#> 
#> $plot_height
#> [1] 15
#> 
#> $colour_palette
#> NULL
#> 
#> $colour_2nd_binary_cat
#> [1] "#ffffff"
#> 
#> $colour_na
#> [1] "grey"
#> 
#> $label_font_size
#> [1] 9
#> 
#> $main_font_size
#> [1] 9
#> 
#> $strip_font_size
#> [1] 6
#> 
#> $legend_font_size
#> [1] 6
#> 
#> $font_family
#> [1] "sans"
#> 
#> $path
#> NULL
#> 
#> $docx_template
#> NULL
#> 
#> $docx_return_object
#> [1] FALSE
#> 
```
