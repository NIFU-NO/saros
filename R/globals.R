utils::globalVariables(names = c(".", ".data", ".env"))

.saros.env <- new.env(parent = emptyenv())
.saros.env$summary_data_sort1 <-
  c(".top", ".upper", ".mid_upper", ".lower", ".mid_lower", ".bottom")
.saros.env$summary_data_sort2 <-
  c(".variable_name", ".category",
    ".count", ".count_se",
    ".proportion", ".proportion_se",
    ".mean", ".mean_se", #".mean_base",
    ".variable_label",  ".data_label", ".comb_categories",
    ".sum_value")
.saros.env$data_label_opts <-
  c("proportion", "percentage", "percentage_bare", "count", "mean", "median")


.saros.env$defaults <-
  list(report_yaml_file = NULL,
       chapter_yaml_file = NULL,
       label_separator = " - ",
       name_separator = NULL,
       index_filename = "index.qmd",
       qmd_start_section_filepath = NULL,
       qmd_end_section_filepath = NULL,
       element_names =
         c(#"opening_text",
           #"uni_opening_text",
           #"method",

           #"uni_int_text",
           "uni_cat_text",
           #"uni_chr_text",
           #"uni_int_plot_html",
           #"uni_int_plot_pdf",
           "uni_cat_prop_plot",
           "uni_cat_freq_plot",
           # "uni_chr_plot_html",
           # "uni_chr_plot_pdf",
           # "uni_int_table",
           "uni_cat_table",
           "uni_chr_table",
           # "uni_int_plot_pdf",
           "uni_sigtest",

           # "bi_opening_text",
           # "bi_catcat_text",
           # "bi_intcat_text",
           # "bi_catint_text",
           # "bi_intint_text",

           "bi_catcat_prop_plot",
           "bi_catcat_freq_plot",
           "bi_catcat_table",
           # "bi_intcat_plot",
           # "bi_intint_plot",
           # "bi_intcat_table",
           # "bi_intint_table",
           "bi_sigtest"
         ),
       element_args =
         list(
           groupby = c("chapter", "variable_name"),
           sort_by = ".upper",
           data_label = .saros.env$data_label_opts,
           always_show_bi_for_by = c(),
           categories_treated_as_na = c(),
           variables_always_at_top = c(),
           variables_always_at_bottom = c(),
           return_raw = TRUE,
           panel_tabset_tailored = FALSE,
           showNA = c("never", "always", "ifany"),
           totals = FALSE,
           hide_label_if_prop_below = .01,
           hide_bi_entry_if_sig_above = .05,
           hide_test_if_n_below = 10,
           hide_chr_for_others = TRUE,
           label_font_size = 8,
           main_font_size = 8,
           x_axis_label_width = 20,
           plot_height_multiplier = NA_real_,
           plot_height_fixed_constant = NA_real_,
           plot_height_max = 20,
           plot_height_min = 1.5,
           png_scale = 1.2,
           png_width = 14,
           png_height = 16,
           vertical_height = 12,
           max_width_obj = 90,
           max_width_file = 64,
           font_family = "sans",
           vertical = FALSE,
           tailored_first = TRUE,
           single_y_bivariate_elements = FALSE,
           descend = TRUE,
           colour_palette =
             # list(
             # `nifu_mixed` =
             # c(nifu_mixed_1 = "#C82D49",
             #   nifu_mixed_2 = "#363636",
             #   nifu_mixed_3 = "#2D8E9F",
             #   nifu_mixed_4 = "#E7E6E6",
             #   nifu_mixed_5 = "#EDE2D2")
             # `nifu_bluescale` =
             c(nifu_blue_1 =	"#90D4E0",
               nifu_blue_2 =	"#70C7D7",
               nifu_blue_3 =	"#50BBCE",
               nifu_blue_4 =	"#36AABF",
               nifu_blue_5 =	"#2D8E9F",
               nifu_blue_6 =	"#24727F",
               nifu_blue_7 =	"#1B555F"),
           # `nifu_redscale` =
           #   c(nifu_red_1 =	"#E8B0B7",
           #     nifu_red_2 =	"#DE919A",
           #     nifu_red_3 =	"#D5727D",
           #     nifu_red_4 =	"#C84957",
           #     nifu_red_5 =	"#BC3848",
           #     nifu_red_6 =	"#9D2F3C",
           #     nifu_red_7 =	"#7E2630")
           # )
           colour_na = "gray90",
           colour_2nd_binary_cat = "#ffffff",
           digits = 1,
           data_label_decimal_symbol = ",",
           docx_template = NULL,
           reps = 1000,
           information =
             c(".variable_label", #".variable_name",
               ".category",
               ".count", ".count_se",
               ".proportion", ".proportion_se",
               ".mean", ".mean_se", #".mean_base",
               ".data_label", ".comb_categories", ".sum_value"),
           contents = c("intro", "not_used_category",
                        "mode_max",
                        "value_max", "value_min", "value_diff", # Diff not implemented
                        "mean_max", "mean_min", "mean_diff",
                        "median_max", "median_min", "median_diff", # Not implemented
                        "variance_max", "variance_min"), # Not implemented
           include_numbers = TRUE, # not implemented
           require_common_categories = TRUE,
           n_top_bottom = 1
         ),

       translations =
         list(last_sep = " and ",
              intro_prefix = "We will now look at the questions asked regarding ",
              intro_suffix = "",
              mode_max_onfix = " on ",
              mode_max_prefix = "The most common responses were ",
              mode_max_suffix = "",
              not_used_prefix = "The following response categories were not used: ",
              not_used_suffix = "",
              value_max_prefix = "",
              value_max_infix = " {?is/are} the {dots$n_top_bottom} item{?s} where the most have responded ",
              value_max_suffix = "",
              value_min_prefix = "",
              value_min_infix = " {?is/are} the {dots$n_top_bottom} item{?s} where the fewest have responded ",
              value_min_suffix = "",
              mean_onfix = "M = ",
              mean_max_prefix = "They have highest mean on ",
              mean_max_suffix = "",
              mean_min_prefix = "They have lowest mean on ",
              mean_min_suffix = "",
              median_onfix = "Median = ",
              median_max_prefix = "They have highest median on ",
              median_max_suffix = "",
              median_min_prefix = "They have lowest median on ",
              median_min_suffix = "",


              intro_by_prefix = "We will now look at the questions asked regarding ",
              intro_by_infix = " broken down by ",
              intro_by_suffix = "",
              by_breakdown = " by ",
              n_equal_prefix = " (N = ",
              n_equal_suffix = ")",
              table_heading_N = "Total (N)",
              by_total = "Everyone",
              sigtest_prefix = "Significance testing",
              sigtest_suffix = "",
              tailored_group_prefix = " Group: ",
              tailored_group_suffix = "",
              tailored_label_all_others = "Others",
              empty_chunk_text = "Text"
         )
  )

