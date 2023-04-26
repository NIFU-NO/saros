utils::globalVariables(names = c("."))

.saros.env <- new.env(parent = emptyenv())
.saros.env$summary_data_sort1 <-
  c(".top", ".upper", ".mid_upper", ".lower", ".mid_lower", ".bottom")
.saros.env$summary_data_sort2 <-
  c(".variable_name", ".category",
    ".count", ".count_se",
    ".proportion", ".proportion_se",
    ".mean", ".mean_se", ".mean_base",
    ".variable_label",  ".data_label", ".comb_categories",
    ".sum_value")
.saros.env$data_label_opts <-
  c("proportion", "percentage", "percentage_bare", "count", "mean", "median")


.saros.env$defaults <-
  list(group_by = c("chapter", "label_prefix"),
       label_separator = " - ",
       name_separator = NULL,
       captions = "none",
       index_filename = "index.qmd",

       element_names =
         c("opening_text" = FALSE,
           "uni_opening_text" = FALSE,
           # "miss_plot_html",
           # "miss_table_html",
           # "miss_text_html",
           # "miss_plot_docx",
           # "miss_table_docx",
           # "miss_text_docx",

           "uni_int_text" = FALSE,
           "uni_cat_text" = TRUE,
           "uni_chr_text" = FALSE,
           "uni_int_plot_html" = FALSE,
           "uni_cat_plot_html" = TRUE,
           "uni_chr_plot_html" = FALSE,
           "uni_int_table_html" = FALSE,
           "uni_cat_table_html" = TRUE,
           "uni_chr_table_html" = FALSE,
           "uni_int_plot_docx" = FALSE,
           "uni_cat_plot_docx" = TRUE,
           "uni_chr_plot_docx" = FALSE,
           "uni_int_table_docx" = FALSE,
           "uni_cat_table_docx" = TRUE,
           "uni_chr_table_docx" = FALSE,
           "uni_int_plot_png" = FALSE,
           "uni_cat_plot_png" = TRUE,
           "uni_chr_plot_png" = FALSE,
           "uni_int_table_xlsx" = FALSE,
           "uni_cat_table_xlsx" = FALSE,
           "uni_chr_table_xlsx" = FALSE,
           "uni_cat_sigtest" = TRUE,
           "uni_int_sigtest" = TRUE,

           "bi_opening_text" = FALSE,
           "bi_catcat_text" = FALSE,
           "bi_intcat_text" = FALSE,
           "bi_catint_text" = FALSE,
           "bi_intint_text" = FALSE,

           "bi_catcat_plot_html" = TRUE,
           "bi_intcat_plot_html" = FALSE,
           "bi_catint_plot_html" = FALSE,
           "bi_intint_plot_html" = FALSE,
           "bi_catcat_table_html" = TRUE,
           "bi_intcat_table_html" = FALSE,
           "bi_catint_table_html" = FALSE,
           "bi_intint_table_html" = FALSE,
           "bi_catcat_plot_docx" = TRUE,
           "bi_intcat_plot_docx" = FALSE,
           "bi_catint_plot_docx" = FALSE,
           "bi_intint_plot_docx" = FALSE,
           "bi_catcat_table_docx" = TRUE,
           "bi_intcat_table_docx" = FALSE,
           "bi_catint_table_docx" = FALSE,
           "bi_intint_table_docx" = FALSE,
           "bi_catcat_sigtest" = TRUE,
           "bi_catint_sigtest" = FALSE,
           "bi_intcat_sigtest" = TRUE,
           "bi_intint_sigtest" = TRUE
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
              value_max_infix = " {?is/are} the {n_top_bottom} item{?s} where the most have responded ",
              value_max_suffix = "",
              value_min_prefix = "",
              value_min_infix = " {?is/are} the {n_top_bottom} item{?s} where the fewest have responded ",
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
              intro_by_suffix = ""

         ),
       element_args =
         list(
           return_raw = TRUE,
           showNA = "never",
           ignore_if_below = 0,
           label_font_size = 8,
           main_font_size = 8,
           font_family = "Calibri",
           vertical = FALSE,
           data_label = c("proportion", "percentage", "percentage_bare", "count", "mean", "median"),
           x_axis_label_width = 20,
           seed = 1,
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
           hide_test_if_n_below = 10,
           sort_by = NULL,
           descend = FALSE,
           reps = 1000,
           contents = c("intro", "not_used_category",
                        "mode_max",
                        "value_max", "value_min", "value_diff", # Diff not implemented
                        "mean_max", "mean_min", "mean_diff",
                        "median_max", "median_min", "median_diff", # Not implemented
                        "variance_max", "variance_min"), # Not implemented
           include_numbers = TRUE, # not implemented
           n_top_bottom = 1
         )
  )

