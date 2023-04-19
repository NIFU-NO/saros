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
.saros.env$element_type <-
  c("opening_text" = FALSE,
    "uni_opening_text" = FALSE,
    # "miss_plot_html",
    # "miss_table_html",
    # "miss_text_html",
    # "miss_plot_docx",
    # "miss_table_docx",
    # "miss_text_docx",

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
    "uni_int_text" = FALSE,
    "uni_cat_text" = TRUE,
    "uni_chr_text" = FALSE,

    "bi_opening_text" = FALSE,
    "bi_sig_text_html" = FALSE,

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
    "bi_catcat_text" = FALSE,
    "bi_intcat_text" = FALSE,
    "bi_catint_text" = FALSE,
    "bi_intint_text" = FALSE
  )

#' List All Valid Names of The Elements Argument
#'
#' @param valid_only Only return implemented elements, or all (planned) elements.
#' Note that *_docx-elements have limited storage support and
#' are hence not included in examples in this package as minor (unimportant) warnings will occur.
#'
#' @export
#'
list_available_element_types <-
  function(valid_only = TRUE) {
  names(.saros.env$element_type[if(valid_only) unname(.saros.env$element_type) else TRUE])
}

