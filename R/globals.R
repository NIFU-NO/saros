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
.saros.env$element_type <-
  c("opening_text",
    "uni_opening_text",
    # "miss_plot_html",
    # "miss_table_html",
    # "miss_text_html",
    # "miss_plot_docx",
    # "miss_table_docx",
    # "miss_text_docx",

    "uni_int_plot_html",
    "uni_cat_plot_html",
    "uni_chr_plot_html",
    "uni_int_table_html",
    "uni_cat_table_html",
    "uni_chr_table_html",
    "uni_int_plot_docx",
    "uni_cat_plot_docx",
    "uni_chr_plot_docx",
    "uni_int_table_docx",
    "uni_cat_table_docx",
    "uni_chr_table_docx",
    "uni_int_text",
    "uni_cat_text",
    "uni_chr_text",

    "bi_opening_text",
    "bi_sig_text_html",

    "bi_catcat_plot_html",
    "bi_intcat_plot_html",
    "bi_catint_plot_html",
    "bi_intint_plot_html",
    "bi_catcat_table_html",
    "bi_intcat_table_html",
    "bi_catint_table_html",
    "bi_intint_table_html",
    "bi_catcat_plot_docx",
    "bi_intcat_plot_docx",
    "bi_catint_plot_docx",
    "bi_intint_plot_docx",
    "bi_catcat_table_docx",
    "bi_intcat_table_docx",
    "bi_catint_table_docx",
    "bi_intint_table_docx",
    "bi_catcat_text",
    "bi_intcat_text",
    "bi_catint_text",
    "bi_intint_text"
  )

#' List All Valid Names of The Elements Argument
#'
#' @export
#'
list_available_element_types <-
  function() {
  .saros.env$element_type
}

