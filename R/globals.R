utils::globalVariables(names = c("."))

.saros.env <- new.env(parent = emptyenv())
.saros.env$summary_data_sort1 <-
  c(".top", ".upper", ".mid_upper", ".lower", ".mid_lower", ".bottom")
.saros.env$summary_data_sort2 <-
  c(".variable_name", ".category", ".count", ".proportion",
    ".variable_label",  ".data_label", ".comb_categories",
     ".sum_value", ".mean_base")
.saros.env$element_type <-
  c("opening_text",
    # "miss_plot",
    # "miss_table",
    # "miss_text",
    "uni_int_plot_html",
    "uni_cat_plot_html",
    "uni_chr_plot_html",
    "uni_int_plot_docx",
    "uni_cat_plot_docx",
    "uni_chr_plot_docx",
    "uni_int_table_html",
    "uni_cat_table_html",
    "uni_chr_table_html",
    "uni_int_table_docx",
    "uni_cat_table_docx",
    "uni_chr_table_docx",
    "uni_int_text",
    "uni_cat_text",
    "uni_chr_text",
    "bi_opening_text",
    "bi_sig_text",
    "bi_plot",
    "bi_table",
    "bi_text"
  )

#' List All Valid Names of The Elements Argument
#'
#' @export
#'
list_available_element_types <-
  function() {
  .saros.env$element_type
}

