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
    ".sum_value",
    ".element_name")
.saros.env$data_label_opts <-
  c("proportion", "percentage", "percentage_bare",
    "count", "mean", "median")
.saros.env$refined_chapter_overview_columns <-
  c("chapter",
    paste0(c(".variable_role", ".variable_selection", ".variable_position",
    ".variable_name", ".variable_name_prefix", ".variable_name_suffix",
    ".variable_label_prefix", ".variable_label_suffix",
    ".variable_type", ".variable_group_id"), "_dep"),
    paste0(c(".variable_role", ".variable_selection", ".variable_position",
             ".variable_name", ".variable_name_prefix", ".variable_name_suffix",
             ".variable_label_prefix", ".variable_label_suffix",
             ".variable_type", ".variable_group_id"), "_indep"),
    ".element_name")
.saros.env$element_names_simplified <-
  c("cat_table", "cat_prop_plot", "cat_freq_plot", "chr_table", "checkbox_plot")
#' Get Valid Data Labels for Figures and Tables
#'
#' @return Character vector
#' @export
#'
get_data_label_opts <- function() {
  .saros.env$data_label_opts
}
