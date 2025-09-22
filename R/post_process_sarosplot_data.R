post_process_makeme_data <- function(
  data,
  indep = NULL,
  showNA = "never",
  colour_2nd_binary_cat = NULL
) {
  if (length(indep) > 0) {
    # Only reverse unordered factors, preserve ordered factor levels
    if (!is.ordered(data[[indep]])) {
      data[[indep]] <- forcats::fct_rev(data[[indep]])
    }
  }

  if (
    dplyr::n_distinct(data$.category, na.rm = showNA == "never") == 2 &&
      !is.null(colour_2nd_binary_cat)
  ) {
    data$.category <- forcats::fct_rev(data$.category)
  }
  data
}
