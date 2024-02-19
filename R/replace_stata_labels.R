#' Replace Stata Labels
#'
#' If no replacement exists, keeps the original.
#'
#' @param data data frame for which labels will be replaced
#' @param df_new_labels a data frame with a var_name_col and a var_label_col
#' @param var_name_col,var_label_col variable names (as strings) for the lookup of variable names in data, and the corresponding variable label.
#' @param trim string, trim "both" sides (default), "left" or "right".
#' @param overwrite Flag. Whether to remove label if it already exists.
#'
#' @return data, with variable labels replaced
#' @export
replace_stata_labels <- function(data, df_new_labels,
                                 var_name_col = "name",
                                 var_label_col = "vallab_full",
                                 trim = c("both", "all"),
                                 overwrite = TRUE) {

  for(var in colnames(data)) {
    original_label <- attr(data[[var]], "label")
    replacement_label <- df_new_labels[df_new_labels[[var_name_col]] == var, var_label_col]
    if(is_string(replacement_label) && (isTRUE(overwrite) || !is_string(original_label))) {
      attr(data[[var]], "label") <- replacement_label
    }
    if(any(trim %in% c("left", "right", "both"))) attr(data[[var]], "label") <- stringi::stri_trim(attr(data[[var]], "label"), side = trim[trim != "all"])
    if(any(trim %in% "all")) attr(data[[var]], "label") <- stringi::stri_replace_all_regex(attr(data[[var]], "label"), pattern = "[[:space:]]{2,}", replacement = " ")
  }
  data
}
