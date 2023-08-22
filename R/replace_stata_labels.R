#' Replace Stata Labels
#'
#' @param data data frame for which labels will be replaced
#' @param df_new_labels a data frame with a var_name_col and a var_label_col
#' @param var_name_col,var_label_col variable names (as strings) for the lookup of variable names in data, and the corresponding variable label.
#' @param trim string, trim "both" sides (default), "left" or "right".
#'
#' @return data, with variable labels replaced
#' @export
replace_stata_labels <- function(data, df_new_labels,
                                 var_name_col = "name",
                                 var_label_col = "vallab_full",
                                 trim = "both") {
  new_labels <- get_raw_labels(data, return_as_list = TRUE)
  new_labels <-
    lapply(X = seq_along(new_labels), FUN = function(i) {
      .x <- new_labels[[i]]
      .y <- names(new_labels)[[i]]

      if(any(df_new_labels[[var_name_col]] == .y)) {
        x <- df_new_labels[df_new_labels[[var_name_col]] == .y, var_label_col]
        if(trim %in% c("left", "right", "both")) x <- stringi::stri_trim(x, side = trim)
        if(trim == "all") x <- stringi::stri_replace_all_regex(x, pattern = "[[:space:]]{2,}", replacement = " ")
        x
      }  else .y
    })
  for(i in seq_len(ncol(data))) {
    attr(data[[i]], "label") <- new_labels[[i]]
  }
  data
}
