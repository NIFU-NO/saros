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
  labelled::var_label(data) <-
    labelled::var_label(data) %>%
    purrr::map2(.y = names(.), .f = ~{
      if(any(df_new_labels[[var_name_col]] == .y)) {
        x <- df_new_labels[df_new_labels[[var_name_col]] == .y, var_label_col]
        if(trim %in% c("left", "right", "both")) x <- stringr::str_trim(x, side = trim)
        if(trim == "all") x <- stringr::str_replace_all(x, pattern = "[[:space:]]{2,}", " ")
        x
      }  else .y
    })
  data
}
