#' Replace Stata Labels
#'
#' @param data data frame for which labels will be replaced
#' @param df_new_labels a data frame with a var_name_col and a var_label_col
#' @param var_name_col,var_label_col variable names (as strings) for the lookup of variable names in data, and the corresponding variable label.
#'
#' @return data, with variable labels replaced
#' @export
replace_stata_labels <- function(data, df_new_labels, var_name_col = "name", var_label_col = "vallab_full") {
  labelled::var_label(data) <-
    labelled::var_label(data) %>%
    purrr::map2(.y = names(.), .f = ~{
      if(any(df_new_labels[[var_name_col]] %in% .x)) {
        df_new_labels[df_new_labels[[var_name_col]] == .x, var_label_col]
      }  else .y
    })
  data
}
