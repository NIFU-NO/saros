#' Replace Stata Labels
#'
#' @param data
#' @param df_new_labels
#' @param var_name_col
#' @param var_label_col
#'
#' @return
#' @export
#'
#' @examples
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
