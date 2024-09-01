utils::globalVariables(names = c(".", ".data", ".env"))


#' Get Valid Data Labels for Figures and Tables
#'
#' @return Character vector
#' @export
#'
get_data_label_opts <- function() {
  .saros.env$data_label_opts
}
