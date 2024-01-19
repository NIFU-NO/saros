#' @export
prepare_chunk.hline <- function(element_name, ...) {
  "-----"
}

#' @export
prepare_chunk.default <- function(element_name, ...) {
  cli::cli_warn("Invalid element_name: {element_name}. Check that you have loaded the required packages/methods.")
  stringi::stri_c("Invalid element_name", element_name, sep = ": ", ignore_null=TRUE)
}
