prepare_chunk <- function(element_name, ...) {
  class(element_name) <- element_name
  UseMethod("prepare_chunk", element_name)
}
