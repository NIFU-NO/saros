#' @param element_name String, see draft_report
#'
#' @param ... Dynamic dots passed onto the methods.
#'
#' @export
prepare_chunk <- function(element_name, ...) {
  class(element_name) <- element_name
  UseMethod("prepare_chunk", element_name)
}


# All prepare_chunk methods should

## INPUTS
### colour_palette
### plot_height
### obj_name

## DO (in as fewest function calls as possible)
### check inputs (or place this in prepare_chunk()?)
### create the object
### save object to disk with qs
### save object to disk as native
### return a string for the chunk

