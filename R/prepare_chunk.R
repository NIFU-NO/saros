#' Internal function to prepare a chunk for a Quarto report.
#'
#' @param element_name The name of the element to be prepared.
#' @param ... Dynamic dots passed onto the methods.
#'
#' @return Returns a text string for the chunk, but
#' also has side-effects for generating files.
#' @export
prepare_chunk <- function(element_name, ...) {
  UseMethod("prepare_chunk")
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

