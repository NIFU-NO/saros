
#' Get Global Options for saros-functions
#'
#' @inheritParams global_settings_set
#'
#' @return List with options in R
#' @export
#'
#' @examples global_settings_get()
global_settings_get <- function(fn_name = "makeme") {
  getOption("saros")[[paste0(fn_name, "_defaults")]]
}

#' Get Global Options for saros-functions
#'
#' @param fn_name String, one of `"make_link"`, `"fig_height_h_barchart"` and `"makeme"`.
#' @param new List of arguments (see `?make_link()`, `?makeme()`, `fig_height_h_barchart()`)
#' @param quiet Flag. If `FALSE` (default), informs about what has been set.
#' @param null_deletes Flag. If `FALSE` (default), `NULL` elements in `new`
#'    become `NULL` elements in the option. Otherwise, the corresponding element,
#'    if present, is deleted from the option.
#'
#' @return Invisibly returned list of old and new values.
#' @export
#'
#' @examples global_settings_set(new=list(digits=2))
global_settings_set <- function(new, fn_name = "makeme", quiet = FALSE, null_deletes = FALSE) {
  saros_options <- getOption("saros", list())
  current_options <- saros_options[[paste0(fn_name, "_defaults")]]
  updated_options <- utils::modifyList(current_options, new, keep.null = !null_deletes)
  saros_options[[paste0(fn_name, "_defaults")]] <- updated_options
  options(saros = saros_options)
  if(isFALSE(quiet)) {
    msg_part <- paste0("options('saros')$", fn_name, "_defaults")
    cli::cli_inform("{.arg {msg_part}} has now been set.")
  }
  invisible(list(old = current_options,
                 new = updated_options))
}

#' Reset Global Options for saros-functions
#'
#' @inheritParams global_settings_set
#' @return Invisibly returned list of old and new values.
#' @export
#'
#' @examples global_settings_reset()
global_settings_reset <- function(fn_name = "makeme") {
  saros_options <- getOption("saros", list())
  old <- saros_options[[paste0(fn_name, "_defaults")]]
  saros_options[[paste0(fn_name, "_defaults")]] <- .saros.env[[paste0(fn_name, "_defaults")]]
  options(saros = saros_options)
  msg_part <- paste0("options('saros')$", fn_name, "_defaults")
  cli::cli_inform("{.val {msg_part}} has now been reset to factory defaults.")
  invisible(list(old = old,
                 new = saros_options[[paste0(fn_name, "_defaults")]]))
}

