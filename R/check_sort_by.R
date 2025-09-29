check_sort_by <- function(x, sort_by = NULL, call = rlang::caller_env()) {
  set_options <- c(
    .saros.env$summary_data_sort1,
    .saros.env$summary_data_sort2
  )
  categories_in_data <- unique(as.character(x))

  if (is.null(sort_by)) {
    return()
  }
  if (
    is.character(sort_by) &&
      length(sort_by) == 1 &&
      sort_by %in% set_options
  ) {
    return()
  }
  # Error on empty character vector first
  if (is.character(sort_by) && length(sort_by) == 0) {
    cli::cli_abort(
      c(
        x = "Invalid {.arg sort_by}: empty character vector",
        i = "{.arg sort_by} cannot be an empty character vector."
      ),
      call = call
    )
  }
  if (
    is.character(sort_by) &&
      length(sort_by) > 0 &&
      all(sort_by %in% categories_in_data)
  ) {
    return()
  }
  cli::cli_abort(
    c(
      x = "Invalid {.arg sort_by}: {sort_by}",
      i = "{.arg sort_by} must be either NULL (no sorting), a single string from the set options {.var {set_options}} or all valid categories in the data ({.var {categories_in_data}})."
    ),
    call = call
  )
}
