#' Convert List of Tables to Quarto Tabset
#'
#' @description
#' Creates a Quarto tabset from a named list of data frames, rendering each
#' as a table in its own tab. Designed to be called within a
#' Quarto document code chunk with `results='asis'`.
#'
#' @param tbl_list A named list of data frames. Names become tab labels.
#' @param table_fn A function that converts a data frame to a printable
#'   table object. Defaults to [knitr::kable()]. Other options include
#'   [gt::gt()], `tinytable::tt`, etc. Can be set globally via
#'   `global_settings_set(new = list(table_fn = gt::gt), fn_name = "crowd_tables_as_tabset")`.
#' @param pagebreak Character. Controls page break insertion between tables:
#'   - `"auto"` (default): Insert page breaks for non-HTML formats only
#'   - `"always"`: Always insert page breaks between tables
#'   - `"never"`: Never insert page breaks
#'
#' @return Called for its side effects (printing tabset markdown and tables).
#'   Returns `NULL` invisibly.
#'
#' @details
#' This function outputs raw Quarto markdown (level-5 headings) interleaved
#' with printed tables. The enclosing chunk should use the Quarto
#' `tabset` panel layout and `results: asis`.
#'
#' @seealso [crowd_plots_as_tabset()] for the plot equivalent.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tbl_list <- list(
#'   "Group A" = head(mtcars),
#'   "Group B" = tail(mtcars)
#' )
#' crowd_tables_as_tabset(tbl_list)
#'
#' # Use gt::gt instead
#' crowd_tables_as_tabset(tbl_list, table_fn = gt::gt)
#' }
crowd_tables_as_tabset <- function(
  tbl_list,
  table_fn = knitr::kable,
  pagebreak = c("auto", "always", "never")
) {
  args <-
    check_options(
      call = match.call(),
      defaults_env = global_settings_get(fn_name = "crowd_tables_as_tabset"),
      default_values = formals(crowd_tables_as_tabset)
    )

  pagebreak <- match.arg(args$pagebreak, choices = c("auto", "always", "never"))
  insert_pagebreak <- switch(pagebreak,
    auto = output_format() != "html",
    always = TRUE,
    never = FALSE
  )
  table_names <- names(tbl_list)

  for (i in seq_along(table_names)) {
    if (insert_pagebreak && i > 1) {
      cat("\\newpage\n\n")
    }
    cat(sprintf("##### %s\n", table_names[i]))
    print(args$table_fn(tbl_list[[table_names[i]]]))
  }
}
