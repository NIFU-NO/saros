#' Check Quarto Website Folders for Missing index.qmd
#'
#' @description
#' Scans a Quarto website project directory for folders that contain `.qmd`
#' files (directly or in subfolders) but are missing an `index.qmd` file.
#' Such folders often cause a malfunctioning navigation menu in the rendered
#' Quarto website.
#'
#' Folders whose names start with `_` or `.` are excluded, as these are
#' typically Quarto internal or hidden directories.
#'
#' @param path *Path to project root*
#'
#'   `scalar<character>` // default: `"."` (`optional`)
#'
#'   The root directory of the Quarto website project to check.
#'
#' @param quiet *Suppress warnings*
#'
#'   `scalar<logical>` // default: `FALSE` (`optional`)
#'
#'   If `TRUE`, no cli warnings are issued. The affected paths are still
#'   returned invisibly.
#'
#' @return A character vector of folder paths (relative to `path`) that
#'   contain `.qmd` files but lack an `index.qmd`. Returned invisibly.
#'
#' @export
#' @examples
#' \dontrun{
#' # Check the current project
#' check_quarto_website_index()
#'
#' # Check a specific directory
#' check_quarto_website_index("path/to/quarto-project")
#' }
check_quarto_website_index <- function(path = ".", quiet = FALSE) {
  path <- normalizePath(path, mustWork = TRUE)

  # Single scan: collect all .qmd files once
  all_qmd <- list.files(
    path,
    pattern = "\\.qmd$",
    recursive = TRUE,
    full.names = FALSE,
    ignore.case = TRUE
  )

  # Normalize separators for consistent matching
  all_qmd <- gsub("\\\\", "/", all_qmd)

  # Exclude .qmd files inside _/. prefixed directories
  excluded <- grepl("(^|/)[\\_\\.]", all_qmd)
  all_qmd <- all_qmd[!excluded]

  if (length(all_qmd) == 0L) {
    return(invisible(character(0L)))
  }

  # Extract the directory component of each .qmd file (relative to path)
  qmd_dirs <- unique(dirname(all_qmd))

  # Build set of all ancestor directories that contain .qmd files (at any depth)
  all_ancestors <- unique(unlist(lapply(qmd_dirs, function(d) {
    parts <- strsplit(d, "/", fixed = TRUE)[[1L]]
    if (length(parts) == 1L && parts[1L] == ".") {
      return(".")
    }
    c(".", vapply(
      seq_along(parts),
      function(i) paste(parts[1:i], collapse = "/"),
      character(1L)
    ))
  })))

  missing <- character(0L)

  for (dir in all_ancestors) {
    index_rel <- if (dir == ".") "index.qmd" else paste0(dir, "/index.qmd")
    if (!file.exists(file.path(path, index_rel))) {
      missing <- c(missing, dir)
    }
  }

  if (length(missing) > 0L && !quiet) {
    cli::cli_warn(c(
      "Found {length(missing)} folder{?s} with {.file .qmd} files but no
      {.file index.qmd}:",
      stats::setNames(
        paste0("{.path ", missing, "}"),
        rep("x", length(missing))
      ),
      "i" = "Missing {.file index.qmd} files can cause broken navigation
      menus in Quarto websites."
    ))
  }

  invisible(missing)
}
