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

  all_dirs <- list.dirs(path, recursive = TRUE, full.names = TRUE)

  # Exclude directories whose basename starts with _ or .
  keep <- !grepl(
    pattern = "(^|[\\\\/])[\\_\\.]",
    x = substring(all_dirs, nchar(path) + 2L)
  )
  # Also exclude the root-level path itself from needing filtering by prefix,

  # but we do want to check the root for index.qmd
  dirs_to_check <- all_dirs[keep]

  missing <- character(0L)

  for (dir in dirs_to_check) {
    # Check if this folder or any of its subfolders contain .qmd files
    qmd_files <- list.files(
      dir,
      pattern = "\\.qmd$",
      recursive = TRUE,
      ignore.case = TRUE
    )
    if (length(qmd_files) == 0L) next

    # Check if this specific folder has an index.qmd
    has_index <- file.exists(file.path(dir, "index.qmd"))
    if (!has_index) {
      rel_path <- substring(dir, nchar(path) + 2L)
      if (nzchar(rel_path)) {
        missing <- c(missing, rel_path)
      } else {
        missing <- c(missing, ".")
      }
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
