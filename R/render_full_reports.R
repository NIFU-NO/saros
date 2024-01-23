#' Post-render rendering of complete reports
#'
#' If one wishes to render complete reports that are linked to
#'   in a website, but not listed among the chapters on the sidebar menu,
#'   one can make these with filenames starting with underscores (_) which
#'   will make them not listed in said menu. However, this will also mean they
#'   are not rendered in a Quarto (Website) project. This function, which can
#'   be called within a post-script (see example below), will render only these
#'   after the ordinary rendering of the project, and copied to the _site folder.
#'
#' @param files Optional character vector of report files (qmd). Can be obtained within a project by `Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")`
#' @param path If no files are given, a path to the root folder of the local "site".
#' @param processable_path Path to where report files can be (recursively) found.
#' @param site_path Path to _site
#' @param resource_paths Paths to where _extensions and _images folders can be found and copied to wherever needed
#' @param warn_on_file_error If TRUE, will collect warnings if a file fails to render or be copied. If FALSE (default), will stop the rendering process.
#' @param ... Additional arguments passed to `quarto::render()`
#' @return Returns invisibly a character vector of processed files.
#' @export
#'
render_full_reports <- function(
    files = NULL,
    path,
    processable_path = file.path(path, "Reports"),
    site_path = file.path(path, "_site"),
    resource_paths = file.path(path, c("_extensions", "_images")),
    warn_on_file_error = FALSE,
    ...) {
  dots <- rlang::list2(...)

  if(rlang::is_character(files) && all(nchar(files)>0)) {
    processable_files <- files
  } else if(rlang::is_string(processable_path) && dir.exists(processable_path)) {
    processable_files <- list.files(path = processable_path,
                                    pattern = "^_[^z].+\\.qmd",
                                    recursive = TRUE, full.names = TRUE)
  } else cli::cli_abort("Either {.arg files} or {.arg processable_path} must be specified and exist.")
  processable_files <-
    processable_files[stringi::stri_detect_regex(basename(processable_files), pattern = "^_[^z].+")]

  processable_files_folders <- dirname(processable_files)

  new_files_docx <- fs::path_ext_set(fs::path_ext_remove(processable_files), ext = ".docx")
  new_file_destinations_docx <-
    stringi::stri_replace_first_fixed(
      str = new_files_docx,
      pattern = path,
      replacement = site_path)

  new_files_pdf <- fs::path_ext_set(fs::path_ext_remove(processable_files), ext = ".pdf")
  new_file_destinations_pdf <-
    stringi::stri_replace_first_fixed(
      str = new_files_pdf,
      pattern = path,
      replacement = site_path)

  on.exit(add = TRUE, after = TRUE, expr ={
    for(res in resource_paths) {
      unlink(file.path(processable_files_folders, basename(res)), force = TRUE, recursive = TRUE)
    }
  })

  # for(res in resource_paths) {
  #   fs::dir_copy(path = rep(res, times=length(processable_files_folders)),
  #                new_path = file.path(processable_files_folders, basename(res)), overwrite = TRUE)
  # }

  processed_files <- rep(FALSE, length(processable_files))

  on.exit(add = TRUE, after = TRUE, expr ={
    if(any(!processed_files)) {
      cli::cli_warn("Failed to render these files: {processable_files[!processed_files]}")
    }
  })

  for(i in seq_along(processable_files)) {

    for(res in resource_paths) {
      fs::dir_copy(path = res,
                   new_path = file.path(processable_files_folders[i], basename(res)), overwrite = TRUE)
    }
    rlang::try_fetch(expr = {
      rlang::exec(quarto::quarto_render,
                  input = processable_files[i],
                  output_format = "all",
                  !!!dots)
    },
    error = function(cnd) {
      msg <- "Failed to render: {processable_files[i]}"
      if(warn_on_file_error) {
        cli::cli_warn(msg, parent = cnd)
      } else {
        cli::cli_abort(msg, parent = cnd)
      }
    })

    ## Should preferably identify what kind of outputs have been produced and copy whatever there is
    ##   (e.g. if only html, only copy html, if html and pdf, copy both, etc.)
    ##   For now, just copy both docx and pdf

    rlang::try_fetch(expr = {
      if(fs::file_exists(new_files_docx[i])) {
        fs::file_copy(path = new_files_docx[i],
                      new_path = new_file_destinations_docx[i],
                      overwrite = TRUE)
      }
    }, error = function(cnd) {
      msg <- "Failed to copy {new_files_docx[i]} to {new_file_destinations_docx[i]}."
      if(warn_on_file_error) {
        cli::cli_warn(msg, parent = cnd)
      } else {
        cli::cli_abort(msg, parent = cnd)
      }
    })
    rlang::try_fetch(expr = {
      if(fs::file_exists(new_files_pdf[i])) {
        fs::file_copy(path = new_files_pdf[i],
                      new_path = new_file_destinations_pdf[i],
                      overwrite = TRUE)
      }
    }, error = function(cnd) {
      msg <- "Failed to copy {new_files_pdf[i]} to {new_file_destinations_pdf[i]}."
      if(warn_on_file_error) {
        cli::cli_warn(msg, parent = cnd)
      } else {
        cli::cli_abort(msg, parent = cnd)
      }
    })

    processed_files[i] <- TRUE

  }
  cli::cli_alert_success("Completed processing of full reports")
  invisible(processed_files)
}
