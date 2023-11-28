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
#' @param extensions_path,images_path Path to where _extensions and _images folders can be found and copied to whereever needed
#'
#' @return NULL
#' @export
#'
render_full_reports <- function(
    files = NULL,
    path = getwd(),
    processable_path = file.path(path, "Reports"),
    site_path = file.path(path, "_site"),
    extensions_path = file.path(path, "_extensions"),
    images_path = file.path(path, "_images")) {

  if(rlang::is_character(files) && all(nchar(files)>0)) {
    processable_files <- files
  } else if(rlang::is_string(processable_path) && dir.exists(processable_path)) {
    processable_files <- list.files(path = processable_path,
                                    pattern = "^_[^z].+\\.qmd",
                                    recursive = TRUE, full.names = TRUE)
  }
  processable_files <-
    processable_files[stringi::stri_detect_regex(basename(processable_files), pattern = "^_[^z].+")]

  processable_files_folders <- dirname(processable_files)

  new_files_docx <- fs::path_ext_set(fs::path_ext_remove(processable_files), ext = ".docx")
  new_files_pdf <- fs::path_ext_set(fs::path_ext_remove(processable_files), ext = ".pdf")

  new_file_destinations_docx <-
    stringi::stri_replace_first_fixed(
      str = new_files_docx,
      pattern = path,
      replacement = site_path)

  new_file_destinations_pdf <-
    stringi::stri_replace_first_fixed(
      str = new_files_pdf,
      pattern = path,
      replacement = site_path)

  fs::dir_copy(path = extensions_path,
               new_path = processable_files_folders, overwrite = TRUE)
  fs::dir_copy(path = images_path,
               new_path = processable_files_folders, overwrite = TRUE)

  for(i in seq_along(processable_files)) {
    quarto::quarto_render(input = processable_files[i], output_format = "all")
  }
  fs::file_copy(path = new_files_pdf,
                new_path = new_file_destinations_pdf, overwrite = TRUE)
  fs::file_copy(path = new_files_docx,
                new_path = new_file_destinations_docx, overwrite = TRUE)
  unlink(fs::path(processable_files_folders, "_extensions"))
  unlink(fs::path(processable_files_folders, "_images"))
  unlink(new_files_docx)
  unlink(new_files_pdf)
}
