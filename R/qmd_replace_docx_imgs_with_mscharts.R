#' Replace Images in A Quarto-Generated docx-file with Mschart docx-files
#'
#' @param file String path to a docx-file which has images that will be replaced
#'
#' @return String, path to input file.
#' @export
#'
#' @examples
replace_docx_imgs_with_mscharts <- function(file) {
  if(fs::path_file(file)=="index") cli::cli_abort("Not to be used on index-files.")
  doc <- officer::read_docx(path = file)
  to_be_replaced <- stringi::stri_subset_regex(officer::docx_bookmarks(doc),
                                               pattern = "^fig-pdf")
  for(img in to_be_replaced) {
    doc <- officer::cursor_bookmark(doc, id = to_be_replaced[1])
    doc <- officer::body_remove(x = doc)
    chart_filename <- stringi::stri_replace_all_regex(to_be_replaced[1],
                                                    pattern = "^fig-pdf_|_[0-9]{3,3}$",
                                                    replacement = "")
    chart_filename <- stringi::stri_c(chart_filename, "\\.docx", ignore_null = TRUE)
    chart_filepath <- dir(path = fs::path_dir(file), pattern = chart_filepath,
                          full.names = TRUE, recursive = TRUE, ignore.case = TRUE,
                          include.dirs = FALSE, no.. = TRUE)
    if(length(chart_filepath) != 1) cli::cli_abort("Unable to find {.file {chart_filepath}}.")
    doc <- officer::body_add(x = doc,
                             value = officer::block_pour_docx(chart_filepath))
  }
  print(doc, target = file)
  file
}

#' Post-Render All docx-files Found in The Output Folder To Replace Images with Mscharts
#'
#' @param path String, path to _site output folder
#'
#' @return Nothing, side-effects only.
#' @export
post_render_docx_img_replacer <- function(path = Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")) {

  docx_files <- dir(path = path, pattern = "[^(index)]\\.docx", full.names = TRUE,
                    recursive = FALSE, include.dirs = FALSE, no.. = TRUE)
  if(length(docx_files)==0) {
    cli::cli_warn("Nothing found in {.arg path}: {.path {path}}.")
  }
  for(docx_file in docx_files) {
    tmp <- replace_docx_imgs_with_mscharts(file = docx_file)
    cli::cli_inform("Modified {.path {tmp}}")
  }
}
