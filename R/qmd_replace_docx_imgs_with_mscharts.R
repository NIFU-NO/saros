#' Replace Images in A Quarto-Generated docx-file with Mschart docx-files
#'
#' @param main_file docx file where images are to be replaced with mscharts
#' @param chart_dir Directory path to where the mscharts as docx files can be found.
#' @param delete_mschart_files Flag. Whether to delete the mschart docx files after insertion. Defaults to FALSE.
#'
#' @return String, path to input file.
#' @export
#'
replace_docx_imgs_with_mscharts <- function(main_file,
                                            chart_dir = fs::path_ext_remove(main_file),
                                            delete_mschart_files = FALSE) {

  doc <- officer::read_docx(path = main_file)
  to_be_replaced <- stringi::stri_subset_regex(officer::docx_bookmarks(doc),
                                               pattern = "^fig-")
  for(img in to_be_replaced) {
    doc <- officer::cursor_bookmark(doc, id = img)
    doc <- officer::body_remove(x = doc)
    chart_filename <- stringi::stri_replace_all_regex(img,
                                                    pattern = "^fig-|_[0-9]{3,3}$",
                                                    replacement = "")
    chart_filename <- stringi::stri_c(chart_filename, "\\.docx", ignore_null = TRUE)
    chart_filepath <- dir(path = chart_dir,
                          pattern = chart_filepath,
                          full.names = TRUE, recursive = TRUE, ignore.case = TRUE,
                          include.dirs = FALSE, no.. = TRUE)
    if(length(chart_filepath) != 1) cli::cli_abort("Unable to find {.file {chart_filepath}}.")
    doc <- officer::body_add(x = doc,
                             value = officer::block_pour_docx(chart_filepath))
    if(delete_mschart_files) file.remove(chart_filepath)
  }
  print(doc, target = main_file)
  main_file
}

#' Post-Render All docx-files Found in The Output Folder To Replace Images with Mscharts
#'
#' @param site_dir,site_mesos_dir,chart_dir,chart_mesos_dir String. Paths to the site (and mesos subfolder) with the docx files where the images are to be replaced, and paths to where to find the docx files with mscharts to be replaced with can be found.
#' @param delete_mschart_files Flag. Whether to delete the mschart docx files after successful copying into the docx files. Defaults to FALSE.
#'
#' @return Nothing, side-effects only.
#' @export
post_render_docx_img_replacer <- function(site_dir = fs::path(Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR"), "reports", "report"),
                                          site_mesos_dir = fs::path(site_dir, "mesos"),
                                          chart_dir = fs::path(Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR"), "..", "reports", "report"),
                                          chart_mesos_dir = fs::path(chart_dir, "mesos"),
                                          delete_mschart_files = FALSE) {

  ### The path to where the main report chapter docx files are located
  ### Vector of the mesos groups, taken from the folders in report_path/mesos
  mesos_groups <- list.dirs(site_mesos_dir, full.names = FALSE, recursive = FALSE)
  ### Vector of absolute paths to the above-mentioned mesos folders
  site_mesos_dirs <- fs::path(site_mesos_dir, mesos_groups)

  ### First find path for where the folders to the charts will be found
  ### Assume that the relative paths within this folder resemble that of path
  chapters <- dir(site_dir, pattern = "\\.html", full.names = FALSE,
                  recursive = FALSE, include.dirs = FALSE, no.. = TRUE)
  chapters <- stringi::stri_replace(chapters, pattern = "\\.html", replacement = "")

  chart_paths <- fs::path(chart_paths, chapters)


  docx_files <- dir(path = site_dir, pattern = "[^(index)]\\.docx", full.names = TRUE,
                    recursive = FALSE, include.dirs = FALSE, no.. = TRUE)
  if(length(docx_files)==0) {
    cli::cli_warn("Nothing found in {.arg site_dir}: {.path {site_dir}}.")
  }
  for(docx_file in docx_files) {
    tmp <- replace_docx_imgs_with_mscharts(main_file = docx_file,
                                           chart_dir = chart_dir,
                                           delete_mschart_files = delete_mschart_files)
    cli::cli_inform("Modified {.path {tmp}}")
  }
}
