

#' Generate Quarto Index File That Merges All Chapters
#'
#' This function creates an index Quarto file (QMD) that merges all chapters in the specified order. It can also include optional title and author(s) information.
#'
#' @inheritParams draft_report
#' @param index_filepath Path to the output index.qmd file. Defaults to "complete_report.qmd"
#' @param chapter_filepaths A character vector containing the filepaths of the chapter files to be included in the index, in the desired order.
#' @param report_filepath A string containing the filepath to the report.qmd.
#' @param yaml_file A string containing the filepath to a yaml-file to be inserted at top of qmd-file.
#' @param call Internal call argument. Not to be fiddled with by the user.
#'
#' @return A string containing the filepath to the generated Quarto index file.
#' @keywords internal
#' @examples
#' filepath <- tempfile(pattern = "index", fileext = ".qmd")
#' index_file <-
#'   saros:::gen_qmd_index(
#'                  authors = c("Marky Twain",
#'                              "J.K. Rawlings",
#'                              "Stephen Prince"),
#'                chapter_filepaths =
#'                  c("Ch1.qmd",
#'                  "Ch2.qmd",
#'                  "Ch3.qmd"),
#'                  index_filepath = filepath)
gen_qmd_index <-
  function(
    index_filepath = "index.qmd",
    ...,
    chapter_filepaths = NULL,
    report_filepath = NULL,
    yaml_file = NULL,
    call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...), allow_unique_overrides = FALSE)
    check_string(dots$title, n = 1, null.ok = TRUE, call = call)
    check_string(dots$authors, n = NULL, null.ok = TRUE, call = call)
    check_string(index_filepath, n = 1, null.ok = FALSE,  call = call)
    check_string(chapter_filepaths, n = NULL, null.ok = TRUE, call = call)
    check_string(report_filepath, n = 1, null.ok = TRUE, call = call)
    check_string(yaml_file, n = 1, null.ok = TRUE, call = call)

    yaml_section <-
      process_yaml(yaml_file = yaml_file,
                   format = if(rlang::is_character(chapter_filepaths)) "typst" else "html",
                   title = dots$title,
                   authors = dots$authors)

    if(rlang::is_character(chapter_filepaths)) {
      main_section <-
        unlist(lapply(chapter_filepaths,
                      FUN = function(.x) stringi::stri_c('{{< include "', .x, '" >}}', ignore_null=TRUE)))
      main_section <-  stringi::stri_c(main_section, ignore_null=TRUE, collapse = "\n\n")
    }
    report_link <- if(rlang::is_character(report_filepath)) {
      stringi::stri_c(dots$translations$download_report, "[(pdf)](_", dots$title, ".pdf)\n",
                      dots$translations$download_report, "[(docx)](_", dots$title, ".docx)")
    }

    qmd_start_section <- if(!rlang::is_null(dots$qmd_start_section_filepath)) stringi::stri_c(ignore_null=TRUE, readLines(con = dots$qmd_start_section_filepath), collapse="\n")
    qmd_end_section <- if(!rlang::is_null(dots$qmd_end_section_filepath)) stringi::stri_c(ignore_null=TRUE, readLines(con = dots$qmd_end_section_filepath), collapse="\n")

    out <-
      stringi::stri_c(yaml_section, "\n",
                   # qmd_start_section,
                   if(rlang::is_character(chapter_filepaths)) main_section,
                   if(rlang::is_character(report_filepath)) report_link,
                   qmd_end_section,
                   ignore_null=TRUE, sep="\n")
    cat(out, file = index_filepath, append = FALSE)
    index_filepath
  }

