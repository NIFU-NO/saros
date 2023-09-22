

#' Generate Quarto Index File That Merges All Chapters
#'
#' This function creates an index Quarto file (QMD) that merges all chapters in the specified order. It can also include optional title and author(s) information.
#'
#' @inheritParams draft_report
#' @param index_filepath Path to the output index.qmd file. Defaults to "complete_report.qmd"
#' @param chapter_filepaths A character vector containing the filepaths of the chapter files to be included in the index, in the desired order.
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
    call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...), allow_unique_overrides = FALSE)
    check_string(dots$authors, n = NULL, null.ok = TRUE, call = call)
    check_string(index_filepath, null.ok = FALSE, n = 1, call = call)

    yaml_section <-
      process_yaml(yaml_file = dots$index_yaml_file,
                   title = dots$title,
                   authors = dots$authors)


    main_section <-
      unlist(lapply(chapter_filepaths,
                     FUN = function(.x) stringi::stri_c(ignore_null=TRUE, '\n{{< include "', .x, '" >}}')))
    main_section <-  stringi::stri_c(ignore_null=TRUE, main_section, collapse = "\n")

    qmd_start_section <- if(!rlang::is_null(dots$qmd_start_section_filepath)) stringi::stri_c(ignore_null=TRUE, readLines(con = dots$qmd_start_section_filepath), collapse="\n")
    qmd_end_section <- if(!rlang::is_null(dots$qmd_end_section_filepath)) stringi::stri_c(ignore_null=TRUE, readLines(con = dots$qmd_end_section_filepath), collapse="\n")

    out <-
      stringi::stri_c(ignore_null=TRUE, yaml_section,
                   qmd_start_section,
                   main_section,
                   qmd_end_section)
    cat(out, file = index_filepath, append = FALSE)
    index_filepath
  }

