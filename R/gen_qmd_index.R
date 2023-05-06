

#' Generate Quarto Index File That Merges All Chapters
#'
#' This function creates an index Quarto file (QMD) that merges all chapters in the specified order. It can also include optional title and author(s) information.
#'
#' @param yaml_file A ymlthis-object. Defaults will be obtained from getOptions("ymlthis.default_options") if NULL.
#' @param authors A character vector listing the authors of the Quarto document. If not provided, the author information will be omitted.
#' @param index_filepath A string specifying the name of the Quarto index file to be created. Defaults to "complete_report.qmd".
#' @param chapter_filepaths A character vector containing the filepaths of the chapter files to be included in the index, in the desired order.
#' @param call Internal call argument. Not to be fiddled with by the user.
#'
#' @return A string containing the filepath to the generated Quarto index file.
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
#' if(interactive()) browseURL(filepath)
#' unlink(filepath)
gen_qmd_index <-
  function(
    yaml_file = NULL,
    authors = NULL,
    index_filepath = "complete_report.qmd",
    chapter_filepaths = NULL,
    call = rlang::caller_env()) {

    check_string(authors, n = NULL, null.ok = TRUE, call = call)
    check_string(index_filepath, null.ok = FALSE, n = 1, call = call)
    check_yml(yaml_file, call = call)

    if(rlang::is_null(yaml_file)) {
      yml_section <-
        ymlthis::yml(get_yml = TRUE,
                     author = FALSE,
                     date = FALSE)
    } else {
      yml_section <-  yaml_file
    }

    add_freeze <- "execute:\n  freeze: auto\n---\n\n"
    add_caption_settings <-
      "crossref:
  fig-title: Figur
  tbl-title: Tabell
  title-delim: ~"

    yml_section <-
      yml_section %>%
      ymlthis::yml_author(name = if(!is.null(authors)) authors else ymlthis::yml_empty()) %>%
      ymlthis::yml_toplevel(format = "html",
                            echo = FALSE,
                            editor = "visual",
                            `number-sections` = TRUE
                            ) %>%
      ymlthis::asis_yaml_output(fences = TRUE) %>%
      stringi::stri_replace_all(regex = "```|yaml|\\[\\]", replacement = "\n") %>%
      stringi::stri_replace_all(regex = "\\'FALSE\\'", replacement = "false") %>%
      stringi::stri_replace_all(regex = "\\'TRUE\\'", replacement = "true") %>%
      stringi::stri_replace_all(regex = "^\n\n\n", replacement = "") %>%
      as.character() #%>%
      # stringi::stri_replace_all(regex = "---\n\n", replacement = add_freeze) %>%


    main_section <-
      purrr::map_chr(chapter_filepaths,
                     .f = ~stringr::str_c('\n{{< include "', .x, '" >}}')) %>%
      stringr::str_c(collapse = "\n")

    end_section <-
      "\n\n### Referanser

::: {#refs}
:::"
    stringr::str_c(yml_section, main_section,
                   end_section) %>%
      cat(file = index_filepath, append = FALSE)
    index_filepath
  }

