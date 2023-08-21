process_yaml <- function(yaml_file = NULL,
                         title = NULL,
                         authors = NULL) {

  if(rlang::is_null(yaml_file)) {
    yml_section <-
      ymlthis::yml(get_yml = TRUE,
                   author = FALSE,
                   date = FALSE)
    if(!rlang::is_null(title)) yml_section <- ymlthis::yml_title(yml_section, title=title)
    yml_section <-
      ymlthis::yml_toplevel(yml_section,
                            format = "html",
                            echo = FALSE) %>%
      ymlthis::yml_author(name = if(!is.null(authors)) authors else ymlthis::yml_empty()) %>%
      ymlthis::asis_yaml_output(fences = TRUE)
  } else {
    yml_section <- stringr::str_c(readLines(yaml_file), collapse="\n")
    if(!rlang::is_null(title)) {
      if(stringi::stri_detect(str = yml_section, fixed="title: ")) {
        yml_section <- stringr::str_replace(yml_section, pattern = "title:(.+)\n",
                                            replacement = stringr::str_c("title:\\1 - ", title, "\n"))
      } else yml_section <- stringr::str_c("title: '", title, "'\n", yml_section)
    }
  }


  out <-
    yml_section %>%
    stringi::stri_replace_all(regex = "```|yaml|\\[\\]", replacement = "\n") %>%
    stringi::stri_replace_all(regex = "\\'FALSE\\'", replacement = "false") %>%
    stringi::stri_replace_all(regex = "\\'TRUE\\'", replacement = "true") %>%
    stringi::stri_replace_all(regex = "^\n\n\n", replacement = "") %>%
    as.character()
}
