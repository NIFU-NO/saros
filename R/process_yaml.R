process_yaml <- function(yaml_file = NULL,
                         title = NULL,
                         authors = NULL,
                         add_fences = TRUE) {

  if(!rlang::is_string(yaml_file)) {
    yml_section <-
      list(title = title,
           format = "html",
           echo = FALSE,
           authors = authors)
  } else {
    yml_section <- yaml::read_yaml(file = yaml_file)
    if(any(names(yml_section) == "translations")) {
      yml_section$translations <- unlist(yml_section$translations, recursive = FALSE)
    }
    new_title <- stringi::stri_c(yml_section$title,
                                 if(rlang::is_string(title) &&
                                    rlang::is_string(yml_section$title)) " - ",
                                 title, ignore_null=TRUE)

    if(length(new_title)>0)  yml_section$title <- new_title
    if(is.null(yml_section$title)) yml_section$title <- NULL
    if(rlang::is_character(authors)) yml_section$authors <- authors

  }

  yml_section <- yaml::as.yaml(yml_section,
                               handlers = list(
    logical = function(x) {
      result <- ifelse(x, "true", "false")
      class(result) <- "verbatim"
      result
    }))

  if(add_fences) {
    yml_section <- stringi::stri_c("---",
                                   yml_section,
                                   "---",
                                   sep="\n",
                                   ignore_null = TRUE)
  }
  yml_section
}
