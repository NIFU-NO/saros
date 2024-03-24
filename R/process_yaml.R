process_yaml <- function(yaml_file = NULL,
                         format = "html",
                         title = NULL,
                         authors = NULL,
                         add_fences = TRUE,
                         chapter_number = NA) {

  if(!is_string(yaml_file)) {
    yaml_section <-
      list(format = format,
           echo = FALSE,
           `fig-dpi` = 800,
           authors = authors)
  } else {
    yaml_section <- yaml::read_yaml(file = yaml_file)
    if(any(names(yaml_section) == "translations")) {
      yaml_section$translations <- unlist(yaml_section$translations, recursive = FALSE)
    }
    yaml_section$title <- title
    # new_title <- stringi::stri_c(yaml_section$title,
    #                              if(is_string(title) &&
    #                                 is_string(yaml_section$title)) " - ",
    #                              title,
    #                              ignore_null=TRUE)

    # if(length(new_title)>0)  yaml_section$title <- new_title
    if(is.character(authors)) yaml_section$authors <- authors

  }
  # if(is.null(yaml_section$title)) {
  #   yaml_section$title <-  as.character(chapter_number)
  # }
  if(is.null(yaml_section$authors) || all(nchar(yaml_section$authors)==0)) {
    yaml_section$authors <- NULL
  }
  if(!is.na(chapter_number)) {
    yaml_section[["number-offset"]] <- chapter_number - 1
  }


  yaml_section <- yaml::as.yaml(yaml_section,
                               handlers = list(
    logical = function(x) {
      result <- ifelse(x, "true", "false")
      class(result) <- "verbatim"
      result
    }))

  if(add_fences) {
    yaml_section <- stringi::stri_c("---",
                                   yaml_section,
                                   "---",
                                   sep="\n",
                                   ignore_null = TRUE)
  }
  yaml_section
}
