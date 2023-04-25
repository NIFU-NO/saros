
insert_obj_in_qmd <- function(element_name, index, filepath) {

  if(!is.null(filepath)) {

    obj_name <-
      stringr::str_c(element_name, "_", index) %>%
      conv_to_valid_obj_name()

    qmd_format <-
      if(stringr::str_detect(element_name, "html")) {
        "html"
      } else if(stringr::str_detect(element_name, "docx")) {
        "docx"
      } else if(stringr::str_detect(element_name, "pdf")) {
        "pdf"
      } else ""

    conditional_start <-
      stringr::str_c('::: {.content-visible when-format="', qmd_format, '"}\n')

    conditional_end <- ":::\n"

    if(stringr::str_detect(element_name, "text", negate = TRUE)) {
      function_call_prefix <-
          dplyr::case_when(stringr::str_detect(element_name, "plot_html") ~ 'ggiraph::girafe(ggobj = ',
                           stringr::str_detect(element_name, "table_html") ~ 'reactable::reactable(',
                           stringr::str_detect(element_name, "plot_docx") ~ 'mschart::body_add_chart(doc = chapter_docx, chart = ',
                           stringr::str_detect(element_name, "table_docx") ~ 'flextable::body_add_flextable(doc = chapter_docx, chart = ',
                           stringr::str_detect(element_name, "sigtest") ~ 'reactable::reactable(',
                           .default = '(')

      stringr::str_c(
        "### ", element_name, "\n",
        obj_name,
        ' <- \n  readRDS("', filepath, '")\n',
        function_call_prefix, obj_name, ')') %>%
        rcode_to_quarto(code = ., call = call) %>%
        stringr::str_c(if(nchar(qmd_format) > 0) conditional_start,
                       .,
                       if(nchar(qmd_format) > 0) conditional_end)
    } else readRDS(filepath)
  } else ""
}
