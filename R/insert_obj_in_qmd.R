
insert_obj_in_qmd_inner <-
  function(element_name,
           obj_name,
           mesos_group = NULL,
           filepath,
           caption = NULL,
           figure_height = 10,
           # translations = .saros.env$defaults$translations,
           call = rlang::caller_env()) {

    qmd_format <-
      if(stringi::stri_detect(element_name, fixed = "html")) {
        "html"
      } else if(stringi::stri_detect(element_name, fixed = "pdf")) {
        "pdf"
      } else ""

    conditional_start <-
      if(qmd_format == "html") {
        stringi::stri_c(ignore_null=TRUE, '::: {.content-visible when-format="html"}')
      } else stringi::stri_c(ignore_null=TRUE, '::: {.content-visible unless-format="html"}')

    conditional_end <- ":::\n"

    function_call_prefix <- # Replace with glue?
      dplyr::case_when(stringi::stri_detect(element_name, regex = "plot[2-9]*_html") ~ 'ggiraph::girafe(ggobj = ',
                       stringi::stri_detect(element_name, regex = "table|sigtest") ~ '', #kableExtra::kbl(

                       stringi::stri_detect(element_name, regex = "plot[2-9]*_pdf") ~ '(',
                       .default = '(')
    function_call_suffix <- # Replace with glue?
      dplyr::case_when(stringi::stri_detect(element_name, regex = "plot[2-9]*_html") ~ ')',
                       stringi::stri_detect(element_name, regex = "table|sigtest") ~ '', #)
                       .default = ')')

    tbl_fig_prefix <- if(stringi::stri_detect(element_name, fixed = "plot")) "fig-" else "tbl-"


    filepath_xlsx <- stringi::stri_replace_all_regex(str = filepath,
                                                     pattern = "(.*)\\.[[:alnum:]]+$",
                                                     replacement = "$1.xlsx")

    if(stringi::stri_detect(element_name, regex = "plot|table")) caption <- stringi::stri_c(ignore_null=TRUE, caption, " [xlsx](", filepath_xlsx, ")")

    hashpipe_string <-
        stringi::stri_c("#| label: '", tbl_fig_prefix, qmd_format, if(stringi::stri_length(qmd_format) > 0) "_", obj_name, "_",
                       stringi::stri_c(ignore_null=TRUE, sample(0:9, size=3, replace=TRUE), collapse=""), "'\n",
                       if(length(caption)>0) stringi::stri_c(ignore_null=TRUE,
                         "#| ", tbl_fig_prefix, "cap: '", caption, "'\n"),
                       if(tbl_fig_prefix == "fig-" && !is.na(figure_height)) stringi::stri_c(ignore_null=TRUE, "#| fig-height: ", figure_height),
                       ignore_null=TRUE)

    # glue_spec <- as.character(glue::glue("{obj_name} <- \n readRDS(\"{filepath}\")\n{function_call_prefix}{obj_name})", .null = ""))
    stringi::stri_c(ignore_null=TRUE, obj_name, # Replace with glue?
                   ' <- \n  readRDS("', filepath, '")\n',
                   function_call_prefix, obj_name, function_call_suffix) %>%
      stringi::stri_c(if(stringi::stri_length(qmd_format) > 0) conditional_start,
                     stringi::stri_c("```{r, '",obj_name, "'}", ignore_null = TRUE),
                     hashpipe_string,
                     .,
                     "```",
                     if(stringi::stri_length(qmd_format) > 0) conditional_end else "",
                     sep="\n", ignore_null=TRUE)
  }




insert_obj_in_qmd <- function(element_name,
                              index,
                              variable_prefix = NULL,
                              mesos_group = NULL,
                              filepath_txt = NULL,
                              filepath,
                              figure_height = 10,
                              caption = NULL,
                              add_text = TRUE,
                              max_width_obj = eval(formals(draft_report)$max_width_obj),
                              max_width_file = eval(formals(draft_report)$max_width_file),
                              translations = eval(formals(draft_report)$translations),
                              call = rlang::caller_env()) {

  if(!is.null(filepath)) {

    if(stringi::stri_detect(element_name, fixed = "text")) {
      text <- tryCatch(readRDS(filepath_txt)[[1]],
                       error = function(e) cli::cli_warn("Unable to read text from {.path {filepath_txt}}. File not found.", call = call))
      return(text)
    }



    obj_name <-
      stringi::stri_c(conv_to_valid_obj_name(index, max_width = max_width_obj),
                     variable_prefix,
                     ignore_null=TRUE)


    out <-
      insert_obj_in_qmd_inner(element_name = element_name,
                              obj_name = obj_name,
                              mesos_group = mesos_group,
                              filepath = filepath,
                              caption = caption,
                              figure_height = figure_height)
    if(add_text) {
      stringi::stri_c(ignore_null=TRUE,
                      out,
                      "\n\n",
                      translations$empty_chunk_text,
                      "\n\n",
                     sep="")
    } else out
  } else ""
}
