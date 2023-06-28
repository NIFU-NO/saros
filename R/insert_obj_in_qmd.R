
insert_obj_in_qmd_inner <-
  function(element_name,
           obj_name,
           tailored_group = NULL,
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
        stringr::str_c('::: {.content-visible when-format="html"}')
      } else stringr::str_c('::: {.content-visible unless-format="html"}')

    conditional_end <- ":::\n"

    function_call_prefix <- # Replace with glue?
      dplyr::case_when(stringi::stri_detect(element_name, fixed = "plot_html") ~ 'ggiraph::girafe(ggobj = ',
                       stringi::stri_detect(element_name, regex = "table|sigtest") ~ 'kableExtra::kbl(',

                       stringi::stri_detect(element_name, fixed = "plot_pdf") ~ '(',
                       .default = '(')
    function_call_suffix <- # Replace with glue?
      dplyr::case_when(stringi::stri_detect(element_name, fixed = "plot_html") ~ ', options = ggiraph_options)',
                       stringi::stri_detect(element_name, regex = "table|sigtest") ~ ') |> \nkableExtra::kable_classic(lightable_options="hover")',
                       .default = ')')

    tbl_fig_prefix <- if(stringi::stri_detect(element_name, fixed = "plot")) "fig-" else "tbl-"



    label_caption <-
        stringr::str_c("#| label: '", tbl_fig_prefix, qmd_format, if(stringi::stri_length(qmd_format) > 0) "_", obj_name, "_",
                       stringr::str_c(sample(0:9, size=3, replace=TRUE), collapse=""), "'\n",
                       if(length(caption)>0) stringr::str_c(
                         "#| ", tbl_fig_prefix, "cap: '", caption, "'\n"),
                       if(tbl_fig_prefix == "fig-" && !is.na(figure_height)) stringr::str_c("#| fig-height: ", figure_height))

    # glue_spec <- as.character(glue::glue("{obj_name} <- \n readRDS(\"{filepath}\")\n{function_call_prefix}{obj_name})", .null = ""))
    stringr::str_c(obj_name, # Replace with glue?
                   ' <- \n  readRDS("', filepath, '")\n',
                   function_call_prefix, obj_name, function_call_suffix) %>%
      stringr::str_c(if(stringi::stri_length(qmd_format) > 0) conditional_start,
                     "```{r}",
                     label_caption,
                     .,
                     "```",
                     if(stringi::stri_length(qmd_format) > 0) conditional_end else "",
                     sep="\n")
  }




insert_obj_in_qmd <- function(element_name,
                              index,
                              variable_prefix = NULL,
                              tailored_group = NULL,
                              filepath_txt = NULL,
                              filepath,
                              figure_height = 10,
                              caption = NULL,
                              add_text = TRUE,
                              max_width_obj = .saros.env$defaults$max_width_obj,
                              max_width_file = .saros.env$defaults$max_width_file,
                              translations = .saros.env$defaults$translations,
                              call = rlang::caller_env()) {

  if(!is.null(filepath)) {

    if(stringi::stri_detect(element_name, fixed = "text")) {
      text <- tryCatch(readRDS(filepath_txt)[[1]],
                       error = function(e) cli::cli_warn("Unable to read text from {.path {filepath_txt}}. File not found.", call = call))
      return(text)
    }



    obj_name <-
      stringr::str_c(element_name, "_",
                     conv_to_valid_obj_name(index, max_width = max_width_obj),
                     variable_prefix)


    out <-
      insert_obj_in_qmd_inner(element_name = element_name,
                              obj_name = obj_name,
                              tailored_group = tailored_group,
                              filepath = filepath,
                              caption = caption,
                              figure_height = figure_height,
                              # translations = translations,
                              call = call)
    if(add_text) {
      stringr::str_c(out,
                     translations$empty_chunk_text,
                     sep="\n")
    } else out
  } else ""
}
