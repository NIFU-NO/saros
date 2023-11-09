insert_obj_in_qmd <-
  function(element_name,
           mesos_group = NULL,
           filepath,
           filepath_txt = NULL,
           caption = NULL,
           figure_height = 10,
           add_text = TRUE,
           index,
           max_width_file = eval(formals(draft_report)$max_width_file),
           max_width_obj = eval(formals(draft_report)$max_width_obj),
           translations = eval(formals(draft_report)$translations),
           call = rlang::caller_env()) {

    # Early returns
    if(!rlang::is_string(filepath)) return("")

    if(stringi::stri_detect(element_name, fixed = "text")) {
      text <- tryCatch(qs::qread(filepath_txt)[[1]],
                       error = function(e) cli::cli_warn("Unable to read text from {.path {filepath_txt}}. File not found.", call = call))
      return(text)
    }

    obj_name <- index
      # stringi::stri_c(conv_to_valid_obj_name(index,
      #                                        max_width = max_width_obj),
      #                 ignore_null=TRUE)



    function_call_prefix <- # Replace with glue?
      dplyr::case_when(stringi::stri_detect(element_name, regex = "plot[2-9]*") ~ 'ggiraph::girafe(ggobj = ',
                       stringi::stri_detect(element_name, regex = "table|sigtest") ~ '', #kableExtra::kbl(

                       .default = '(')
    function_call_suffix <- # Replace with glue?
      dplyr::case_when(stringi::stri_detect(element_name, regex = "plot[2-9]*") ~ ')',
                       stringi::stri_detect(element_name, regex = "table|sigtest") ~ '', #)
                       .default = ')')

    tbl_fig_prefix <- if(stringi::stri_detect(element_name, fixed = "plot")) "fig-" else "tbl-"


    filepath_xlsx <- stringi::stri_replace_all_regex(str = filepath,
                                                     pattern = "(.*)\\.[[:alnum:]]+$",
                                                     replacement = "$1.xlsx")

    if(stringi::stri_detect(element_name, regex = "plot|table")) {
      caption <- stringi::stri_c(ignore_null=TRUE, caption, " [xlsx](", filepath_xlsx, ")")
    }

    ### Create lines for the chunk


    label <-
      stringi::stri_c(tbl_fig_prefix,
                      obj_name,
                      ignore_null = TRUE)


    r_chunk_header_line <-
      stringi::stri_c("```{r}", ignore_null = TRUE)

    hashpipe_caption <-
      if(length(caption)>0) stringi::stri_c( "#| ", tbl_fig_prefix, "cap: '", caption, "'",
                                             ignore_null=TRUE)
    hashpipe_figheight <-
      if(tbl_fig_prefix == "fig-" &&
         !is.na(figure_height)) {
        stringi::stri_c("#| fig-height: ", figure_height, "\n",
                        "#| fig-dpi: 450", ignore_null=TRUE)
      }

    hashpipe_label <-
        stringi::stri_c("#| label: '", label, "'", ignore_null=TRUE)


    chunk_body <- stringi::stri_c(obj_name, # Replace with glue?
                                  ' <- \n  qs::qread("', filepath, '")\n',
                                  function_call_prefix, obj_name, function_call_suffix,
                                  ignore_null=TRUE)

    separator_lorem_ipsum <-
      if(add_text) {
      stringi::stri_c(
                      "",
                      translations$empty_chunk_text,
                      "",
                      sep="", ignore_null=TRUE)
    } else ""

      stringi::stri_c(r_chunk_header_line,
                     hashpipe_label,
                     hashpipe_caption,
                     hashpipe_figheight,
                     chunk_body,
                     "```",
                     separator_lorem_ipsum,
                     sep="\n", ignore_null=TRUE)
  }
