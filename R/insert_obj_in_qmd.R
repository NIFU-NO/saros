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
           serialized_format = eval(formals(draft_report)$serialized_format)[1],
           function_call_prefix = NULL,
           function_call_suffix = NULL,
           translations = eval(formals(draft_report)$translations),
           call = rlang::caller_env()) {

    # Early returns
    if(!rlang::is_string(filepath)) return("")

    if(stringi::stri_detect(element_name, fixed = "text")) {
      text <- tryCatch(serialize_read(filepath_txt, format = serialized_format)[[1]],
                       error = function(e) cli::cli_warn("Unable to read text from {.path {filepath_txt}}. File not found.", call = call))
      return(text)
    }

    obj_name <- index
      # stringi::stri_c(conv_to_valid_obj_name(index,
      #                                        max_width = max_width_obj),
      #                 ignore_null=TRUE)

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
                        ignore_null=TRUE)
      }

    hashpipe_label <-
        stringi::stri_c("#| label: '", label, "'", ignore_null=TRUE)


    chunk_body <- stringi::stri_c(obj_name, # Replace with glue?
                                  ' <- \n  ',
                                  serialize_read_syntax(format=serialized_format),
                                  '("', filepath, '")\n',
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
