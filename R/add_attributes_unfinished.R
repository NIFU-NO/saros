# add_attributes <-
#   function(x,
#            element_name,
#            data,
#            ...
#   ) {
#
#
#
#
#     ## cat_*_plot
#     if (!is.null(dots$label_separator)) {
#       by_label <- unname(get_raw_labels(data = data, col_pos = indep_pos))
#       attr(chart, "saros_caption") <-
#         get_raw_labels(data = data, col_pos = dep_pos) %>%
#         get_main_question2(label_separator = dots$label_separator) %>%
#         add_caption_attribute(
#           data_out = data_out,
#           by_pos = by_label,
#           mesos_group = mesos_group,
#           translations = dots$translations
#         )
#     }
#
#     ## cat_table
#     main_question <-
#       get_raw_labels(data = data, col_pos = dep_pos) %>%
#       get_main_question2(label_separator = dots$label_separator,
#                          warn_multiple = TRUE, call = call) %>%
#       unique()
#
#     if(length(indep_pos)>0) {
#       by_label <-
#         get_raw_labels(data = data, col_pos = indep_pos) %>%
#         get_main_question2(label_separator = dots$label_separator,
#                            warn_multiple = TRUE, call = call) %>%
#         unique()
#     } else by_label <- character(0)
#     attr(table, "saros_caption") <-
#       if(!is.null(dots$label_separator)) {
#         add_caption_attribute(
#           main_question = main_question,
#           data_out = data_out,
#           by_pos = by_label,
#           mesos_group = mesos_group,
#           translations = dots$translations)
#       }
#
#     ################
#
#     output_format
#     conditional_start
#     tbl_fig_tag
#
#     ## caption
#     # caption_label
#     # caption_by
#     # caption_mesos_group
#     # caption_N
#     # caption_data_download_text
#     # caption_data_download_filepath
#
#     # caption_glue_pattern
#
#     label
#     figure_height
#     figure_width
#     import_prefix
#     import_path
#     import_suffix
#     obj_call_prefix
#     obj_name
#     obj_call_suffix
#     conditional_end
#
#     qmd_string <-
#       stringi::stri_c(
#
#       )
#
#     attr(x, "saros_qmd_string") <- qmd_string
#     x
#
#   }
