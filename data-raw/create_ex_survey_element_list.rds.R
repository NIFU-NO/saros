library(dplyr)
# ex_survey_elements_list <-
#   list(
#     uni_cat_prop_plot_html =
#       saros::lst_saros_elements(
#         data = saros::ex_survey,
#         chapter_overview =
#           saros::ex_survey_ch_overview %>%
#           saros::refine_chapter_overview(data = saros::ex_survey,
#                                       label_separator = " - ",
#                                       name_separator = "_"),
#         element_name = "uni_cat_prop_plot_html",
#         label_separator = " - "
#       ),
#     bi_catcat_prop_plot_html =
#       saros::lst_saros_elements(
#         chapter_overview =
#           saros::ex_survey_ch_overview %>%
#           saros::refine_chapter_overview(data = saros::ex_survey,
#                                       label_separator = " - ",
#                                       name_separator = "_"),
#         element_name = "bi_catcat_prop_plot_html",
#         data = saros::ex_survey,
#         label_separator = " - ",
#         showNA = "never"
#       ),
#     bi_catcat_prop_plot_docx =
#       saros::lst_saros_elements(
#         chapter_overview =
#           saros::ex_survey_ch_overview %>%
#           saros::refine_chapter_overview(data = saros::ex_survey,
#                                       label_separator = " - ",
#                                       name_separator = "_"),
#         element_name = "bi_catcat_prop_plot_docx",
#         data = saros::ex_survey,
#         label_separator = " - ",
#         showNA = "never"
#       )
#     )
# library(dplyr)
# system.time( # 50 sec
# ex_survey_elements_list <-
#   mass_lst_saros_elements(element_names =
#                             saros:::.saros.env$defaults$element_names[c(1,4)],# %>%
#                             # stringi::stri_match_all_regex(pattern = ".*_table_html|.*_sigtest|.*_text") %>%
#                             # unlist(),
#                           chapter_overview =
#                             saros::ex_survey_ch_overview %>%
#                             saros::refine_chapter_overview(data = saros::ex_survey,
#                                                         organize_by = c("chapter", ".variable_label_prefix"),
#                                                         label_separator = " - ",
#                                                         name_separator = "_"),
#                           data = saros::ex_survey,
#                           label_separator = " - ",
#                           showNA = "never")
# )
# qs::qsave(object = ex_survey_elements_list, file = "inst/extdata/ex_survey_elements_list.RDS", compress = FALSE)
# usethis::use_data(ex_survey_elements_list, overwrite = TRUE)
