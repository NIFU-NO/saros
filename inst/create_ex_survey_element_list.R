library(dplyr)
# ex_survey_elements_list <-
#   list(
#     uni_cat_plot_html =
#       saros::lst_saros_elements(
#         data = saros::ex_survey1,
#         data_overview =
#           saros::ex_survey_ch_overview %>%
#           saros::refine_data_overview(data = saros::ex_survey1,
#                                       label_separator = " - ",
#                                       name_separator = "_"),
#         element_name = "uni_cat_plot_html",
#         label_separator = " - "
#       ),
#     bi_catcat_plot_html =
#       saros::lst_saros_elements(
#         data_overview =
#           saros::ex_survey_ch_overview %>%
#           saros::refine_data_overview(data = saros::ex_survey1,
#                                       label_separator = " - ",
#                                       name_separator = "_"),
#         element_name = "bi_catcat_plot_html",
#         data = saros::ex_survey1,
#         label_separator = " - ",
#         showNA = "no"
#       ),
#     bi_catcat_plot_docx =
#       saros::lst_saros_elements(
#         data_overview =
#           saros::ex_survey_ch_overview %>%
#           saros::refine_data_overview(data = saros::ex_survey1,
#                                       label_separator = " - ",
#                                       name_separator = "_"),
#         element_name = "bi_catcat_plot_docx",
#         data = saros::ex_survey1,
#         label_separator = " - ",
#         showNA = "no"
#       )
#     )
ex_survey_elements_list <-
  mass_lst_saros_elements(element_names = saros::list_available_element_types(),
                          data_overview =
                            saros::ex_survey_ch_overview %>%
                            saros::refine_data_overview(data = saros::ex_survey1,
                                                        label_separator = " - ",
                                                        name_separator = "_"),
                          data = saros::ex_survey1,
                          label_separator = " - ",
                          showNA = "no")
saveRDS(object = ex_survey_elements_list, file = "inst/extdata/ex_survey_elements_list.RDS", compress = FALSE)
usethis::use_data(ex_survey_elements_list, overwrite = TRUE)
