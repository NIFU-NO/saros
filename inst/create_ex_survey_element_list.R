library(dplyr)
ex_survey_elements_list <-
  list(
    uni_cat_plot_html =
      saros::lst_saros_elements(
        data = saros::ex_survey1,
        data_overview =
          saros::ex_survey_ch_overview %>%
          saros::refine_data_overview(label_separator = " - ",
                               name_separator = "_",
                               data = saros::ex_survey1),
        element_name = "uni_cat_plot_html",
        label_separator = " - "
      ),
    bi_catcat_plot_html =
      saros::lst_saros_elements(
        data = saros::ex_survey1,
        data_overview =
          saros::ex_survey_ch_overview %>%
          saros::refine_data_overview(label_separator = " - ",
                                      name_separator = "_",
                                      data = saros::ex_survey1),
        element_name = "bi_catcat_plot_html",
        label_separator = " - "
      ))
saveRDS(object = ex_survey_elements_list, file = "inst/extdata/ex_survey_elements_list.RDS", compress = FALSE)
usethis::use_data(ex_survey_elements_list, overwrite = TRUE)
