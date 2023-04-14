test_that("gen_qmd_structure", {
  if(!rlang::is_null(quarto::quarto_path())) {
  results1 <-
    ex_survey_ch_overview %>%
    refine_data_overview(label_separator = " - ",
                         name_separator = "_",
                         data = ex_survey1) %>%

    gen_qmd_structure(data_overview = .,
                      elements = ex_survey_elements_list,
                      path = tempdir())

  testthat::expect_warning(object = {
  ex_survey_elements_list2 <-
    list(uni_cat_plot_html =
           lst_saros_elements(data_overview =
                                ex_survey_ch_overview %>%
                                refine_data_overview(group_by = c("label_prefix"),
                                                     data = ex_survey1,
                                                     label_separator = " - ",
                                                     name_separator = "_"),
                       element_name = "uni_cat_plot_html",
                       data = ex_survey1)
         )},
  regexp = "NAs introduced by coercion"
  )

  results2 <-
    ex_survey_ch_overview %>%
    refine_data_overview(label_separator = " - ",
                         name_separator = "_",
                         data = ex_survey1) %>%
    group_by(label_prefix) %>%

    gen_qmd_structure(data_overview = .,
                      elements = ex_survey_elements_list2,
                      path = tempdir())

  results3 <-
    ex_survey_ch_overview %>%
    refine_data_overview(label_separator = " - ",
                         name_separator = "_",
                         data = ex_survey1) %>%
    ungroup() %>%

    gen_qmd_structure(data_overview = .,
                      elements = ex_survey_elements_list,
                      path = tempdir())
}
})
