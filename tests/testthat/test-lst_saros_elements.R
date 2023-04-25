testthat::test_that("gen_element produces elements", {
  library(dplyr)

  testthat::expect_no_error(
  result <-
    ex_survey_ch_overview %>%
    refine_data_overview(data = ex_survey1, name_separator = "_",
                         label_separator = " - ",
                         group_by = c("col_group", "name_prefix")) %>%
    filter(designated_role == "dep", designated_type == "cat") %>%
    lst_saros_elements(data_overview = .,
                       data = ex_survey1,

                               element_name = "uni_cat_plot_docx",
                               label_separator = " - ",
                       showNA = "never")
  )


})
