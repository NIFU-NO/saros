testthat::test_that("gen_qmd_chapter", {
  library(dplyr)
  temp_folder <- tempdir()
  data_overview <-
    ex_survey_ch_overview %>%
    refine_data_overview(data = ex_survey1,
                                label_separator = " - ",
                                name_separator = "_") %>%
    filter(chapter == "2 Ambivalence")

  # Define the elements list, using the placeholder names



  chapter_filepath <-
    saros:::gen_qmd_chapter(data_overview = data_overview,
                  elements = ex_survey_elements_list,
                  path = temp_folder)
  unlink(temp_folder)

})
