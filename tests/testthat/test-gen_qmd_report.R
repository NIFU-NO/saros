test_that("gen_qmd_report", {
  library(dplyr)
  temp_folder <- tempdir()
  data_overview <-
   ex_survey_ch_overview %>%
    saros::refine_data_overview(data = ex_survey1,
                                label_separator = " - ",
                                name_separator = "_")


  testthat::expect_no_error(
  index_filepath <-
    gen_qmd_report(
      data_overview = data_overview,
      elements = ex_survey_elements_list,
      captions = "pretty",
      path = temp_folder))

  testthat::expect_equal(object = index_filepath,
                         expected = fs::path(temp_folder, "index.qmd"))

  testthat::expect_no_error(
  quarto::quarto_render(fs::path(temp_folder, "index.qmd"), quiet = TRUE))
  unlink(temp_folder)

})
