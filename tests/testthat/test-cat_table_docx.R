# testthat::test_that("crosstable_to_apa", {
  # suppressMessages(library(dplyr))

  # testthat::expect_s3_class(object = {
  #
  #   test <-
  #     crosstable::crosstable(ex_survey1, b_1:b_3, percent_pattern = "{p_col}", percent_digits = 0) %>%
  #     # crosstable::pivot_crosstable() %>%
  #     embed_table_cat_docx(label_separator=" - ",
  #                          caption_style = "Table Caption",
  #                          body_style = "Normal Table",
  #                          footer_style = "Normal",
  #                          table_header_style = "Normal Table",
  #                          topcaption = F)
  # }, class = "rdocx", exact = TRUE)
  #
  # x <- withr::with_tempfile(new = "test", code = {
  #   filepath <- print(test, target = "test.docx")
  # }, fileext = ".docx")
  #
  #
  #
  #
  # testthat::expect_s3_class(object = {
  #   test <-
  #     crosstable::crosstable(ex_survey1, b_1:b_3, percent_pattern = "{p_col}", percent_digits = 0) %>%
  #     # crosstable::pivot_crosstable() %>%
  #     embed_table_cat_docx(label_separator=" - ",
  #                          caption_style = "Table Caption",
  #                          body_style = "Normal Table",
  #                          footer_style = "Normal",
  #                          table_header_style = "Normal Table",
  #                          topcaption = FALSE, return_docx = FALSE)
  # }, class = "flextable", exact = TRUE)
  #
# })
