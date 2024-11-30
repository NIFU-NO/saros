testthat::test_that("chart cat_docx", {
  library(saros)
  suppressMessages(library(dplyr))
  library(officer)


  testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      makeme(data=_,
             type="cat_prop_plot_docx",
             dep = paste0("b_", 1:3),
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             showNA = "always",
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             sort_by = c("A bit", "A lot"),
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1
      )
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))



  testthat::expect_error(object = makeme(data=mtcars,
                                         type="cat_prop_plot_docx",
                                         dep = c(cyl, vs, gear, carb)),
                         regexp = "Column `cyl` and column `vs` lack common categories")
  testthat::expect_error(object = makeme(data=ex_survey,
                                         type="cat_prop_plot_docx",
                                         dep = tidyselect::matches("^[ab]")),
                         regexp = "Column `a_1` and column `b_1` lack common categories")

  testthat::expect_s3_class(object = {
    test <-
      makeme(data=ex_survey,
             dep = paste0("a_", 1:9),
             type="cat_prop_plot_docx",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             showNA = "always",
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))


  testthat::expect_s3_class(object = {
    test <-
      makeme(data = ex_survey,
             dep = paste0("a_", 1:9),
             type = "cat_prop_plot_docx",
             showNA = "never",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))



  testthat::expect_s3_class(object = {

    test <-
      ex_survey |>
      dplyr::mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly disagree"))) |>
      labelled::copy_labels_from(from = ex_survey) |>
      makeme(data=_,
             dep = paste0("p_", 1:4),
             type = "cat_prop_plot_docx",
             showNA = "never",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))





  testthat::expect_s3_class(object = {
    test <- # The dangerous one
      ex_survey |>
      dplyr::mutate(across(p_1, ~forcats::fct_recode(.x, NULL = "Somewhat disagree"))) |>
      labelled::copy_labels_from(from = ex_survey) |>
      makeme(data=_,
             dep = paste0("p_", 1:4),
             type="cat_prop_plot_docx",
             height_per_col = .3,
             height_fixed = 1,
             showNA = "never",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))





  testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      dplyr::mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) |>
      labelled::copy_labels_from(from = ex_survey) |>
      makeme(data=_,
             dep = paste0("p_", 1:4),
             type="cat_prop_plot_docx",
             showNA = "never",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))





  testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      dplyr::mutate(dplyr::across(p_1, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) |>
      labelled::copy_labels_from(from = ex_survey) |>
      makeme(data=_,
             dep = paste0("p_", 1:4),
             type="cat_prop_plot_docx",
             showNA = "never",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))





  testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      makeme(data=_,
             dep = paste0("a_", 1:9),
             type="cat_prop_plot_docx",
             data_label = "percentage_bare",
             font_family = "Calibri",
             showNA = "never",
             label_font_size = 9,
             main_font_size = 9,
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))





  testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      makeme(data=_,
             dep = paste0("a_", 1:9),
             type="cat_prop_plot_docx",
             sort_by = ".count",
             descend = FALSE,
             vertical=FALSE,
             showNA = "never",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             digits = 0,
             hide_label_if_prop_below = 0,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))






  testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      makeme(data=_,
             dep = paste0("a_", 1:9),
             type="cat_prop_plot_docx",
             sort_by = ".count",
             descend = TRUE,
             vertical=FALSE,
             showNA = "never",

             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             digits = 0,
             hide_label_if_prop_below = 0,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))





  testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      makeme(data=_,
             dep = paste0("b_", 1:3),
             type = "cat_prop_plot_docx",
             sort_by = ".count",

             showNA = "never",
             font_family = "Calibri",
             label_font_size = 9,
             main_font_size = 9,
             data_label = "percentage_bare",
             data_label_decimal_symbol = ".",
             vertical = FALSE,
             digits = 0,
             hide_label_if_prop_below = 0,
             descend = TRUE,
             height_per_col = .3,
             height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
  ))







  testthat::expect_error(object = {
    test <-
      ex_survey |>
      makeme(data=_,
             dep = paste0("b_", 1),
             indep = c("x1_sex","x2_human"),
             type="cat_prop_plot_docx")
  }, regexp = "Too many columns provided for `indep`")



  # testthat::expect_output_file(object =
  #                                filepath <- print(test,
  # file = system.file("template","test8.docx", package = "saros", mustWork = TRUE))

})
