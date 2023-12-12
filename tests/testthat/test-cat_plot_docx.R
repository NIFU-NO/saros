testthat::test_that("chart cat_docx", {
  library(saros)
  suppressMessages(library(dplyr))
  library(officer)


  testthat::expect_s3_class(object = {
  test <-
    ex_survey |>
    embed_cat_prop_plot_docx(dep = b_1:b_3,
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
                        height_fixed = 1,
                        return_raw = FALSE
                        )
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
  x <- withr::with_tempfile(new = "test", code = {
    filepath <- print(test, target = "test.docx")
  }, fileext = ".docx")
  ))



  testthat::expect_error(object = embed_cat_prop_plot_docx(mtcars,
                                                           dep = c(cyl, vs, gear, carb),
                                                      return_raw = FALSE),
                         regexp = "Column `cyl` and column `vs` lack common categories")
  testthat::expect_error(object = embed_cat_prop_plot_docx(ex_survey, dep = tidyselect::matches("^[ab]"),
                                                      return_raw = FALSE),
                         regexp = "Column `a_1` and column `b_1` lack common categories")

  testthat::expect_s3_class(object = {
  test <-
    ex_survey |>
    embed_cat_prop_plot_docx(dep = a_1:a_9,
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
                             height_fixed = 1,
                        return_raw = FALSE)
  }, class = "rdocx", exact = TRUE)
  invisible(capture.output(
  x <- withr::with_tempfile(new = "test", code = {
    filepath <- print(test, target = "test.docx")
  }, fileext = ".docx")
))


    testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      embed_cat_prop_plot_docx(dep = a_1:a_9,
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
                               height_fixed = 1,
                          return_raw = FALSE)
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
      embed_cat_prop_plot_docx(dep = p_1:p_4,
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
                               height_fixed = 1,
                          return_raw = FALSE)
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
      embed_cat_prop_plot_docx(dep = p_1:p_4,
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
                          descend = TRUE,
                          return_raw = FALSE)
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
      embed_cat_prop_plot_docx(dep = p_1:p_4,
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
                               height_fixed = 1,
                          return_raw = FALSE)
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
      embed_cat_prop_plot_docx(dep = p_1:p_4,
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
                               height_fixed = 1,
                          return_raw = FALSE)
    }, class = "rdocx", exact = TRUE)
    invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
    ))





    testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      embed_cat_prop_plot_docx(dep = a_1:a_9, digits = 0L,
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
                               height_fixed = 1,
                          return_raw = FALSE)
    }, class = "rdocx", exact = TRUE)
    invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
    ))





    testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      embed_cat_prop_plot_docx(dep = a_1:a_9, sort_by = ".count",
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
                          height_fixed = 1,
                          return_raw = FALSE)
    }, class = "rdocx", exact = TRUE)
    invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
    ))






    testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      embed_cat_prop_plot_docx(dep = a_1:a_9,
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
                        height_fixed = 1,
                        return_raw = FALSE)
    }, class = "rdocx", exact = TRUE)
    invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
    ))





    testthat::expect_s3_class(object = {
    test <-
      ex_survey |>
      embed_cat_prop_plot_docx(dep = b_1:b_3,
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
                               height_fixed = 1,
                          return_raw = FALSE)
    }, class = "rdocx", exact = TRUE)
    invisible(capture.output(
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")
    ))







    testthat::expect_error(object = {
      test <-
        ex_survey |>
        embed_cat_prop_plot_docx(dep = b_1,
                                 indep = x1_sex:x2_human,
                            return_raw = FALSE)
    }, regexp = "Too many columns provided for `indep`")



  # testthat::expect_output_file(object =
  #                                filepath <- print(test,
  # file = system.file("template","test8.docx", package = "saros", mustWork = TRUE))

})
