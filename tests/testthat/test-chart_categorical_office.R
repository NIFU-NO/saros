testthat::test_that("chart_categorical_office", {
  suppressMessages(library(dplyr))
  library(tibble)
  library(officer)


  docx_template <- system.file("template","NIFUmal_tom.docx", package = "saros", mustWork = TRUE)
  colour_palette <-
    readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx", package = "saros", mustWork = TRUE),
                       sheet = "NIFUblue") |>
    dplyr::pull(hex)
  chart_format <-
  readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx", package = "saros", mustWork = TRUE), sheet = 1) |>
    dplyr::filter(saros_style == "figure") |>
    dplyr::pull(template_style)

  testthat::expect_s3_class(object = {
  test <-
    ex_survey1 |>
    embed_plot_cat_docx(cols = b_1:b_3,
                        docx_template = docx_template,
                        colour_palette = colour_palette,
                        chart_formatting = chart_format,
                        font_family = "Calibri",
                        label_font_size = 9,
                        main_font_size = 9,
                        showNA = "no",
                        data_label = "percentage_bare",
                        digits = 0,
                        sort_by = c("A bit", "A lot"),
                        descend = TRUE,
                        height_per_col = .3,
                        height_fixed = 1
                        )
  }, class = "rdocx", exact = TRUE)
  x <- withr::with_tempfile(new = "test", code = {
    filepath <- print(test, target = "test.docx")
  }, fileext = ".docx")



  testthat::expect_error(object = embed_plot_cat_docx(mtcars, cols = c(cyl, vs, gear, carb)),
                         regexp = "Column `cyl` and column `vs` lack common categories")
  testthat::expect_error(object = embed_plot_cat_docx(ex_survey1, cols = tidyselect::matches("^[ab]")),
                         regexp = "Column `a_1` and column `b_1` lack common categories")

  testthat::expect_s3_class(object = {
  test <-
    ex_survey1 |>
    embed_plot_cat_docx(cols = a_1:a_9)
  }, class = "rdocx", exact = TRUE)
  x <- withr::with_tempfile(new = "test", code = {
    filepath <- print(test, target = "test.docx")
  }, fileext = ".docx")



    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      embed_plot_cat_docx(cols = a_1:a_9, showNA = "no")
}, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {

    test <-
      ex_survey1 |>
      mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly disagree"))) |>
      labelled::copy_labels_from(from = ex_survey1) |>
      embed_plot_cat_docx(cols = p_1:p_4)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <- # The dangerous one
      ex_survey1 |>
      mutate(across(p_1, ~forcats::fct_recode(.x, NULL = "Somewhat disagree"))) |>
      labelled::copy_labels_from(from = ex_survey1) |>
      embed_plot_cat_docx(cols = p_1:p_4,
                          docx_template = docx_template,
                          colour_palette = colour_palette,
                          chart_formatting = chart_format,
                          height_per_col = .3,
                          height_fixed = 1)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) |>
      labelled::copy_labels_from(from = ex_survey1) |>
      embed_plot_cat_docx(cols = p_1:p_4)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      mutate(across(p_1, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) |>
      labelled::copy_labels_from(from = ex_survey1) |>
      embed_plot_cat_docx(cols = p_1:p_4)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      embed_plot_cat_docx(cols = a_1:a_9, digits = 0L, data_label = "percentage_bare", font_family = "Calibri")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      embed_plot_cat_docx(cols = a_1:a_9, sort_by = ".count", descend = FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")






    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      embed_plot_cat_docx(cols = a_1:a_9, sort_by = ".count",
                          descend = TRUE, vertical=FALSE, showNA = "no",
                        percentage = FALSE)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      embed_plot_cat_docx(cols = b_1:b_3, sort_by = ".count",
                          descend = TRUE, vertical=FALSE, showNA = "no",
                          percentage = FALSE)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      embed_plot_cat_docx(cols = b_1:b_3, sort_by = "A bit", descend = FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 |>
      embed_plot_cat_docx(cols = b_1:b_3, sort_by = c("A bit", "A lot"), descend = FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 |>
        embed_plot_cat_docx(cols = b_1:b_3, sort_by = c("A bit", "A lot"),
                            descend = FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 |>
        embed_plot_cat_docx(cols = b_1:b_3, ignore_if_below = 10)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 |>
        embed_plot_cat_docx(cols = b_1:b_3, label_separator = " - ")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 |>
        embed_plot_cat_docx(cols = b_1, by = x1_sex)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 |>
        embed_plot_cat_docx(cols = b_1, by = x1_sex, sort_by = "A lot", descend = TRUE)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      filepath <- print(test, target = "test.docx")
    }, fileext = ".docx")


    testthat::expect_error(object = {
      test <-
        ex_survey1 |>
        embed_plot_cat_docx(cols = b_1, by = x1_sex:x2_human)
    }, regexp = "Too many columns provided for `by`")



  # testthat::expect_output_file(object =
  #                                filepath <- print(test,
  # file = system.file("template","test8.docx", package = "saros", mustWork = TRUE))

})
