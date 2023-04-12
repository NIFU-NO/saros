test_that("prepare_mschart", {
  testthat::expect_equal(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = a_1:a_9,
      what = "per",
      digits = 0, percent_sign = FALSE) %>%
      dplyr::slice(1) %>% dplyr::pull(data_label),
    expected = "49")

  testthat::expect_equal(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = a_1:a_9,
      what = "per",
      sort_by = "value", desc = T) %>%
      dplyr::slice(1) %>% dplyr::pull(value),
    expected = 60)

  testthat::expect_equal(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = b_1:b_3,
      what = "per",
      sort_by = "A bit", desc = FALSE) %>%
      dplyr::slice(5) %>% dplyr::pull(value),
    expected = 42)

  testthat::expect_equal(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = b_1:b_3,
      what = "per",
      sort_by = c("A bit", "A lot"), desc = FALSE) %>%
      dplyr::slice(9) %>% dplyr::pull(sum_value),
    expected = 60)

  testthat::expect_equal(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = b_1:b_3,
      what = "percent",
      ignore_if_below = 10) %>%
      dplyr::slice(3) %>% dplyr::pull(data_label),
    expected = "")

  testthat::expect_equal(saros:::prepare_mschart_data(
    data = ex_survey1,
    cols = a_1:a_9,
    what = "fre", showNA = "no") %>%
      dplyr::slice(1) %>% dplyr::pull(data_label),
    "49")

  testthat::expect_equal(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = a_1:a_9,
      what = "fre",
      sort_by = "value", desc = T) %>%
      dplyr::slice(1) %>% dplyr::pull(value),
    expected = 60)


  testthat::expect_error(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = a_1:a_9,
      by = b_1:b_3),
    regexp = "Too many columns provided for `by`")

  testthat::expect_error(
    saros:::prepare_mschart_data(
      data = ex_survey1,
      cols = a_1:a_9,
      by = x1_sex),
    regexp = "Multiple columns for `cols` and `by` are not allowed")

})
#########################################################


testthat::test_that("report_chart_likert(what='percent')", {
  suppressMessages(library(dplyr))
  library(labelled)
  library(tibble)
  library(readxl)
  library(officer)


  docx_template <- system.file("template","NIFUmal_tom.docx", package = "saros", mustWork = TRUE)
  colour_palette <-
    readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx", package = "saros", mustWork = TRUE),
                       sheet = "NIFUblue") %>%
    dplyr::pull(hex)
  chart_format <-
  readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx", package = "saros", mustWork = TRUE), sheet = 1) %>%
    dplyr::filter(saros_style == "figure") %>%
    dplyr::pull(template_style)

  testthat::expect_s3_class(object = {
  test <-
    ex_survey1 %>%
    report_chart_likert(cols = b_1:b_3,
                        docx_template = docx_template,
                        colour_palette = colour_palette,
                        chart_formatting = chart_format,
                        font_family = "Calibri",
                        label_font_size = 9,
                        main_font_size = 9,
                        showNA = "no",
                        what = "percent",
                        digits = 0,
                        percent_sign = FALSE,
                        sort_by = c("A bit", "A lot"),
                        desc = TRUE,
                        height_per_col = .3,
                        height_fixed = 1
                        )
  }, class = "rdocx", exact = TRUE)
  x <- withr::with_tempfile(new = "test", code = {
    print(test, target = "test.docx")
  }, fileext = ".docx")



  testthat::expect_error(object = report_chart_likert(mtcars, cols = c(cyl, vs, gear, carb)),
                         regexp = "Column `cyl` and column `vs` lack common categories")
  testthat::expect_error(object = report_chart_likert(ex_survey1, cols = tidyselect::matches("^[ab]")),
                         regexp = "Column `a_1` and column `b_1` lack common categories")

  testthat::expect_s3_class(object = {
  test <-
    ex_survey1 %>%
    report_chart_likert(cols = a_1:a_9)
  }, class = "rdocx", exact = TRUE)
  x <- withr::with_tempfile(new = "test", code = {
    print(test, target = "test.docx")
  }, fileext = ".docx")



    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, showNA = "no")
}, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {

    test <-
      ex_survey1 %>%
      mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly disagree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <- # The dangerous one
      ex_survey1 %>%
      mutate(across(p_1, ~forcats::fct_recode(.x, NULL = "Somewhat disagree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4,
                          docx_template = docx_template,
                          colour_palette = colour_palette,
                          chart_formatting = chart_format,
                          height_per_col = .3,
                          height_fixed = 1)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      mutate(across(p_1, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, digits = 0L, percent_sign = FALSE, font_family = "Calibri")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, sort_by = "value", desc=FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")






    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, sort_by = "value", desc=T, vertical=FALSE, showNA = "no",
                          what = "fre")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")





    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      report_chart_likert(cols = b_1:b_3, sort_by = "value", desc=T, vertical=FALSE, showNA = "no",
                          what = "fre")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      report_chart_likert(cols = b_1:b_3, sort_by = "A bit", desc=FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
    test <-
      ex_survey1 %>%
      report_chart_likert(cols = b_1:b_3, sort_by = c("A bit", "A lot"), desc=FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 %>%
        report_chart_likert(cols = b_1:b_3, sort_by = c("A bit", "A lot"), desc=FALSE, vertical=FALSE, showNA = "no")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")




    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 %>%
        report_chart_likert(cols = b_1:b_3, ignore_if_below = 10)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 %>%
        report_chart_likert(cols = b_1:b_3, label_separator = " - ")
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 %>%
        report_chart_likert(cols = b_1, by = x1_sex)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")



    testthat::expect_s3_class(object = {
      test <-
        ex_survey1 %>%
        report_chart_likert(cols = b_1, by = x1_sex, sort_by = "A lot", desc=T)
    }, class = "rdocx", exact = TRUE)
    x <- withr::with_tempfile(new = "test", code = {
      print(test, target = "test.docx")
    }, fileext = ".docx")


    testthat::expect_error(object = {
      test <-
        ex_survey1 %>%
        report_chart_likert(cols = b_1, by = x1_sex:x2_human)
    }, regexp = "Too many columns provided for `by`")



  # testthat::expect_output_file(object =
  #                                print(test,
  # file = system.file("template","test8.docx", package = "saros", mustWork = TRUE))

})
