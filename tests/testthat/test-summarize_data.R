testthat::test_that("summarize_data", {

  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = a_1:a_9,
      percentage = TRUE,
      digits = 0, percent_sign = FALSE) |>
      dplyr::slice(1) |>
      dplyr::pull(.data_label),
    expected = "49")

  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = a_1:a_9,
      percentage = TRUE,
      sort_by = ".count", desc = T) |>
      dplyr::slice(1) |>
      dplyr::pull(.count),
    expected = 60)

  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = b_1:b_3,
      percentage = TRUE,
      sort_by = "A bit", desc = FALSE) |>
      dplyr::slice(1) |>
      dplyr::pull(.count),
    expected = 55)

  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = b_1:b_3,
      percentage = TRUE,
      sort_by = c("A bit", "A lot"), desc = FALSE) |>
      dplyr::slice(9) |>
      dplyr::pull(.sum_value),
    expected = 60)

  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = b_1:b_3,
      percentage = TRUE,
      ignore_if_below = 10) |>
      dplyr::slice(3) |>
      dplyr::pull(.data_label),
    expected = "")

  testthat::expect_equal(saros:::summarize_data(
    data = ex_survey1,
    cols = a_1:a_9,
    percentage = FALSE, showNA = "no") |>
      dplyr::slice(1) |>
      dplyr::pull(.data_label),
    "49")

  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = a_1:a_9,
      percentage = FALSE,
      sort_by = ".count", desc = T) |>
      dplyr::slice(1) |>
      dplyr::pull(.count),
    expected = 60)


  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = a_1:a_9,
      by = x1_sex) |>
      dplyr::slice(20) |>
      dplyr::pull(.count),
    expected = 20)

  testthat::expect_equal(
    saros:::summarize_data(
      data = ex_survey1,
      cols = a_1:a_9,
      by = c(x1_sex:x2_human, f_uni)) |>
      dplyr::filter(.variable_name == "a_1",
                    x1_sex == "Males",
                    x2_human == "Robot?",
                    f_uni == "University of A", .category == "No") |>
      dplyr::pull(.proportion),
    expected = 0.25)


})


testthat::test_that("crosstable2 srvyr gives same output as regular tbl with 0 by-col", {
  suppressMessages(library(dplyr))
  suppressMessages(library(srvyr))
  x <-
    ex_survey1 %>%
    saros:::summarize_data(cols = matches("^b_"),
                                  sort_by = c("A lot", "A bit"),
                                  label_separator = " - ", percentage = TRUE)
  x_srv <-
    ex_survey1 %>%
    srvyr::as_survey(strata = f_uni) %>%
    saros:::summarize_data(cols = matches("^b_"),
                                  sort_by = c("A lot", "A bit"),
                                  label_separator = " - ", percentage = TRUE)



  testthat::expect_equal(object = x %>%
                           dplyr::slice(1) %>%
                           dplyr::pull(.data$.proportion),
                         expected = .55)

  testthat::expect_equal(object = names(x_srv),
                         expected = names(x))

  testthat::expect_equal(object = nrow(x_srv),
                         expected = nrow(x))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_name"]]),
                         expected = dplyr::pull(x, .data[[".variable_name"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".category"]]),
                         expected = dplyr::pull(x, .data[[".category"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".count"]]),
                         expected = dplyr::pull(x, .data[[".count"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_label"]]),
                         expected = dplyr::pull(x, .data[[".variable_label"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".data_label"]]),
                         expected = dplyr::pull(x, .data[[".data_label"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".proportion"]]),
                         expected = dplyr::pull(x, .data[[".proportion"]]),
                         tolerance = .0000000001)

  testthat::expect_equal(object = dplyr::pull(x, .data[[".proportion_se"]]),
                         expected = rep(NA_real_, nrow(x)))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".comb_categories"]]),
                         expected = dplyr::pull(x, .data[[".comb_categories"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".sum_value"]]),
                         expected = dplyr::pull(x, .data[[".sum_value"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".mean_base"]]),
                         expected = dplyr::pull(x, .data[[".mean_base"]]))
})

testthat::test_that("crosstable2 srvyr gives same output as regular tbl with 1 by-col", {
  suppressMessages(library(dplyr))
  suppressMessages(library(srvyr))
  x <-
    ex_survey1 %>%
    saros:::summarize_data(cols = matches("^b_"), by = x1_sex,
                                  sort_by = c("A lot", "A bit"),
                                  label_separator = " - ", percentage = TRUE)
  x_srv <-
    ex_survey1 %>%
    srvyr::as_survey(strata = f_uni) %>%
    saros:::summarize_data(cols = matches("^b_"), by = x1_sex,
                                  sort_by = c("A lot", "A bit"),
                                  label_separator = " - ", percentage = TRUE)

  testthat::expect_equal(object = names(x_srv),
                         expected = names(x))

  testthat::expect_equal(object = nrow(x_srv),
                         expected = nrow(x))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_name"]]),
                         expected = dplyr::pull(x, .data[[".variable_name"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[["x1_sex"]]),
                         expected = dplyr::pull(x, .data[["x1_sex"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".category"]]),
                         expected = dplyr::pull(x, .data[[".category"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".count"]]),
                         expected = dplyr::pull(x, .data[[".count"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_label"]]),
                         expected = dplyr::pull(x, .data[[".variable_label"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".data_label"]]),
                         expected = dplyr::pull(x, .data[[".data_label"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".proportion"]]),
                         expected = dplyr::pull(x, .data[[".proportion"]]))

  testthat::expect_equal(object = dplyr::pull(x, .data[[".proportion_se"]]),
                         expected = rep(NA_real_, nrow(x)))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".comb_categories"]]),
                         expected = dplyr::pull(x, .data[[".comb_categories"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".sum_value"]]),
                         expected = dplyr::pull(x, .data[[".sum_value"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".mean_base"]]),
                         expected = dplyr::pull(x, .data[[".mean_base"]]))
})


testthat::test_that("crosstable2 srvyr gives same output as regular tbl with 2 by-col", {
  suppressMessages(library(dplyr))
  suppressMessages(library(srvyr))
  x <-
    ex_survey1 %>%
    saros:::summarize_data(cols = matches("^b_"), by = c(x1_sex, x2_human),
                                  sort_by = c("A lot", "A bit"),
                                  label_separator = " - ", percentage = TRUE)
  x_srv <-
    ex_survey1 %>%
    srvyr::as_survey(strata = f_uni) %>%
    saros:::summarize_data(cols = matches("^b_"), by = c(x1_sex, x2_human),
                                  sort_by = c("A lot", "A bit"),
                                  label_separator = " - ", percentage = TRUE)

  testthat::expect_equal(object = names(x_srv),
                         expected = names(x))

  testthat::expect_equal(object = nrow(x_srv),
                         expected = nrow(x))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_name"]]),
                         expected = dplyr::pull(x, .data[[".variable_name"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[["x1_sex"]]),
                         expected = dplyr::pull(x, .data[["x1_sex"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[["x2_human"]]),
                         expected = dplyr::pull(x, .data[["x2_human"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".category"]]),
                         expected = dplyr::pull(x, .data[[".category"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".count"]]),
                         expected = dplyr::pull(x, .data[[".count"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_label"]]),
                         expected = dplyr::pull(x, .data[[".variable_label"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".data_label"]]),
                         expected = dplyr::pull(x, .data[[".data_label"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".proportion"]]),
                         expected = dplyr::pull(x, .data[[".proportion"]]))

  testthat::expect_equal(object = dplyr::pull(x, .data[[".proportion_se"]]),
                         expected = rep(NA_real_, nrow(x)))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".comb_categories"]]),
                         expected = dplyr::pull(x, .data[[".comb_categories"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".sum_value"]]),
                         expected = dplyr::pull(x, .data[[".sum_value"]]))

  testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".mean_base"]]),
                         expected = dplyr::pull(x, .data[[".mean_base"]]))
})


