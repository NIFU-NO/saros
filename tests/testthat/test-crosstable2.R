testthat::test_that("crosstable2", {

  testthat::expect_no_error(object = {
    x <-
      ex_survey1 |>
      saros:::crosstable2.data.frame(
        cols = tidyselect::matches("b_"), by = NULL, showNA = "ifany")
  })
  testthat::expect_equal(dim(x), c(9,10))
  testthat::expect_equal(object =
                           x |>
                           dplyr::filter(.variable_name == "b_1", .category == "A bit") |>
                           dplyr::pull(.proportion),
                         expected = .37)

  testthat::expect_no_error(object = {
    x <-
      ex_survey1 |>
      saros:::crosstable2.data.frame(
        cols = tidyselect::matches("b_"), by = x1_sex, showNA = "ifany")
  })
  testthat::expect_equal(dim(x), c(18, 11))
  testthat::expect_equal(object =
                           x |>
                           dplyr::filter(.variable_name == "b_1",
                                         .category == "A lot",
                                         x1_sex == "Males") |>
                           dplyr::pull(.proportion),
                         expected = 0.0576923,
                         tolerance = .000001)

})
