testthat::test_that("crosstable", {
library(saros)
  testthat::expect_no_error(object = {
    x <-
      ex_survey |>
      saros:::crosstable.data.frame(
        dep = paste0("b_", 1:3),
        indep = NULL, showNA = "ifany")
  })
  testthat::expect_equal(dim(x), c(9,10))
  testthat::expect_equal(object =
                           x |>
                           dplyr::filter(.variable_name == "b_1", .category == "A bit") |>
                           dplyr::pull(.proportion),
                         expected = 0.4766667,
                         tolerance = .00001)

  testthat::expect_no_error(object = {
    x <-
      ex_survey |>
      saros:::crosstable.data.frame(
        dep = paste0("b_", 1:3),
        indep = "x1_sex",
        showNA = "ifany")
  })
  testthat::expect_equal(dim(x), c(18, 11))
  testthat::expect_equal(object =
                           x |>
                           dplyr::filter(.variable_name == "b_1",
                                         .category == "A lot",
                                         x1_sex == "Males") |>
                           dplyr::pull(.proportion),
                         expected = 0.1059603,
                         tolerance = .0001)

})


testthat::test_that("crosstable works with explicit NA level", {
  library(saros)

  testthat::expect_no_error(object = {
    x <-
      ex_survey |>
      dplyr::mutate(dplyr::across(p_1:p_4, ~forcats::fct_na_value_to_level(.x))) |>
      saros:::crosstable.data.frame(dep = paste0("p_", 1:4),
                                             indep = "x1_sex", showNA = "ifany")
  })
  testthat::expect_equal(dim(x), c(34,11))
})


