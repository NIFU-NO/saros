testthat::test_that("check_options uses explicit arguments", {
  makeme_mock <- function(digits = 1, data_label = "count", showNA = "never") {}
  env <- new.env()
  env$makeme_mock_defaults <- list()
  env$ignore_args <- character(0)

  call <- match.call(
    makeme_mock,
    call = quote(makeme_mock(digits = 3, data_label = "proportion"))
  )
  result <- saros:::check_options(
    call,
    defaults_env = env$makeme_mock_defaults,
    default_values = formals(makeme_mock),
    ignore_args = env$ignore_args
  )

  testthat::expect_equal(result$digits, 3)
  testthat::expect_equal(result$data_label, "proportion")
  testthat::expect_equal(result$showNA, c("never"))
})


testthat::test_that("check_options uses defaults_env if argument not explicit", {
  makeme_mock <- function(digits = 1, data_label = "count", showNA = "never") {}
  env <- new.env()
  env$makeme_mock_defaults <- list(digits = 3, data_label = "proportion")
  env$ignore_args <- character(0)

  call <- match.call(makeme_mock, call = quote(makeme_mock()))
  result <- saros:::check_options(
    call,
    defaults_env = env$makeme_mock_defaults,
    default_values = formals(makeme_mock),
    ignore_args = env$ignore_args
  )

  testthat::expect_equal(result$digits, 3)
  testthat::expect_equal(result$data_label, "proportion")
  testthat::expect_equal(result$showNA, c("never"))
})

testthat::test_that("check_options uses function argument formals if neither explicit nor defaults_env", {
  makeme_mock <- function(digits = 1, data_label = "count", showNA = "never") {}
  env <- new.env()
  env$makeme_mock_defaults <- list()
  env$ignore_args <- character(0)

  call <- match.call(makeme_mock, call = quote(makeme_mock()))
  result <- saros:::check_options(
    call,
    defaults_env = env$makeme_mock_defaults,
    default_values = formals(makeme_mock),
    ignore_args = env$ignore_args
  )

  testthat::expect_equal(result$digits, 1)
  testthat::expect_equal(result$data_label, "count")
  testthat::expect_equal(result$showNA, c("never"))
})

testthat::test_that("check_options ignores arguments in ignore_args", {
  makeme_mock <- function(digits = 1, data_label = "count", showNA = "never") {}
  env <- new.env()
  env$makeme_mock_defaults <- list(showNA = "always")
  env$ignore_args <- c("showNA")

  call <- match.call(
    makeme_mock,
    call = quote(makeme_mock(digits = 3, showNA = "proveit"))
  )
  result <- saros:::check_options(
    call,
    defaults_env = env$makeme_mock_defaults,
    default_values = formals(makeme_mock),
    ignore_args = env$ignore_args
  )

  testthat::expect_equal(result$digits, 3)
  testthat::expect_equal(result$data_label, "count")
  testthat::expect_true(is.null(result$showNA))
})
