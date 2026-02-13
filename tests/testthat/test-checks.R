testthat::test_that("check_bool", {
  test_arg <- "d"
  testthat::expect_error(
    object = saros:::check_bool(test_arg),
    regexp = "`test_arg` must be a logical of length 1, not a string"
  )
  test_arg <- TRUE
  testthat::expect_no_error(
    object = saros:::check_bool(test_arg)
  )
})

testthat::test_that("check_integerish", {
  test_arg <- "d"
  testthat::expect_error(
    object = saros:::check_integerish(test_arg),
    regexp = "`test_arg` must be an integer of length 1, not a string"
  )

  test_arg <- -2
  testthat::expect_error(
    object = saros:::check_integerish(test_arg, min = 0),
    regexp = "`test_arg` must be a positive integer of length 1, not a number"
  )

  test_arg <- 2L
  testthat::expect_no_error(
    object = saros:::check_integerish(test_arg)
  )

  test_arg <- 2.0
  testthat::expect_no_error(
    object = saros:::check_integerish(test_arg)
  )

  test_arg <- 10
  testthat::expect_no_error(
    object = saros:::check_integerish(test_arg, min = 0)
  )

  test_arg <- 10
  testthat::expect_error(
    object = saros:::check_integerish(test_arg, min = 0, max = 8),
    regexp = "`test_arg` must be a positive integer of length 1 \\(max=8\\), not a number"
  )
})


testthat::test_that("check_double", {
  test_arg <- "d"
  testthat::expect_error(
    object = saros:::check_double(test_arg),
    regexp = "`test_arg` must be a numeric of length 1, not a string"
  )

  test_arg <- -2.5
  testthat::expect_error(
    object = saros:::check_double(test_arg, min = 0),
    regexp = "`test_arg` must be a positive numeric of length 1 \\(min=0\\), not a number"
  )

  test_arg <- 2.5
  testthat::expect_no_error(
    object = saros:::check_double(test_arg)
  )
})

testthat::test_that("validate_string", {
  # Test with valid string
  test_arg <- "valid_string"
  testthat::expect_no_error(
    object = saros:::validate_string(test_arg)
  )

  # Test with invalid input (not a string)
  test_arg <- 123
  testthat::expect_error(
    object = saros:::validate_string(test_arg),
    regexp = "`test_arg` must be a string"
  )

  # Test with character vector (length > 1)
  test_arg <- c("a", "b")
  testthat::expect_error(
    object = saros:::validate_string(test_arg),
    regexp = "`test_arg` must be a string"
  )

  # Test with NULL when null_allowed = FALSE
  test_arg <- NULL
  testthat::expect_error(
    object = saros:::validate_string(test_arg, null_allowed = FALSE),
    regexp = "`test_arg` must be a string"
  )

  # Test with NULL when null_allowed = TRUE
  test_arg <- NULL
  testthat::expect_no_error(
    object = saros:::validate_string(test_arg, null_allowed = TRUE)
  )

  # Test backwards compatibility alias
  test_arg <- "test"
  testthat::expect_no_error(
    object = saros:::check_string(test_arg)
  )
})

testthat::test_that("validate_params", {
  # Test with valid integerish parameters
  params <- list(n = 10, x = 5)
  spec <- list(
    n = list(type = "integerish"),
    x = list(type = "integerish", min = 0, max = 10)
  )
  testthat::expect_no_error(
    saros:::validate_params(params, spec)
  )

  # Test with invalid integerish (out of range)
  params <- list(x = 15)
  spec <- list(x = list(type = "integerish", max = 10))
  testthat::expect_error(
    saros:::validate_params(params, spec),
    regexp = "`x` must be"
  )

  # Test with valid double parameters
  params <- list(ratio = 0.5)
  spec <- list(ratio = list(type = "double", min = 0, max = 1))
  testthat::expect_no_error(
    saros:::validate_params(params, spec)
  )

  # Test with invalid double (out of range)
  params <- list(ratio = 1.5)
  spec <- list(ratio = list(type = "double", min = 0, max = 1))
  testthat::expect_error(
    saros:::validate_params(params, spec),
    regexp = "`ratio` must be"
  )

  # Test with valid bool parameter
  params <- list(flag = TRUE)
  spec <- list(flag = list(type = "bool"))
  testthat::expect_no_error(
    saros:::validate_params(params, spec)
  )

  # Test with invalid bool parameter
  params <- list(flag = "yes")
  spec <- list(flag = list(type = "bool"))
  testthat::expect_error(
    saros:::validate_params(params, spec),
    regexp = "`flag` must be a logical"
  )

  # Test with valid string parameter
  params <- list(name = "test")
  spec <- list(name = list(type = "string"))
  testthat::expect_no_error(
    saros:::validate_params(params, spec)
  )

  # Test with invalid string parameter
  params <- list(name = 123)
  spec <- list(name = list(type = "string"))
  testthat::expect_error(
    saros:::validate_params(params, spec),
    regexp = "`name` must be a string"
  )

  # Test with NULL when null_allowed = TRUE
  params <- list(optional = NULL)
  spec <- list(optional = list(type = "string", null_allowed = TRUE))
  testthat::expect_no_error(
    saros:::validate_params(params, spec)
  )

  # Test with NULL when null_allowed = FALSE (default)
  params <- list(required = NULL)
  spec <- list(required = list(type = "string"))
  testthat::expect_error(
    saros:::validate_params(params, spec),
    regexp = "`required` must be a string"
  )

  # Test with missing parameter (should be skipped)
  params <- list(x = 5)
  spec <- list(x = list(type = "integerish"), y = list(type = "integerish"))
  testthat::expect_no_error(
    saros:::validate_params(params, spec)
  )

  # Test with unknown validation type
  params <- list(x = 5)
  spec <- list(x = list(type = "unknown"))
  testthat::expect_error(
    saros:::validate_params(params, spec),
    regexp = "Unknown validation type"
  )

  # Test with multiple parameters
  params <- list(
    count = 10,
    ratio = 0.5,
    enabled = TRUE,
    label = "test"
  )
  spec <- list(
    count = list(type = "integerish", min = 0),
    ratio = list(type = "double", min = 0, max = 1),
    enabled = list(type = "bool"),
    label = list(type = "string")
  )
  testthat::expect_no_error(
    saros:::validate_params(params, spec)
  )
})
