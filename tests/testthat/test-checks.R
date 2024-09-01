testthat::test_that("check_bool", {
  test_arg <- "d"
  testthat::expect_error(
    object = saros:::check_bool(test_arg),
    regexp = "`test_arg` must be a logical of length 1, not a string"
  )
  test_arg <- TRUE
  testthat::expect_no_error(
    object = saros:::check_bool(test_arg))
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
    regexp = "`test_arg` must be a positive integer of length 1, not a number")

  test_arg <- 2L
  testthat::expect_no_error(
    object = saros:::check_integerish(test_arg))

  test_arg <- 2.0
  testthat::expect_no_error(
    object = saros:::check_integerish(test_arg))


  test_arg <- 10
  testthat::expect_no_error(
    object = saros:::check_integerish(test_arg, min = 0))


  test_arg <- 10
  testthat::expect_error(
    object = saros:::check_integerish(test_arg, min = 0, max = 8),
    regexp = "`test_arg` must be a positive integer of length 1 \\(max=8\\), not a number")
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



