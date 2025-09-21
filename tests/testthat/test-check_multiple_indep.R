testthat::test_that("check_multiple_indep", {
  data <- data.frame(a = 1:5, b = 6:10, c = 11:15, d = 16:20, e = 21:25)

  # Test 1: One column provided for 'indep', expect no error
  testthat::expect_no_error(
    object = saros:::check_multiple_indep(data, a)
  )

  # Test 2: Two columns provided for 'indep', expect an error
  testthat::expect_error(
    object = saros:::check_multiple_indep(data, c(a, b)),
    regexp = "Too many columns provided for `indep`"
  )

  # Test 3: Empty data frame, expect no error here.
  data_no_col <- data.frame()
  testthat::expect_no_error(
    object = saros:::check_multiple_indep(data_no_col, NULL)
  )

  # Test 4: Select no column, expect no error
  testthat::expect_no_error(
    object = saros:::check_multiple_indep(data, NULL)
  )
})
