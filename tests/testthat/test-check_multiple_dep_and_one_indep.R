
testthat::test_that("check_multiple_dep_and_one_indep", {

  data <- data.frame(a = 1:5, b = 6:10, c = 11:15, d = 16:20, e = 21:25)

  # Test 1: One column for 'dep' and one column for 'indep', expect no error
  testthat::expect_no_error(
    object = saros:::check_multiple_dep_and_one_indep(data, a, b)
  )

  # Test 2: Two columns for 'dep' and one column for 'indep', expect an error
  testthat::expect_error(
    object = saros:::check_multiple_dep_and_one_indep(data, c(a, b), c),
    regexp = "Multiple columns for `dep` and `indep` are not allowed.*You provided dep = \\^c\\(a, b\\)"
  )

})
