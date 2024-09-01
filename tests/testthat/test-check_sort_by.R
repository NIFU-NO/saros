testthat::test_that("check_sort_by tests", {
  # Test 1: Valid single set option
  sort_by_test_1 <- ".upper"
  testthat::expect_no_error(
    saros:::check_sort_by(x = c("Not at all", "A bit", "A lot"),
                               sort_by = sort_by_test_1))

  # Test 2: Valid all categories in the data frame
  sort_by_test_2 <- c("Not at all", "A bit", "A lot")
  testthat::expect_no_error(
    saros:::check_sort_by(x = c("Not at all", "A bit", "A lot"),
                               sort_by = sort_by_test_2))

  # Test 2: Valid all categories in the data frame
  sort_by_test_2 <- c("Not at all", "A bit")
  testthat::expect_no_error(
    saros:::check_sort_by(x = c("Not at all", "A bit", "A lot"),
                               sort_by = sort_by_test_2))

  # Test 2: Error on not all categories in the data frame
  sort_by_test_2 <- c("Not at all", "A bit", "A lot")
  testthat::expect_error(
    saros:::check_sort_by(x = c("Not at all", "A bit"),
                               sort_by = sort_by_test_2))


  # Test 3: Invalid single set option
  sort_by_test_3 <- ".invalid_option"
  testthat::expect_error(
    saros:::check_sort_by(x = c("Not at all", "A bit", "A lot"),
                               sort_by = sort_by_test_3))

  # Test 4: Invalid character vector with mixed valid and invalid categories
  sort_by_test_4 <- c("Not at all", "A bit", "Invalid")
  testthat::expect_error(
    saros:::check_sort_by(x = c("Not at all", "A bit", "A lot"),
                               sort_by = sort_by_test_4))

  # Test 5: NULL sort_by (default)
  sort_by_test_5 <- NULL
  testthat::expect_no_error(
    saros:::check_sort_by(x = c("Not at all", "A bit", "A lot"),
                               sort_by = sort_by_test_5))
})
