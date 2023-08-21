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

testthat::test_that("check_string", {
  test_arg <- 5
  testthat::expect_error(
    object = saros:::check_string(test_arg),
    regexp = "`test_arg` must be a character vector of length 1, not a number"
  )

  test_arg <- "test"
  testthat::expect_no_error(
    object = saros:::check_string(test_arg)
  )

  test_arg <- NULL
  testthat::expect_no_error(
    object = saros:::check_string(test_arg, null.ok = TRUE)
  )

  test_arg <- NULL
  testthat::expect_error(
    object = saros:::check_string(test_arg, null.ok = FALSE),
    regexp = "`test_arg` must be a character vector of length 1, not NULL"
  )
})

testthat::test_that("check_list", {
  test_arg <- "d"
  testthat::expect_error(
    object = saros:::check_list(test_arg, null.ok = FALSE),
    regexp = "`test_arg` must be a list, not a string"
  )

  test_arg <- list(a = 1, b = 2)
  testthat::expect_no_error(
    object = saros:::check_list(test_arg)
  )

  test_arg <- NULL
  testthat::expect_error(
    object = saros:::check_list(test_arg, null.ok = FALSE),
    regexp = "`test_arg` must be a list, not NULL"
  )
  testthat::expect_no_error(
    object = saros:::check_list(test_arg, null.ok = TRUE))



  test_arg <- list(a = 1, b = 2)
  testthat::expect_no_error(
    object = saros:::check_list(test_arg, n = 2, null.ok = TRUE))
  testthat::expect_error(
    object = saros:::check_list(test_arg, n = 1, null.ok = TRUE),
    regexp = "must be a list of length 1, not")

})


testthat::test_that("check_colour", {
  test_arg <- "invalid"
  testthat::expect_error(
    object = saros:::check_colour(test_arg),
    regexp = "`test_arg` must be a character \\(hex colour code\\) of length 1, not a string"
  )

  test_arg <- "#FF5733"
    testthat::expect_no_error(
      object = saros:::check_colour(test_arg)
    )

    test_arg <- NULL
    testthat::expect_no_error(
      object = saros:::check_colour(test_arg)
    )

    test_arg <- c("#FF5733", "#C70039")
    testthat::expect_error(
      object = saros:::check_colour(test_arg),
      regexp = "`test_arg` must be a character \\(hex colour code\\) of length 1, not a character"
    )

    test_arg <- 42
    testthat::expect_no_error(
      object = saros:::check_colour(test_arg))
})

testthat::test_that("check_colours", {
  test_arg <- c("invalid1", "invalid2")
  testthat::expect_error(
    object = saros:::check_colours(test_arg),
    regexp = "`test_arg` must be a character vector \\(hex colours\\), not a character"
  )

  test_arg <- c("#FF5733", "#C70039")
  testthat::expect_no_error(
    object = saros:::check_colours(test_arg)
  )

  test_arg <- NULL
  testthat::expect_no_error(
    object = saros:::check_colours(test_arg)
  )

  test_arg <- c("#FF5733", "invalid")
  testthat::expect_error(
    object = saros:::check_colours(test_arg),
    regexp = "`test_arg` must be a character vector \\(hex colours\\), not a character"
  )

  test_arg <- c("#FF5733", 42)
  testthat::expect_no_error(
    object = saros:::check_colours(test_arg))

})

testthat::test_that("check_data_frame", {
  test_arg <- "not_a_data_frame"
  testthat::expect_error(
    object = saros:::check_data_frame(test_arg),
    regexp = "`test_arg` must be a data.frame, not a string"
  )

  test_arg <- data.frame(a = 1:3, b = 4:6)
  testthat::expect_no_error(
    object = saros:::check_data_frame(test_arg)
  )


  test_arg <- list(a = 1:3, b = 4:6)
  testthat::expect_error(
    object = saros:::check_data_frame(test_arg),
    regexp = "`test_arg` must be a data.frame, not a list"
  )

  test_arg <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  testthat::expect_error(
    object = saros:::check_data_frame(test_arg),
    regexp = "`test_arg` must be a data.frame, not a double matrix"
  )
})


testthat::test_that("check_autonum", {

  test_arg <- "not_a_run_autonum"
  testthat::expect_error(
    object = saros:::check_autonum(test_arg),
    regexp = "If not NULL, `test_arg` must be an object from .*, not a string"
  )

  test_arg <- officer::run_autonum()
  testthat::expect_no_error(
    object = saros:::check_autonum(test_arg)
  )

  test_arg <- NULL
  testthat::expect_no_error(
    object = saros:::check_autonum(test_arg)
  )
})

testthat::test_that("check_multiple_by", {

  data <- data.frame(a = 1:5, b = 6:10, c = 11:15, d = 16:20, e = 21:25)

  # Test 1: One column provided for 'by', expect no error
  testthat::expect_no_error(
    object = saros:::check_multiple_by(data, a)
  )

  # Test 2: Two columns provided for 'by', expect an error
  testthat::expect_error(
    object = saros:::check_multiple_by(data, c(a, b)),
    regexp = "Too many columns provided for `by`"
  )

  # Test 3: Empty data frame, expect no error here.
  data_no_col <- data.frame()
  testthat::expect_no_error(
    object = saros:::check_multiple_by(data_no_col, NULL)
  )

  # Test 4: Select no column, expect no error
  testthat::expect_no_error(
    object = saros:::check_multiple_by(data, NULL))

})


testthat::test_that("check_multiple_cols_and_one_by", {

  data <- data.frame(a = 1:5, b = 6:10, c = 11:15, d = 16:20, e = 21:25)

  # Test 1: One column for 'cols' and one column for 'by', expect no error
  testthat::expect_no_error(
    object = saros:::check_multiple_cols_and_one_by(data, a, b)
  )

  # Test 2: Two columns for 'cols' and one column for 'by', expect an error
  testthat::expect_error(
    object = saros:::check_multiple_cols_and_one_by(data, c(a, b), c),
    regexp = "Multiple columns for `cols` and `by` are not allowed.*You provided cols = \\^c\\(a, b\\)"
  )

})

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
