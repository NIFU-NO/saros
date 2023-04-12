test_that("create_static_text_ordinal", {
  #  Test 1: Check if the function returns a list
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"))
    testthat::expect_true(is.list(result))

#  Test 2: Check if the intro content is correct
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"), contents = "intro")
    testthat::expect_true("intro" %in% names(result))

#  Test 3: Check if the mode_max content is correct
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"), contents = "mode_max")
    testthat::expect_true("mode_max" %in% names(result))

 # Test 4: Check if the not_used_category content is correct
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"), contents = "not_used_category", ignore_if_below = 30)
    testthat::expect_true("not_used_category" %in% names(result))

  #Test 5: Check if the value_max content is correct
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"), contents = "value_max")
    testthat::expect_true("value_max" %in% names(result))

  #Test 6: Check if the value_min content is correct
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"), contents = "value_min")
    testthat::expect_true("value_min" %in% names(result))

  #Test 7: Check if the mean_max content is correct
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"), contents = "mean_max")
    testthat::expect_true("mean_max" %in% names(result))

# Test 8: Check if the mean_min content is correct
    result <- create_static_text_ordinal(ex_survey1, cols = starts_with("e_"), contents = "mean_min")
    testthat::expect_true("mean_min" %in% names(result))

# Test 9: Test when sort_by is NULL (default value)
  result <- create_static_text_ordinal(data = ex_survey1, cols = matches("e_"), percentage = TRUE, contents = "value_max")
  testthat::expect_true(!is.null(result))

# Test 10: Test when sort_by is a character vector
  result <- create_static_text_ordinal(data = ex_survey1, cols = matches("a_"), percentage = TRUE, contents = "value_max", sort_by = c("Yes", "No"))
  testthat::expect_true(!is.null(result))

# Test 11: Test when sort_by is a character vector in a different order
  result <- create_static_text_ordinal(data = ex_survey1, cols = matches("a_"), percentage = TRUE, contents = "value_max", sort_by = c("No", "Yes"))
  testthat::expect_true(!is.null(result))

# Test 12: Test when sort_by is a character vector with invalid values
  testthat::expect_error(create_static_text_ordinal(data = ex_survey1, cols = matches("e_"), percentage = TRUE, contents = "value_max", sort_by = c("Invalid1", "Invalid2")))

# Test 13: Test when sort_by is an integer
  testthat::expect_error(create_static_text_ordinal(data = ex_survey1, cols = matches("e_"), percentage = TRUE, contents = "value_max", sort_by = 1))

# Test 14: Test when sort_by is a negative integer
  testthat::expect_error(create_static_text_ordinal(data = ex_survey1, cols = matches("e_"), percentage = TRUE, contents = "value_max", sort_by = -1))

# Test 15: Test when sort_by is an integer larger than the number of unique categories
  testthat::expect_error(create_static_text_ordinal(data = ex_survey1, cols = matches("e_"), percentage = TRUE, contents = "value_max", sort_by = 5))

# Test 16: Test when sort_by is a numeric value (not an integer)
  testthat::expect_error(create_static_text_ordinal(data = ex_survey1, cols = matches("e_"), percentage = TRUE, contents = "value_max", sort_by = 1.5))
})
