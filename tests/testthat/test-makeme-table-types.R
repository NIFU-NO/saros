test_that("makeme cat_table_html basic functionality", {
  data("ex_survey", package = "saros")

  # Basic cat_table_html test with exact expected values for a_1
  result <- makeme(
    data = ex_survey,
    dep = a_1,
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 1)
  expect_equal(result$`No (%)`, "55")
  expect_equal(result$`Yes (%)`, "36")
  expect_equal(result$`NA (%)`, "8")
  expect_equal(result$`Total (N)`, 300)
})

test_that("makeme cat_table_html with count data_label", {
  data("ex_survey", package = "saros")

  # Test count option - should give actual counts instead of percentages
  result <- makeme(
    data = ex_survey,
    dep = a_1,
    type = "cat_table_html",
    data_label = "count"
  )

  expect_true(is.data.frame(result))
  expect_equal(result$`Total (N)`, 300)
  # With count, column names are just the category names without (count) suffix
  expect_equal(result$No, "166")
  expect_equal(result$Yes, "109")
  expect_equal(result$`NA`, "25")
})

test_that("makeme cat_table_html with independent variable", {
  data("ex_survey", package = "saros")

  # Test with independent variable - should show breakdown by gender
  result <- makeme(
    data = ex_survey,
    dep = a_1,
    indep = x1_sex,
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5) # Gender + No (%) + Yes (%) + NA (%) + Total (N)
  expect_equal(nrow(result), 2) # Males and Females
  expect_equal(as.character(result$Gender), c("Males", "Females"))
  expect_equal(result$`Total (N)`, c(151, 149))
  expect_equal(result$`No (%)`[1], "56") # Males
  expect_equal(result$`No (%)`[2], "54") # Females
})

test_that("makeme cat_table_html with multiple variables", {
  data("ex_survey", package = "saros")

  # Test with multiple dependent variables
  result <- makeme(
    data = ex_survey,
    dep = c(a_1, a_2),
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5) # .variable_label + No (%) + Yes (%) + NA (%) + Total (N)
  expect_equal(nrow(result), 2) # Two variables: a_1 and a_2
  expect_true(all(result$`Total (N)` == 300))

  # Check that a_1 values are present (second row due to ordering)
  expect_equal(result$`No (%)`[2], "55") # a_1
  expect_equal(result$`Yes (%)`[2], "36") # a_1
  expect_equal(result$`NA (%)`[2], "8") # a_1
})

test_that("makeme int_table_html basic functionality", {
  data("ex_survey", package = "saros")

  # Test with c_1 (Company A experience) - exact expected values
  result <- makeme(
    data = ex_survey,
    dep = c_1,
    type = "int_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 11)
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$.variable_label), "Company A")
  expect_equal(result$N, 300)
  expect_equal(result$N_valid, 300)
  expect_equal(result$N_missing, 0)
  expect_equal(result$Mean, 21, tolerance = 0.1)
  expect_equal(result$SD, 5, tolerance = 0.1)
  expect_equal(result$Median, 21, tolerance = 0.1)
  expect_equal(result$Min, 8, tolerance = 0.1)
  expect_equal(result$Max, 32, tolerance = 0.1)
})

test_that("makeme int_table_html with independent variable", {
  data("ex_survey", package = "saros")

  # Test with independent variable - should show stats by gender
  result <- makeme(
    data = ex_survey,
    dep = c_1,
    indep = x1_sex,
    type = "int_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2) # Males and Females
  expect_equal(ncol(result), 12) # Additional Gender column
  expect_true("Gender" %in% colnames(result))
  expect_equal(as.character(result$Gender), c("Males", "Females"))
  # Should have valid sample sizes for both groups
  expect_true(all(result$N > 140))
  expect_true(all(result$N < 160))
})

test_that("makeme int_table_html with multiple variables", {
  data("ex_survey", package = "saros")

  # Test with multiple numeric variables
  result <- makeme(
    data = ex_survey,
    dep = c(c_1, c_2),
    type = "int_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2) # Two variables: c_1 and c_2
  expect_equal(ncol(result), 11) # Same column structure
  expect_true(all(result$N == 300))
  expect_true(all(result$N_valid == 300))
  expect_true(all(result$N_missing == 0))
})

test_that("makeme handles error conditions", {
  data("ex_survey", package = "saros")

  # Test with invalid type
  expect_error(
    makeme(
      data = ex_survey,
      dep = a_1,
      type = "invalid_type"
    ),
    "Invalid make_content-type"
  )
})

test_that("makeme handles combination of valid and all-NA variables correctly", {
  data("ex_survey", package = "saros")

  # Create test data with an all-NA variable that has same levels as existing variable
  test_data <- ex_survey
  test_data$all_na_var <- factor(NA, levels = c("No", "Yes"))
  attr(test_data$all_na_var, "label") <- "All NA Test Variable"

  # This should work: the all-NA variable should be filtered out by default hide settings
  expect_no_error({
    result <- makeme(
      data = test_data,
      dep = c(a_1, all_na_var),
      type = "cat_table_html"
    )
  })

  # The result should only contain a_1 since all_na_var should be hidden
  result <- makeme(
    data = test_data,
    dep = c(a_1, all_na_var),
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1) # Only a_1 should remain
  expect_equal(result$`Total (N)`, 300) # Should have all observations from a_1
})

test_that("makeme with all-NA variable works when hiding is disabled", {
  data("ex_survey", package = "saros")

  test_data <- ex_survey[1:50, ] # Smaller dataset for testing
  test_data$all_na_var <- factor(NA, levels = c("No", "Yes"))
  attr(test_data$all_na_var, "label") <- "All NA Test Variable"

  # When explicitly allowing all-NA variables, this should work without error
  # Note: The all-NA variable might still be filtered by other criteria
  expect_no_error({
    result <- makeme(
      data = test_data,
      dep = c(a_1, all_na_var),
      type = "cat_table_html",
      hide_for_crowd_if_all_na = FALSE,
      hide_for_crowd_if_category_k_below = 0, # Disable category count filtering
      hide_for_crowd_if_valid_n_below = 0 # Disable valid count filtering
    )
  })

  result <- makeme(
    data = test_data,
    dep = c(a_1, all_na_var),
    type = "cat_table_html",
    hide_for_crowd_if_all_na = FALSE,
    hide_for_crowd_if_category_k_below = 0,
    hide_for_crowd_if_valid_n_below = 0
  )

  expect_true(is.data.frame(result))
  # The key test is that it doesn't error - the exact number of rows may vary
  # based on other filtering criteria
  expect_true(nrow(result) >= 1) # At least a_1 should be present
})

test_that("makeme mixed type error still works for truly mixed types", {
  data("ex_survey", package = "saros")

  # This should still error because a_1 is factor and c_1 is numeric
  expect_error(
    makeme(
      data = ex_survey,
      dep = c(a_1, c_1),
      type = "cat_table_html"
    ),
    "Unequal variables|mix of categorical and continuous"
  )
})

test_that("makeme works with multiple all-NA variables", {
  data("ex_survey", package = "saros")

  test_data <- ex_survey
  test_data$all_na_var1 <- factor(NA, levels = c("No", "Yes"))
  test_data$all_na_var2 <- factor(NA, levels = c("No", "Yes"))
  attr(test_data$all_na_var1, "label") <- "All NA Variable 1"
  attr(test_data$all_na_var2, "label") <- "All NA Variable 2"

  # Should work and include only the valid variables
  expect_no_error({
    result <- makeme(
      data = test_data,
      dep = c(a_1, all_na_var1, a_2, all_na_var2),
      type = "cat_table_html"
    )
  })

  result <- makeme(
    data = test_data,
    dep = c(a_1, all_na_var1, a_2, all_na_var2),
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2) # Only a_1 and a_2 should remain
  expect_true(all(result$`Total (N)` == 300))
})

test_that("makeme type checking uses filtered variables for integer types", {
  data("ex_survey", package = "saros")

  test_data <- ex_survey
  # Use c_1 and c_2 which are both numeric variables from ex_survey
  # This test verifies that the type checking logic works correctly for numeric variables

  expect_no_error({
    result <- makeme(
      data = test_data,
      dep = c(c_1, c_2),
      type = "int_table_html"
    )
  })

  result <- makeme(
    data = test_data,
    dep = c(c_1, c_2),
    type = "int_table_html"
  )

  expect_true(is.data.frame(result))
  # Should have both c_1 and c_2 (2 rows for 2 variables)
  expect_equal(nrow(result), 2)
  # Check that standard columns are present
  expect_true(all(c("N", "Mean", "SD") %in% colnames(result)))
})
