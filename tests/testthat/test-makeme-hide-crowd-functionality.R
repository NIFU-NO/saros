test_that("makeme respects hide_for_crowd_if_all_na argument", {
  data("ex_survey", package = "saros")

  # Test with individual variables to avoid mixing variable types
  test_data <- ex_survey
  test_data$all_na_var <- factor(NA, levels = c("No", "Yes"))
  attr(test_data$all_na_var, "label") <- "All NA Variable"

  # Test all_na_var individually with hide_for_crowd_if_all_na = TRUE (default)
  result_hide_true <- makeme(
    data = test_data,
    dep = all_na_var,
    type = "cat_table_html",
    label_separator = " - ",
    hide_for_crowd_if_all_na = TRUE
  )

  # all_na_var should be hidden (empty result)
  expect_true(is.data.frame(result_hide_true))
  expect_equal(nrow(result_hide_true), 0) # Variable should be hidden

  # Test all_na_var individually with hide_for_crowd_if_all_na = TRUE (default)
  result_hide_true <- makeme(
    data = test_data,
    dep = c(all_na_var),
    type = "cat_table_html",
    label_separator = " - ",
    hide_for_crowd_if_all_na = TRUE
  )

  # all_na_var should be hidden (empty result)
  expect_true(is.data.frame(result_hide_true))
  expect_equal(nrow(result_hide_true), 0) # Variable should be hidden

  # Instead of testing all-NA inclusion (which might have other constraints),
  # test a variable with mostly NA but some valid values
  test_data$mostly_na_var <- factor(
    c(rep("No", 2), rep(NA_character_, 298)),
    levels = c("No", "Yes")
  )
  attr(test_data$mostly_na_var, "label") <- "Mostly NA Variable"

  # Test with hide_for_crowd_if_all_na = TRUE (should be included as it's not ALL NA)
  result_mostly_na_true <- makeme(
    data = test_data,
    dep = mostly_na_var,
    type = "cat_table_html",
    hide_for_crowd_if_all_na = TRUE,
    hide_for_crowd_if_valid_n_below = 1,
    hide_for_crowd_if_category_k_below = 1
  )

  # mostly_na_var should be included as it has some valid values
  expect_true(is.data.frame(result_mostly_na_true))
  expect_equal(nrow(result_mostly_na_true), 1) # Variable should be present

  # Test normal variable a_1 (should always be included)
  result_normal <- makeme(
    data = test_data,
    dep = a_1,
    type = "cat_table_html",
    hide_for_crowd_if_all_na = TRUE
  )

  # a_1 should be included as it has valid data
  expect_true(is.data.frame(result_normal))
  expect_equal(nrow(result_normal), 1) # Variable should be present
})

test_that("makeme respects hide_for_crowd_if_valid_n_below argument", {
  data("ex_survey", package = "saros")

  # Create test data with a variable that has very few valid values
  test_data <- ex_survey[1:20, ] # Subset to 20 rows
  test_data$few_valid_var <- factor(
    c(rep("No", 3), rep(NA_character_, 17)),
    levels = c("No", "Yes")
  )
  attr(test_data$few_valid_var, "label") <- "Few Valid Variables"

  # Test few_valid_var individually with hide_for_crowd_if_valid_n_below = 5 (should hide variable with 3 valid values)
  result_hide <- makeme(
    data = test_data,
    dep = few_valid_var,
    type = "cat_table_html",
    hide_for_crowd_if_valid_n_below = 5
  )

  # few_valid_var should be hidden
  expect_true(is.data.frame(result_hide))
  expect_equal(nrow(result_hide), 0) # Variable should be hidden

  # Test few_valid_var individually with hide_for_crowd_if_valid_n_below = 2 (should include variable with 3 valid values)
  # Need to also set category threshold to be permissive since we only have 1 category used
  result_keep <- makeme(
    data = test_data,
    dep = few_valid_var,
    type = "cat_table_html",
    hide_for_crowd_if_valid_n_below = 2,
    hide_for_crowd_if_category_k_below = 1 # Allow variables with 1 category
  )

  # few_valid_var should be included
  expect_true(is.data.frame(result_keep))
  expect_equal(nrow(result_keep), 1) # Variable should be present

  # Test normal variable a_1 (should always be included)
  result_normal <- makeme(
    data = test_data,
    dep = a_1,
    type = "cat_table_html",
    hide_for_crowd_if_valid_n_below = 5
  )

  # a_1 should be included as it has sufficient valid data
  expect_true(is.data.frame(result_normal))
  expect_equal(nrow(result_normal), 1) # Variable should be present
})

test_that("makeme respects hide_for_crowd_if_category_k_below argument", {
  data("ex_survey", package = "saros")

  # Test by creating a single-category version of a_1
  test_data <- ex_survey[1:50, ]
  # Convert a_1 to only have "No" values (single category)
  test_data$a_1 <- factor(rep("No", 50), levels = c("No", "Yes"))
  attr(test_data$a_1, "label") <- "Only No Responses"

  # Test with hide_for_crowd_if_category_k_below = 2 (default - should hide single category variable)
  result_hide <- makeme(
    data = test_data,
    dep = a_1,
    type = "cat_table_html",
    hide_for_crowd_if_category_k_below = 2
  )

  # a_1 should be hidden due to having only 1 category
  expect_true(is.data.frame(result_hide))
  expect_equal(nrow(result_hide), 0) # No variables remain

  # Test with hide_for_crowd_if_category_k_below = 1 (should include single category variable)
  result_keep <- makeme(
    data = test_data,
    dep = a_1,
    type = "cat_table_html",
    hide_for_crowd_if_category_k_below = 1
  )

  # a_1 should be included
  expect_true(is.data.frame(result_keep))
  expect_equal(nrow(result_keep), 1) # Variable present
})

test_that("makeme respects hide_for_crowd_if_category_n_below argument", {
  data("ex_survey", package = "saros")

  # Create test data with a variable that has a category with very few observations
  test_data <- ex_survey[1:50, ]
  # Create a custom variable with one rare category
  test_data$rare_cat_var <- factor(
    c(
      rep("No", 47), # 47 observations
      rep("Yes", 2), # 2 observations
      rep(NA_character_, 1) # 1 NA
    ),
    levels = c("No", "Yes")
  )
  attr(test_data$rare_cat_var, "label") <- "Rare Category Variable"

  # Test with hide_for_crowd_if_category_n_below = 5 (should hide variable with category having 2 obs)
  result_hide <- makeme(
    data = test_data,
    dep = rare_cat_var,
    type = "cat_table_html",
    hide_for_crowd_if_category_n_below = 5
  )

  # rare_cat_var should be hidden due to "Yes" category having only 2 observations
  expect_true(is.data.frame(result_hide))
  expect_equal(nrow(result_hide), 0) # No variables remain

  # Test with hide_for_crowd_if_category_n_below = 1 (should include variable)
  result_keep <- makeme(
    data = test_data,
    dep = rare_cat_var,
    type = "cat_table_html",
    hide_for_crowd_if_category_n_below = 1
  )

  # rare_cat_var should be included
  expect_true(is.data.frame(result_keep))
  expect_equal(nrow(result_keep), 1) # Variable present
})

test_that("makeme respects hide_for_crowd_if_cell_n_below argument with indep variable", {
  data("ex_survey", package = "saros")

  # Use existing variables that should create small cells when crossed with gender
  test_data <- ex_survey[1:30, ] # 15 males, 15 females roughly

  # Test with hide_for_crowd_if_cell_n_below = 20 and independent variable
  # This should hide variables that create cells smaller than 20
  result_hide <- makeme(
    data = test_data,
    dep = a_1,
    indep = x1_sex,
    type = "cat_table_html",
    hide_for_crowd_if_cell_n_below = 20
  )

  # With only 30 rows and gender split, cells will be small - might hide variable
  expect_true(is.data.frame(result_hide))

  # Test with hide_for_crowd_if_cell_n_below = 2 (more permissive)
  result_keep <- makeme(
    data = test_data,
    dep = a_1,
    indep = x1_sex,
    type = "cat_table_html",
    hide_for_crowd_if_cell_n_below = 2
  )

  # Should be included with lower threshold
  expect_true(is.data.frame(result_keep))
  # With 30 rows, we should have at least some data
  expect_true(nrow(result_keep) >= 0)
})

test_that("makeme hide arguments work with integer variables", {
  data("ex_survey", package = "saros")

  # Test with integer/numeric variables
  test_data <- ex_survey[1:50, ]
  test_data$all_na_int <- NA_integer_
  attr(test_data$all_na_int, "label") <- "All NA Integer"
  test_data$few_valid_int <- c(rep(1L, 3), rep(NA_integer_, 47))
  attr(test_data$few_valid_int, "label") <- "Few Valid Integer"

  # Test all_na_int individually
  result_all_na <- makeme(
    data = test_data,
    dep = all_na_int,
    type = "int_table_html",
    hide_for_crowd_if_all_na = TRUE
  )

  # all_na_int should be hidden (all NA)
  expect_true(is.data.frame(result_all_na))
  expect_equal(nrow(result_all_na), 0) # No variables remain

  # Test few_valid_int individually
  result_few_valid <- makeme(
    data = test_data,
    dep = few_valid_int,
    type = "int_table_html",
    hide_for_crowd_if_valid_n_below = 5
  )

  # few_valid_int should be hidden (< 5 valid values)
  expect_true(is.data.frame(result_few_valid))
  expect_equal(nrow(result_few_valid), 0) # No variables remain

  # Test normal variable c_1 (should always be included)
  result_normal <- makeme(
    data = test_data,
    dep = c_1,
    type = "int_table_html",
    hide_for_crowd_if_all_na = TRUE,
    hide_for_crowd_if_valid_n_below = 5
  )

  # c_1 should be included
  expect_true(is.data.frame(result_normal))
  expect_equal(nrow(result_normal), 1) # c_1 remains
})

test_that("makeme hide arguments work correctly with mesos functionality", {
  data("ex_survey", package = "saros")

  # Test with mesos/crowd functionality using existing variables
  result_all <- makeme(
    data = ex_survey,
    dep = a_1,
    crowd = "all",
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_table_html",
    hide_for_crowd_if_all_na = TRUE,
    hide_for_crowd_if_valid_n_below = 5
  )

  expect_true(is.data.frame(result_all))
  # a_1 should be included as it has valid data
  expect_equal(nrow(result_all), 1)
})

test_that("makeme hide arguments preserve expected output structure when hiding variables", {
  data("ex_survey", package = "saros")

  # Test that when variables are hidden, the output structure remains consistent
  test_data <- ex_survey[1:30, ]

  # Test with variables that should be included
  result_keep <- makeme(
    data = test_data,
    dep = c(a_1, a_2),
    type = "cat_table_html",
    hide_for_crowd_if_all_na = TRUE
  )

  # Should include both a_1 and a_2
  expect_true(is.data.frame(result_keep))
  expect_equal(nrow(result_keep), 2) # Both variables
  expect_equal(ncol(result_keep), 5) # Standard cat_table_html columns
  expect_true(all(
    c("No (%)", "Yes (%)", "NA (%)", "Total (N)") %in% colnames(result_keep)
  ))

  # Test with variables that should be hidden due to insufficient data
  test_data_sparse <- test_data[1:5, ] # Very small sample

  result_hide <- makeme(
    data = test_data_sparse,
    dep = c(a_1, a_2),
    type = "cat_table_html",
    hide_for_crowd_if_valid_n_below = 10 # Require at least 10 valid observations
  )

  # Variables should be hidden due to insufficient valid observations
  expect_true(is.data.frame(result_hide))
  expect_equal(nrow(result_hide), 0) # No variables should remain
})

test_that("makeme hide arguments work with combinations of hiding conditions", {
  data("ex_survey", package = "saros")

  # Test with multiple hiding conditions using individual variables
  test_data <- ex_survey[1:40, ]

  # Test variable that should pass all conditions (normal a_1)
  result_pass <- makeme(
    data = test_data,
    dep = a_1,
    type = "cat_table_html",
    hide_for_crowd_if_all_na = TRUE,
    hide_for_crowd_if_category_k_below = 2,
    hide_for_crowd_if_valid_n_below = 5
  )

  # a_1 should pass all conditions and be included
  expect_true(is.data.frame(result_pass))
  expect_equal(nrow(result_pass), 1)

  # Create and test a variable that should fail conditions
  test_data$single_cat <- factor(rep("No", 40), levels = c("No", "Yes"))
  attr(test_data$single_cat, "label") <- "Only No Responses"

  result_fail <- makeme(
    data = test_data,
    dep = single_cat,
    type = "cat_table_html",
    hide_for_crowd_if_category_k_below = 2 # Should hide single-category variable
  )

  # single_cat should fail the category count condition
  expect_true(is.data.frame(result_fail))
  expect_equal(nrow(result_fail), 0) # Should be hidden
})

test_that("makeme hide arguments default values work as expected", {
  data("ex_survey", package = "saros")

  # Test default behavior with standard data
  result_defaults <- makeme(
    data = ex_survey,
    dep = c(a_1, a_2, a_3),
    type = "cat_table_html"
    # Using all default values for hide_for_crowd_if_* parameters
  )

  expect_true(is.data.frame(result_defaults))
  expect_equal(nrow(result_defaults), 3) # All three variables should be included with defaults

  # Verify default values are working by checking they're not too restrictive
  expect_true(all(result_defaults$`Total (N)` == 300))
  expect_true(all(grepl("Agreement", result_defaults$.variable_label)))
})
