# Tests for ordered vs unordered factor sorting behavior - Issue #372
# These tests ensure that the sorting principles are maintained:
# - Ordered factors: Always display in inherent level order, ignore sort_by/descend
# - Unordered factors: Influenced by sort_by and descend parameters

testthat::test_that("ordered dependent variables ignore sort_by and descend", {
  # Create test data with ordered dependent variables in specific order
  test_data <- data.frame(
    var_first = factor(
      c("Low", "Med", "High"),
      levels = c("Low", "Med", "High"),
      ordered = TRUE
    ),
    var_second = factor(
      c("A", "B", "C"),
      levels = c("A", "B", "C"),
      ordered = TRUE
    ),
    var_third = factor(
      c("X", "Y", "Z"),
      levels = c("X", "Y", "Z"),
      ordered = TRUE
    ),
    indep = factor(c("Group1", "Group2", "Group1"))
  )

  # Set variable labels
  labelled::var_label(test_data$var_first) <- "First Variable"
  labelled::var_label(test_data$var_second) <- "Second Variable"
  labelled::var_label(test_data$var_third) <- "Third Variable"

  # Test 1: .variable_position with descend = FALSE
  result1 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_first", "var_second", "var_third"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = FALSE
  )

  # Test 2: .variable_position with descend = TRUE (should be IDENTICAL)
  result2 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_first", "var_second", "var_third"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = TRUE
  )

  # Test 3: .top with descend = FALSE (should be IDENTICAL to position)
  result3 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_first", "var_second", "var_third"),
    indep = "indep",
    sort_by = ".top",
    descend = FALSE
  )

  # Test 4: .top with descend = TRUE (should be IDENTICAL to position)
  result4 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_first", "var_second", "var_third"),
    indep = "indep",
    sort_by = ".top",
    descend = TRUE
  )

  expected_levels <- c("First Variable", "Second Variable", "Third Variable")

  # All results should have identical .variable_label levels for ordered factors
  testthat::expect_equal(levels(result1$.variable_label), expected_levels)
  testthat::expect_equal(levels(result2$.variable_label), expected_levels)
  testthat::expect_equal(levels(result3$.variable_label), expected_levels)
  testthat::expect_equal(levels(result4$.variable_label), expected_levels)
})

testthat::test_that("unordered dependent variables respect sort_by and descend", {
  # Create test data with unordered dependent variables
  test_data <- data.frame(
    var_first = factor(c("Low", "Med", "High"), ordered = FALSE),
    var_second = factor(c("A", "B", "C"), ordered = FALSE),
    var_third = factor(c("X", "Y", "Z"), ordered = FALSE),
    indep = factor(c("Group1", "Group2", "Group1"))
  )

  labelled::var_label(test_data$var_first) <- "First Variable"
  labelled::var_label(test_data$var_second) <- "Second Variable"
  labelled::var_label(test_data$var_third) <- "Third Variable"

  # Test position sorting with different descend values
  result_pos_asc <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_first", "var_second", "var_third"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = FALSE
  )

  result_pos_desc <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_first", "var_second", "var_third"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = TRUE
  )

  # For unordered factors, descend should reverse the order
  expected_asc <- c("First Variable", "Second Variable", "Third Variable")
  expected_desc <- c("Third Variable", "Second Variable", "First Variable")

  testthat::expect_equal(levels(result_pos_asc$.variable_label), expected_asc)
  testthat::expect_equal(levels(result_pos_desc$.variable_label), expected_desc)
})

testthat::test_that("mixed ordered and unordered dependent variables behavior", {
  # Create test data where NOT all dependent variables are ordered
  # This should result in unordered factor behavior (respecting sort_by/descend)
  test_data <- data.frame(
    var_ordered = factor(
      c("Low", "High"),
      levels = c("Low", "High"),
      ordered = TRUE
    ),
    var_unordered = factor(c("A", "B"), ordered = FALSE),
    indep = factor(c("Group1", "Group2"))
  )

  labelled::var_label(test_data$var_ordered) <- "Ordered Variable"
  labelled::var_label(test_data$var_unordered) <- "Unordered Variable"

  # When NOT all deps are ordered, should behave like unordered factors
  result_asc <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_ordered", "var_unordered"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = FALSE
  )

  result_desc <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_ordered", "var_unordered"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = TRUE
  )

  # Should behave as unordered (since not ALL deps are ordered)
  expected_asc <- c("Ordered Variable", "Unordered Variable")
  expected_desc <- c("Unordered Variable", "Ordered Variable")

  testthat::expect_equal(levels(result_asc$.variable_label), expected_asc)
  testthat::expect_equal(levels(result_desc$.variable_label), expected_desc)
})

testthat::test_that("data processing layer preserves ordered factor behavior", {
  # Test the core data processing functionality that makeme relies on
  test_data <- data.frame(
    var1 = factor(
      c("Low", "Med", "High", "Low", "Med", "High"),
      levels = c("Low", "Med", "High"),
      ordered = TRUE
    ),
    var2 = factor(
      c("A", "B", "C", "A", "B", "C"),
      levels = c("A", "B", "C"),
      ordered = TRUE
    ),
    indep = factor(c(
      "Group1",
      "Group1",
      "Group1",
      "Group2",
      "Group2",
      "Group2"
    ))
  )

  labelled::var_label(test_data$var1) <- "Question 1"
  labelled::var_label(test_data$var2) <- "Question 2"

  # Test the underlying data processing directly
  result1 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var1", "var2"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = FALSE
  )

  result2 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var1", "var2"),
    indep = "indep",
    sort_by = ".variable_position",
    descend = TRUE
  )

  # For ordered factors, both results should have identical variable ordering
  expected_order <- c("Question 1", "Question 2")
  testthat::expect_equal(levels(result1$.variable_label), expected_order)
  testthat::expect_equal(levels(result2$.variable_label), expected_order)

  # Also verify the data processing works with different sort methods
  result3 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var1", "var2"),
    indep = "indep",
    sort_by = ".top",
    descend = FALSE
  )

  # Should still maintain same ordering for ordered factors
  testthat::expect_equal(levels(result3$.variable_label), expected_order)
})

testthat::test_that("regression test: sort_by NULL with ordered factors", {
  # Ensure that sort_by = NULL doesn't break ordered factor behavior
  test_data <- data.frame(
    var1 = factor(c("Z", "A"), levels = c("Z", "A"), ordered = TRUE),
    var2 = factor(c("Y", "B"), levels = c("Y", "B"), ordered = TRUE),
    indep = factor(c("Group1", "Group2"))
  )

  labelled::var_label(test_data$var1) <- "Variable 1"
  labelled::var_label(test_data$var2) <- "Variable 2"

  result <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var1", "var2"),
    indep = "indep",
    sort_by = NULL, # Test NULL case
    descend = FALSE
  )

  # Should maintain input order for ordered factors even with sort_by = NULL
  expected_levels <- c("Variable 1", "Variable 2")
  testthat::expect_equal(levels(result$.variable_label), expected_levels)
})
