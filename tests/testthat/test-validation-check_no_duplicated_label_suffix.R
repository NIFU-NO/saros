test_that("check_no_duplicated_label_suffix handles valid data frame", {
  # Valid data frame with no duplicates
  data_summary <- data.frame(
    .variable_label = c("Label A", "Label B", "Label C"),
    .variable_name = c("var1", "var2", "var3"),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    check_no_duplicated_label_suffix(data_summary, error_on_duplicates = TRUE)
  )
})

test_that("check_no_duplicated_label_suffix detects duplicates and errors", {
  # Data frame with duplicate labels
  data_summary <- data.frame(
    .variable_label = c("Label A", "Label A", "Label C"),
    .variable_name = c("var1", "var2", "var3"),
    stringsAsFactors = FALSE
  )

  expect_error(
    check_no_duplicated_label_suffix(data_summary, error_on_duplicates = TRUE),
    "Found duplicated variable labels"
  )
})

test_that("check_no_duplicated_label_suffix warns instead of errors", {
  # Data frame with duplicate labels, but warning mode
  data_summary <- data.frame(
    .variable_label = c("Label A", "Label A", "Label C"),
    .variable_name = c("var1", "var2", "var3"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    check_no_duplicated_label_suffix(data_summary, error_on_duplicates = FALSE),
    "Found duplicated variable labels"
  )
})

test_that("check_no_duplicated_label_suffix handles non-data.frame input", {
  # Non-data.frame input
  expect_warning(
    check_no_duplicated_label_suffix("not a data frame"),
    "data_summary is not a data frame"
  )

  expect_warning(
    check_no_duplicated_label_suffix(NULL),
    "data_summary is not a data frame"
  )
})

test_that("check_no_duplicated_label_suffix handles missing column", {
  # Data frame without required column
  data_summary <- data.frame(
    other_column = c("A", "B", "C"),
    .variable_name = c("var1", "var2", "var3"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    check_no_duplicated_label_suffix(data_summary),
    "Column '.variable_label' not found"
  )
})

test_that("check_no_duplicated_label_suffix handles empty data frame", {
  # Empty data frame
  data_summary <- data.frame(
    .variable_label = character(0),
    .variable_name = character(0),
    stringsAsFactors = FALSE
  )

  expect_warning(
    check_no_duplicated_label_suffix(data_summary),
    "data_summary is empty \\(0 rows\\)"
  )
})

test_that("check_no_duplicated_label_suffix handles NA labels", {
  # Data frame with NA labels (should not trigger duplicate warning)
  data_summary <- data.frame(
    .variable_label = c(NA, NA, "Label C"),
    .variable_name = c("var1", "var2", "var3"),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    check_no_duplicated_label_suffix(data_summary, error_on_duplicates = TRUE)
  )
})

test_that("check_no_duplicated_label_suffix handles mixed NA and duplicates", {
  # Data frame with both NA and real duplicate labels
  data_summary <- data.frame(
    .variable_label = c("Label A", "Label A", NA),
    .variable_name = c("var1", "var2", "var3"),
    stringsAsFactors = FALSE
  )

  expect_error(
    check_no_duplicated_label_suffix(data_summary, error_on_duplicates = TRUE),
    "Found duplicated variable labels"
  )
})

test_that("check_no_duplicated_label_suffix with grouped data frame", {
  skip_if_not_installed("dplyr")

  # Test with a grouped data frame structure
  data_summary <- data.frame(
    .variable_label = c("Label A", "Label B", "Label A"),
    .variable_name = c("var1", "var2", "var1_subset"),
    group_var = c("Group1", "Group1", "Group2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    check_no_duplicated_label_suffix(data_summary, error_on_duplicates = TRUE),
    "Found duplicated variable labels"
  )
})
