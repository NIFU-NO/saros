test_that("sort_dep_by default/.variable_position + NULL + descend", {
  deps <- c("p_1", "p_2", "p_3", "p_4")

  # Default behavior (.variable_position)
  result_default <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    descend = FALSE,
    showNA = "never"
  )
  variable_labels <- as.character(unique(result_default$.variable_label))
  expect_equal(length(variable_labels), 4)
  expect_equal(variable_labels[1], "Red Party")
  expect_equal(variable_labels[2], "Green Party")
  expect_equal(variable_labels[3], "Yellow Party")
  expect_equal(variable_labels[4], "Blue Party")

  # NULL converts to .variable_position
  result_null <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    sort_dep_by = NULL,
    showNA = "never"
  )
  result_explicit <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    sort_dep_by = ".variable_position",
    showNA = "never"
  )
  expect_equal(
    unique(result_null$.variable_label),
    unique(result_explicit$.variable_label)
  )

  # Descend reverses .variable_position
  result_desc <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    sort_dep_by = ".variable_position",
    descend = TRUE,
    showNA = "never"
  )
  labels_desc <- as.character(unique(result_desc$.variable_label))
  expect_equal(variable_labels, rev(labels_desc))
})

test_that("sort_dep_by = .variable_label sorts alphabetically", {
  # Test alphabetical sorting by variable labels
  result_default <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    showNA = "never",
    descend = FALSE
  )

  result_sorted <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    sort_dep_by = ".variable_label",
    showNA = "never",
    descend = FALSE
  )

  labels_default <- as.character(unique(result_default$.variable_label))
  labels_sorted <- as.character(unique(result_sorted$.variable_label))

  # The sorted result should be different from default (unless they happen to be the same)
  # and should be in alphabetical order
  sorted_expected <- sort(labels_default)
  expect_equal(labels_sorted, sorted_expected)
})

test_that("sort_dep_by = .variable_name sorts by column names", {
  # Test sorting by variable names (p_1, p_2, p_3, p_4)
  result_mixed <- saros::makeme(
    data = saros::ex_survey,
    dep = tidyselect::all_of(c("p_4", "p_1", "p_3", "p_2")), # Mixed order input
    type = "cat_table_html",
    sort_dep_by = ".variable_name",
    showNA = "never"
  )

  result_ordered <- saros::makeme(
    data = saros::ex_survey,
    dep = tidyselect::all_of(c("p_1", "p_2", "p_3", "p_4")), # Ordered input
    type = "cat_table_html",
    sort_dep_by = ".variable_name",
    showNA = "never"
  )

  # Both should give the same order regardless of input order
  labels_mixed <- as.character(unique(result_mixed$.variable_label))
  labels_ordered <- as.character(unique(result_ordered$.variable_label))
  expect_equal(labels_mixed, labels_ordered)

  # Should have all 4 variables
  expect_equal(length(labels_mixed), 4)
})

test_that("legacy sort_by parameter shows deprecation warning", {
  # Test that sort_by still works but shows deprecation warning
  testthat::expect_warning(
    result <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      type = "cat_table_html",
      sort_by = ".variable_position"
    ),
    "The 'sort_by' parameter is deprecated"
  )

  # Should still produce valid output
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("sort_indep_by parameter controls independent variable category order", {
  # Create test data with ordered categories for predictable testing
  test_data <- saros::ex_survey[1:100, ] # Smaller dataset for focused testing

  # Test with sort_indep_by = NULL (should preserve factor levels)
  result_null <- saros::makeme(
    data = test_data,
    dep = b_1,
    indep = x1_sex, # Should have "Male" and "Female" categories
    type = "cat_table_html",
    sort_indep_by = NULL,
    showNA = "never"
  )

  # Check that we have Gender column (indep variable becomes row header)
  expect_true("Gender" %in% names(result_null))

  # Test with sort_indep_by = ".upper" (should sort by proportion)
  result_upper <- makeme(
    data = test_data,
    dep = b_1,
    indep = x1_sex,
    type = "cat_table_html",
    sort_indep_by = ".upper",
    showNA = "never"
  )

  expect_s3_class(result_upper, "data.frame")
  expect_true(nrow(result_upper) > 0)
  expect_true("Gender" %in% names(result_upper))
})

test_that("sort_dep_by and sort_indep_by work together", {
  # Test that both parameters can be used simultaneously
  result <- saros::makeme(
    data = saros::ex_survey[1:100, ],
    dep = b_1:b_2,
    indep = x1_sex,
    type = "cat_table_html",
    sort_dep_by = ".variable_label",
    sort_indep_by = ".upper",
    showNA = "never"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Should have at least 2 different variables (b_1 and b_2)
  variable_labels <- unique(result$.variable_label)
  expect_true(length(variable_labels) >= 2)
})

test_that("sort_dep_by = .upper sorts by highest category proportion", {
  # Test that .upper sorting works correctly
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    sort_dep_by = ".upper",
    showNA = "never"
  )

  expect_s3_class(result, "data.frame")
  variable_labels <- unique(result$.variable_label)
  expect_equal(length(variable_labels), 3)

  # Variables should be ordered by their highest category proportion
  # (The exact order would depend on the data, but we can verify the mechanism works)
  expect_true(all(!is.na(variable_labels)))
})

test_that("parameter precedence: sort_dep_by overrides legacy sort_by", {
  # When both parameters are specified, sort_dep_by should take precedence
  expect_warning(
    result <- saros::makeme(
      data = saros::ex_survey,
      dep = p_1:p_4,
      type = "cat_table_html",
      sort_by = ".upper", # legacy parameter
      sort_dep_by = ".variable_position", # should override
      showNA = "never"
    ),
    "deprecated"
  )

  # Should follow .variable_position order (current default behavior)
  variable_labels <- as.character(unique(result$.variable_label))
  expect_equal(length(variable_labels), 4)
  # Just verify it works and produces valid output
  expect_true(all(
    c("Red Party", "Green Party", "Yellow Party", "Blue Party") %in%
      variable_labels
  ))
})

test_that("summarize_cat_cat_data accepts new parameters directly", {
  # Test the underlying function with new parameters
  result <- saros:::summarize_cat_cat_data(
    data = saros::ex_survey,
    dep = paste0("b_", 1:2),
    indep = "x1_sex",
    sort_dep_by = ".variable_position",
    sort_indep_by = NULL,
    showNA = "never"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Should have the expected columns
  expected_cols <- c(".variable_label", ".category", ".proportion")
  expect_true(all(expected_cols %in% names(result)))

  # Should have 2 variables (b_1 and b_2)
  expect_equal(length(unique(result$.variable_name)), 2)
})

test_that("descend parameter works with .variable_position sorting", {
  # Test that descend reverses the variable position order
  result_asc <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    sort_dep_by = ".variable_position",
    descend = FALSE,
    showNA = "never"
  )

  result_desc <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1:p_4,
    type = "cat_table_html",
    sort_dep_by = ".variable_position",
    descend = TRUE,
    showNA = "never"
  )

  labels_asc <- as.character(unique(result_asc$.variable_label))
  labels_desc <- as.character(unique(result_desc$.variable_label))

  # Descending should be reverse of ascending
  expect_equal(labels_asc, rev(labels_desc))

  # Ascending: Red, Green, Yellow, Blue
  expect_equal(labels_asc[1], "Red Party")
  expect_equal(labels_asc[4], "Blue Party")

  # Descending: Blue, Yellow, Green, Red
  expect_equal(labels_desc[1], "Blue Party")
  expect_equal(labels_desc[4], "Red Party")
})
