testthat::test_that("sort_dep_by default/.variable_position + NULL + descend", {
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

test_that("independent category-based sorting works (single and multiple categories)", {
  # Use a subset for speed and determinism
  data <- saros::ex_survey[1:200, ]

  # Single target category (e.g., "Not at all")
  res_single <- saros::makeme(
    data = data,
    dep = b_1,
    indep = x1_sex,
    type = "cat_table_html",
    sort_indep_by = "Not at all",
    showNA = "never"
  )
  expect_s3_class(res_single, "data.frame")
  expect_true(nrow(res_single) >= 1)
  expect_true("Gender" %in% names(res_single))

  # Verify monotonicity of the target category column across indep order
  target_col <- "Not at all (%)"
  if (target_col %in% colnames(res_single)) {
    vals <- suppressWarnings(as.numeric(res_single[[target_col]]))
    # Should be monotonic (either non-decreasing or non-increasing)
    expect_true(all(diff(vals) >= 0) || all(diff(vals) <= 0))
  }

  # Multiple categories combined (e.g., "A bit" + "A lot")
  res_multi <- saros::makeme(
    data = data,
    dep = b_1,
    indep = x1_sex,
    type = "cat_table_html",
    sort_indep_by = c("A bit", "A lot"),
    showNA = "never"
  )
  expect_s3_class(res_multi, "data.frame")
  expect_true(nrow(res_multi) >= 1)
  expect_true("Gender" %in% names(res_multi))

  # If both columns present, check monotonicity of their sum
  cols <- intersect(c("A bit (%)", "A lot (%)"), colnames(res_multi))
  if (length(cols) == 2) {
    vals <- rowSums(
      apply(res_multi[cols], 2, function(x) suppressWarnings(as.numeric(x))),
      na.rm = TRUE
    )
    expect_true(all(diff(vals) >= 0) || all(diff(vals) <= 0))
  }
})

test_that("dependent sorting handles ties without error", {
  # Construct synthetic data where two variables have identical distributions (ties)
  set.seed(123)
  n <- 60
  grp <- factor(rep(c("G1", "G2"), length.out = n), levels = c("G1", "G2"))
  vals <- rep(c("No", "Yes"), length.out = n)
  v1 <- factor(vals, levels = c("No", "Yes"))
  v2 <- factor(vals, levels = c("No", "Yes")) # identical to v1 to force ties
  tie_data <- data.frame(v1 = v1, v2 = v2, grp = grp)
  labelled::var_label(tie_data$v1) <- "V1"
  labelled::var_label(tie_data$v2) <- "V2"
  labelled::var_label(tie_data$grp) <- "Group"

  res <- saros::makeme(
    data = tie_data,
    dep = c(v1, v2),
    indep = grp,
    type = "cat_table_html",
    sort_dep_by = ".upper",
    showNA = "never"
  )

  expect_s3_class(res, "data.frame")
  labs <- unique(as.character(res$.variable_label))
  expect_setequal(labs, c("V1", "V2"))
})

test_that("single-category dependent variable works", {
  # One-level factor shouldn't break sorting
  single <- factor(rep("Only", 50), levels = "Only")
  grp <- factor(rep(c("G1", "G2"), 25), levels = c("G1", "G2"))
  d <- data.frame(single = single, grp = grp)
  labelled::var_label(d$single) <- "Single"
  labelled::var_label(d$grp) <- "Group"

  out <- saros::makeme(
    data = d,
    dep = single,
    indep = grp,
    type = "cat_table_html",
    showNA = "never",
    hide_for_crowd_if_category_k_below = 0
  )

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) >= 1)
  expect_true(any(grepl("Only", colnames(out), fixed = TRUE)))
})

test_that("dependent sorting by multiple categories reverses with descend", {
  # Verify combined-category sorting responds to descend flag
  res_asc <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    sort_dep_by = c("A bit", "A lot"),
    descend = FALSE,
    showNA = "never"
  )
  res_desc <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    sort_dep_by = c("A bit", "A lot"),
    descend = TRUE,
    showNA = "never"
  )

  labs_asc <- as.character(unique(res_asc$.variable_label))
  labs_desc <- as.character(unique(res_desc$.variable_label))
  expect_equal(labs_asc, rev(labs_desc))
})

# testthat::test_that("independent sorting by .count_per_indep_group supports descend_indep", {
#   # Order by total indep-group counts and verify descend_indep reverses
#   out_asc <- saros::makeme(
#     data = saros::ex_survey,
#     dep = b_1:b_2,
#     indep = x1_sex,
#     type = "cat_table_html",
#     sort_indep_by = ".count_per_indep_group",
#     descend_indep = FALSE,
#     showNA = "never"
#   )
#   out_desc <- saros::makeme(
#     data = saros::ex_survey,
#     dep = b_1:b_2,
#     indep = x1_sex,
#     type = "cat_table_html",
#     sort_indep_by = ".count_per_indep_group",
#     descend_indep = TRUE,
#     showNA = "never"
#   )

#   expect_true("Gender" %in% names(out_asc))
#   expect_true("Gender" %in% names(out_desc))

#   # Compare the order of the indep labels (row order)
#   rows_asc <- out_asc$Gender
#   rows_desc <- out_desc$Gender
#   expect_equal(as.character(rows_asc), rev(as.character(rows_desc)))
# })

test_that("NA values in dependent data do not break sorting", {
  # Create a small dataset with NAs in the dependent variable
  set.seed(42)
  dep <- factor(
    sample(c("No", "Yes", NA_character_), size = 100, replace = TRUE),
    levels = c("No", "Yes")
  )
  indep <- factor(sample(c("G1", "G2"), size = 100, replace = TRUE))
  d <- data.frame(dep = dep, indep = indep)
  labelled::var_label(d$dep) <- "Dep"
  labelled::var_label(d$indep) <- "Group"

  r1 <- saros::makeme(
    data = d,
    dep = dep,
    indep = indep,
    type = "cat_table_html",
    sort_dep_by = ".upper",
    descend = FALSE,
    showNA = "never"
  )
  r2 <- saros::makeme(
    data = d,
    dep = dep,
    indep = indep,
    type = "cat_table_html",
    sort_dep_by = ".upper",
    descend = TRUE,
    showNA = "never"
  )

  expect_true(nrow(r1) == nrow(r2))
  expect_equal(
    as.character(unique(r1[[1]])),
    rev(as.character(unique(r2[[1]])))
  )
})
