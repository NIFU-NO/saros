# Tests for ordered vs unordered factor sorting behavior - Issue #372
# These tests ensure that the sorting principles are maintained:
# - Ordered factors: Always display in inherent level order, ignore sort_by/descend
# - Unordered factors: Influenced by sort_by and descend parameters

testthat::test_that("unordered dependent variables adhere to sort_by and descend", {
  test_data <-
    saros::ex_survey |>
    dplyr::relocate(b_3, .before = b_1)

  # Test 1: .variable_position with descend = FALSE
  result1 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("b_1", "b_2", "b_3"),
    indep = "x1_sex",
    sort_dep_by = ".variable_position",
    descend = FALSE
  )

  # Test 2: .variable_position with descend = TRUE (should be OPPOSITE order)
  result2 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("b_1", "b_2", "b_3"),
    indep = "x1_sex",
    sort_dep_by = ".variable_position",
    descend = TRUE
  )

  # Test 3: .top with descend = FALSE
  result3 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("b_1", "b_2", "b_3"),
    indep = "x1_sex",
    sort_dep_by = ".top",
    descend = FALSE
  )

  # Test 4: .top with descend = TRUE (should be OPPOSITE order of result3)
  result4 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("b_1", "b_2", "b_3"),
    indep = "x1_sex",
    sort_dep_by = ".top",
    descend = TRUE
  )

  # All results should have identical .variable_label levels for ordered factors
  testthat::expect_equal(
    as.character(result1$.variable_label),
    rev(as.character(result2$.variable_label))
  )
  testthat::expect_equal(
    as.character(result3$.variable_label),
    rev(as.character(result4$.variable_label))
  )
  testthat::expect_equal(
    levels(result1$.variable_label),
    rev(levels(result2$.variable_label))
  )
  testthat::expect_equal(
    levels(result3$.variable_label),
    rev(levels(result4$.variable_label))
  )
  testthat::expect_equal(
    result3$.proportion,
    c(
      0.41721854,
      0.44295302,
      0.47682119,
      0.47651007,
      0.10596026,
      0.08053691,
      0.50993377,
      0.44966443,
      0.38410596,
      0.45637584,
      0.10596026,
      0.09395973,
      0.47682119,
      0.44966443,
      0.44370861,
      0.47651007,
      0.07947020,
      0.07382550
    )
  )
})

testthat::test_that("ordered dependent variables ignores sort_by and descend", {
  test_data <-
    saros::ex_survey |>
    dplyr::mutate(dplyr::across(
      c(b_1:b_3, x1_sex),
      ~ factor(.x, ordered = TRUE)
    )) |>
    labelled::copy_labels_from(saros::ex_survey)

  # Test the underlying data processing directly
  result1 <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("b_1", "b_2", "b_3"),
    indep = "x1_sex",
    sort_dep_by = ".top",
    descend = FALSE
  )

  result2 <- saros:::summarize_cat_cat_data(
    data = saros::ex_survey,
    dep = c("b_1", "b_2", "b_3"),
    indep = "x1_sex",
    sort_dep_by = ".variable_position",
    descend = FALSE
  )

  testthat::expect_equal(
    result1 |> dplyr::select(-.category, -x1_sex, -.dep_order),
    result2 |> dplyr::select(-.category, -x1_sex, -.dep_order)
  )
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
    sort_dep_by = ".variable_position",
    descend = FALSE
  )

  result_desc <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = c("var_ordered", "var_unordered"),
    indep = "indep",
    sort_dep_by = ".variable_position",
    descend = TRUE
  )

  # Should behave as unordered (since not ALL deps are ordered)
  expected_asc <- c("Ordered Variable", "Unordered Variable")
  expected_desc <- c("Unordered Variable", "Ordered Variable")

  testthat::expect_equal(levels(result_asc$.variable_label), expected_asc)
  testthat::expect_equal(levels(result_desc$.variable_label), expected_desc)
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
    sort_dep_by = NULL, # Test NULL case
    descend = FALSE
  )

  # Should maintain input order for ordered factors even with sort_by = NULL
  expected_levels <- c("Variable 1", "Variable 2")
  testthat::expect_equal(levels(result$.variable_label), expected_levels)
})
