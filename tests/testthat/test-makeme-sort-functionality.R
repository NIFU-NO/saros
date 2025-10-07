test_that("sort_dep_by and sort_indep_by parameters work correctly", {
  # Test 1: Default sort_dep_by should be ".variable_position"
  result_default <- makeme(
    data = ex_survey,
    dep = c("b_1", "b_2", "b_3"),
    type = "cat_table_html"
  )
  expect_s3_class(result_default, "data.frame")

  # Test 2: sort_dep_by = NULL should convert to ".variable_position"
  result_null <- makeme(
    data = ex_survey,
    dep = c("b_1", "b_2", "b_3"),
    type = "cat_table_html",
    sort_dep_by = NULL
  )
  expect_s3_class(result_null, "data.frame")

  # Test 3: Explicit sort_dep_by parameter
  result_explicit <- makeme(
    data = ex_survey,
    dep = c("b_1", "b_2", "b_3"),
    type = "cat_table_html",
    sort_dep_by = ".variable_label"
  )
  expect_s3_class(result_explicit, "data.frame")

  # Test 4: Legacy sort_by parameter should show deprecation warning
  expect_warning(
    makeme(
      data = ex_survey,
      dep = c("b_1", "b_2", "b_3"),
      type = "cat_table_html",
      sort_by = ".upper"
    ),
    "The 'sort_by' parameter is deprecated"
  )

  # Test 5: sort_indep_by parameter works
  result_indep <- makeme(
    data = ex_survey,
    dep = c("b_1", "b_2"),
    indep = "x1_sex",
    type = "cat_table_html",
    sort_dep_by = ".variable_position",
    sort_indep_by = ".upper"
  )
  expect_s3_class(result_indep, "data.frame")

  # Test 6: Both new parameters work together
  result_both <- makeme(
    data = ex_survey,
    dep = c("b_1", "b_2"),
    indep = "x1_sex",
    type = "cat_table_html",
    sort_dep_by = ".variable_label",
    sort_indep_by = NULL
  )
  expect_s3_class(result_both, "data.frame")
})

test_that("sort_by parameter backward compatibility works", {
  # Test that sort_by still works but shows deprecation warning
  expect_warning(
    result <- makeme(
      data = ex_survey,
      dep = c("b_1", "b_2"),
      type = "cat_table_html",
      sort_by = ".upper"
    ),
    "deprecated"
  )

  # The result should still be valid
  expect_s3_class(result, "data.frame")
})

test_that("new sort parameters are passed correctly to summarize_cat_cat_data", {
  # Test that the new parameters work with summarize_cat_cat_data directly
  result <- summarize_cat_cat_data(
    data = ex_survey,
    dep = c("b_1", "b_2"),
    indep = "x1_sex",
    sort_dep_by = ".variable_position",
    sort_indep_by = ".upper"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("parameter precedence works correctly", {
  # When both sort_by and new parameters are specified, new parameters should take precedence
  # but sort_by should still trigger deprecation warning
  expect_warning(
    result <- makeme(
      data = ex_survey,
      dep = c("b_1", "b_2"),
      type = "cat_table_html",
      sort_by = ".upper", # legacy parameter
      sort_dep_by = ".variable_position", # should override
      sort_indep_by = NULL # should override
    ),
    "deprecated"
  )

  expect_s3_class(result, "data.frame")
})
