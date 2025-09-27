testthat::test_that("make_content.int_table_html works with no indep", {
  result <-
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      type = "int_table_html"
    )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2) # Two dependent variables
  testthat::expect_true(".variable_label" %in% colnames(result))
  testthat::expect_true("Mean" %in% colnames(result))
  testthat::expect_true("SD" %in% colnames(result))
  testthat::expect_true("Median" %in% colnames(result))
  testthat::expect_true("N" %in% colnames(result))
})

testthat::test_that("make_content.int_table_html works with indep", {
  result <-
    saros::makeme(
      data = saros::ex_survey,
      dep = c("c_1", "c_2"),
      indep = "x1_sex",
      type = "int_table_html"
    )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 4) # 2 variables × 2 sex categories
  testthat::expect_true(".variable_label" %in% colnames(result))
  testthat::expect_true("Gender" %in% colnames(result)) # x1_sex gets labeled as "Gender"
  testthat::expect_true("Mean" %in% colnames(result))
  testthat::expect_true("SD" %in% colnames(result))
})

testthat::test_that("make_content.int_table_html respects digits argument", {
  result_0 <-
    saros::makeme(
      data = saros::ex_survey,
      dep = "c_1",
      type = "int_table_html",
      digits = 0
    )

  result_2 <-
    saros::makeme(
      data = saros::ex_survey,
      dep = "c_1",
      type = "int_table_html",
      digits = 2
    )

  # Test that digits = 0 produces integers in display (check mean column)
  mean_0 <- result_0$Mean[1]
  mean_2 <- result_2$Mean[1]

  # With digits = 0, mean should be rounded to nearest integer
  # With digits = 2, mean should have 2 decimal places
  testthat::expect_equal(mean_0, round(mean_2, 0))
})

testthat::test_that("make_content.int_table_html handles multiple indep variables", {
  # Currently int tables only support single indep variable
  testthat::expect_error(
    saros::makeme(
      data = saros::ex_survey,
      dep = "c_1",
      indep = c("x1_sex", "x2_human"),
      type = "int_table_html"
    ),
    regexp = "Too many columns provided for"
  )
})

testthat::test_that("make_content.int_table_html handles missing data correctly", {
  # Create test data with some missing values
  test_data <- saros::ex_survey
  test_data$c_1[1:10] <- NA

  result <-
    saros::makeme(
      data = test_data,
      dep = "c_1",
      type = "int_table_html"
    )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("N_missing" %in% colnames(result))
  testthat::expect_equal(result$N_missing[1], 10)
  testthat::expect_equal(result$N_valid[1], 290)
})

testthat::test_that("make_content.int_table_html column ordering is logical", {
  result <-
    saros::makeme(
      data = saros::ex_survey,
      dep = c("c_1", "c_2"),
      indep = "x1_sex",
      type = "int_table_html"
    )

  col_names <- colnames(result)

  # Variable should be first
  testthat::expect_equal(col_names[1], ".variable_label")
  # Independent variable should be second (labeled as "Gender")
  testthat::expect_equal(col_names[2], "Gender")
  # Statistical measures should follow in logical order
  testthat::expect_true(which(col_names == "N") < which(col_names == "Mean"))
  testthat::expect_true(which(col_names == "Mean") < which(col_names == "SD"))
  testthat::expect_true(
    which(col_names == "Median") < which(col_names == "MAD")
  )
})

testthat::test_that("make_content.int_table_html handles totals correctly", {
  result <-
    saros::makeme(
      data = saros::ex_survey,
      dep = "c_1",
      indep = "x1_sex",
      type = "int_table_html",
      totals = TRUE
    )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 3) # 2 sex groups + 1 total

  # Check that "Total" row exists
  testthat::expect_true("Total" %in% result$Gender)

  # Total row should have overall statistics
  total_row <- result[result$Gender == "Total", ]
  testthat::expect_equal(total_row$N, 300) # Total sample size
  testthat::expect_equal(total_row$N_valid, 300)
})

testthat::test_that("make_content.int_table_html handles character indep without warnings", {
  # Test with f_uni which has character values that could cause coercion warnings
  testthat::expect_no_warning(
    result <- saros::makeme(
      data = saros::ex_survey,
      dep = "c_1",
      indep = "f_uni",
      type = "int_table_html"
    )
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0) # Should have multiple rows for different universities
  testthat::expect_true(".variable_label" %in% colnames(result))
})

testthat::test_that("make_content.int_table_html handles table_main_question_as_header with variable suffixes", {
  # Test with variables that have separators to ensure suffixes are extracted
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = c("a_1", "a_2"),
    type = "int_table_html",
    table_main_question_as_header = TRUE
  )

  testthat::expect_s3_class(result, "data.frame")
  # The column header should be the main question
  testthat::expect_true(
    "Do you consent to the following?" %in% colnames(result)
  )
  # The variable values should only contain suffixes ("Agreement #1", "Agreement #2")
  testthat::expect_true(all(c("Agreement #1", "Agreement #2") %in% result[[1]]))
  testthat::expect_false(
    "Do you consent to the following? - Agreement #1" %in% result[[1]]
  )
  testthat::expect_false(
    "Do you consent to the following? - Agreement #2" %in% result[[1]]
  )
})

testthat::test_that("make_content.int_table_html handles label_separator with naming conflicts", {
  # Test with dependent and independent variables that have the same main question
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = c("a_1", "a_2"),
    indep = "a_3",
    type = "int_table_html",
    label_separator = " - ",
    table_main_question_as_header = TRUE
  )

  testthat::expect_s3_class(result, "data.frame")
  # The main question should be the column header
  testthat::expect_true(
    "Do you consent to the following?" %in% colnames(result)
  )
  # The indep column should have "(indep)" suffix to resolve naming conflict
  testthat::expect_true(
    "Do you consent to the following? (indep)" %in% colnames(result)
  )
  # Variable values should show only suffixes
  testthat::expect_true(all(c("Agreement #1", "Agreement #2") %in% result[[1]]))
  testthat::expect_false(
    "Do you consent to the following? - Agreement #1" %in% result[[1]]
  )
})
