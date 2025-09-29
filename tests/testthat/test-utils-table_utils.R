testthat::test_that("setup_table_data returns data when not empty", {
  dots <- list(data_summary = data.frame(x = 1:3, y = 4:6))
  result <- saros:::setup_table_data(dots)
  
  testthat::expect_false(result$should_return)
  testthat::expect_equal(result$data, dots$data_summary)
})

testthat::test_that("setup_table_data handles empty data", {
  dots <- list(data_summary = data.frame())
  result <- saros:::setup_table_data(dots)
  
  testthat::expect_true(result$should_return)
  testthat::expect_equal(result$data, data.frame())
})

testthat::test_that("determine_variable_basis prefers .variable_label", {
  data_summary <- data.frame(
    .variable_label = c("Label 1", "Label 2"),
    .variable_name = c("var1", "var2")
  )
  result <- saros:::determine_variable_basis(data_summary)
  testthat::expect_equal(result, ".variable_label")
})

testthat::test_that("determine_variable_basis falls back to .variable_name with warning", {
  data_summary <- data.frame(
    .variable_label = c(NA, NA),
    .variable_name = c("var1", "var2")
  )
  testthat::expect_warning(
    result <- saros:::determine_variable_basis(data_summary),
    regexp = "No variable labels found"
  )
  testthat::expect_equal(result, ".variable_name")
})

testthat::test_that("arrange_table_data sorts by column basis", {
  data <- data.frame(
    var_col = factor(c("C", "A", "B")),
    values = 1:3
  )
  result <- saros:::arrange_table_data(data, col_basis = "var_col")
  
  # Should be sorted alphabetically by factor levels: A, B, C
  testthat::expect_equal(result$var_col, factor(c("A", "B", "C")))
  testthat::expect_equal(result$values, c(2, 3, 1)) # Corresponding values after sorting
})

testthat::test_that("arrange_table_data sorts by column basis and independent variable", {
  data <- data.frame(
    var_col = factor(c("B", "A", "B", "A")),
    indep_col = factor(c("Y", "X", "X", "Y")),
    values = 1:4
  )
  result <- saros:::arrange_table_data(data, col_basis = "var_col", indep_vars = "indep_col")
  
  # Should be sorted by both columns
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 4)
})

testthat::test_that("round_numeric_stats rounds appropriate columns", {
  data <- data.frame(
    .mean = c(1.12345, 2.67890),
    .median = c(1.5555, 2.3333),
    text_col = c("A", "B")
  )
  result <- saros:::round_numeric_stats(data, digits = 2)
  
  testthat::expect_equal(result$.mean, c(1.12, 2.68))
  testthat::expect_equal(result$.median, c(1.56, 2.33))
  testthat::expect_equal(result$text_col, c("A", "B"))
})

testthat::test_that("round_numeric_stats handles missing numeric columns", {
  data <- data.frame(text_col = c("A", "B"))
  result <- saros:::round_numeric_stats(data, digits = 2)
  
  testthat::expect_equal(result, data)
})

testthat::test_that("round_numeric_stats handles renamed columns", {
  data <- data.frame(
    Mean = c(1.12345, 2.67890),
    SD = c(0.12345, 0.67890),
    N = c(10, 20)
  )
  result <- saros:::round_numeric_stats(data, digits = 1)
  
  testthat::expect_equal(result$Mean, c(1.1, 2.7))
  testthat::expect_equal(result$SD, c(0.1, 0.7))
  testthat::expect_equal(result$N, c(10, 20))  # N should not be rounded
})

testthat::test_that("get_standard_column_renamer handles variable label columns", {
  renamer <- saros:::get_standard_column_renamer(main_question = "Main Q", use_header = TRUE)
  
  result1 <- renamer(".variable_label")
  result2 <- renamer(".variable_label_suffix")
  result3 <- renamer(".variable_name")
  
  testthat::expect_equal(result1, "Main Q")
  testthat::expect_equal(result2, "Main Q")
  testthat::expect_equal(result3, "Main Q")
})

testthat::test_that("get_standard_column_renamer handles standard columns", {
  renamer <- saros:::get_standard_column_renamer()
  
  testthat::expect_equal(renamer("n"), "N")
  testthat::expect_equal(renamer(".mean"), "Mean")
  testthat::expect_equal(renamer("sd"), "SD")
})

testthat::test_that("get_standard_column_renamer handles unknown columns", {
  renamer <- saros:::get_standard_column_renamer()
  
  testthat::expect_equal(renamer("unknown_col"), "unknown_col")
})