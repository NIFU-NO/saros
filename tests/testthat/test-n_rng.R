testthat::test_that("n_rng returns correct string for single dependent variable", {
  data <- data.frame(dep1 = c(1, NA, 3), indep1 = c(NA, 2, 3))
  result <- saros:::n_rng(data, dep = "dep1")
  testthat::expect_equal(result, "2")
})

testthat::test_that("n_rng returns correct string for multiple dependent variables", {
  data <- data.frame(dep1 = c(1, NA, 3), dep2 = c(2, 2, NA))
  result <- saros:::n_rng(data, dep = c("dep1", "dep2"))
  testthat::expect_equal(result, "2")
})

testthat::test_that("n_rng returns correct string for dependent and independent variables", {
  data <- data.frame(dep1 = c(1, NA, 3), indep1 = c(NA, 2, 3))
  result <- saros:::n_rng(data, dep = "dep1", indep = "indep1")
  testthat::expect_equal(result, "1")
})


testthat::test_that("n_rng returns 0 when no data", {
  data <- data.frame(dep1 = numeric(0), indep1 = numeric(0))
  result <- saros:::n_rng(
    data,
    dep = "dep1",
    indep = "indep1",
    glue_template_1 = "{n}"
  )
  testthat::expect_equal(result, "0")
})

testthat::test_that("n_rng returns 0 for all NA values in dependent variables", {
  data <- data.frame(dep1 = c(NA, NA, NA))
  result <- saros:::n_rng(data, dep = "dep1", glue_template_1 = "{n}")
  testthat::expect_equal(result, "0")
})

testthat::test_that("n_rng returns 0 for all NA values in independent variables", {
  data <- data.frame(dep1 = c(1, 2, 3), indep1 = c(NA, NA, NA))
  result <- saros:::n_rng(
    data,
    dep = "dep1",
    indep = "indep1",
    glue_template_1 = "{n}"
  )
  testthat::expect_equal(result, "0")
})

testthat::test_that("n_rng handles empty dependent variable list", {
  data <- data.frame(dep1 = c(1, 2, 3))
  result <- saros:::n_rng(data, dep = character(0), glue_template_1 = "{n}")
  testthat::expect_equal(result, "")
})

testthat::test_that("n_range2 validates plot is from makeme", {
  # Create a ggplot object NOT from makeme
  library(ggplot2)
  plot <- ggplot(saros::ex_survey, aes(x = x1_sex)) + geom_bar()

  # Should work without warning - fallback to counting complete cases
  result <- saros::n_range2(plot)

  # Should return the number of complete rows
  testthat::expect_type(result, "character")
  testthat::expect_true(nchar(result) > 0)

  # The count should match the number of complete cases in ex_survey
  n_complete <- nrow(saros::ex_survey[
    stats::complete.cases(saros::ex_survey),
  ])
  testthat::expect_equal(result, as.character(n_complete))
})

testthat::test_that("n_range2 works with valid makeme plot", {
  # Create a valid plot from makeme
  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  # Should work without error
  result <- saros::n_range2(plot)
  testthat::expect_type(result, "character")
  testthat::expect_true(nchar(result) > 0)
})

testthat::test_that("n_range2 handles all-NA .count_per_indep_group", {
  # Create a makeme plot and manually corrupt the data
  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  # Set all .count_per_indep_group values to NA
  plot$data$.count_per_indep_group <- NA_real_

  # Should warn about no valid sample sizes
  testthat::expect_warning(
    result <- saros::n_range2(plot),
    "No valid sample sizes"
  )

  # Should return "0"
  testthat::expect_equal(result, "0")
})

testthat::test_that("n_range2 calculates N per variable for int_plot_html with different NAs", {
  # Create test data where variables have different numbers of missing values
  test_data <- saros::ex_survey
  test_data$c_1[1:50] <- NA # 250 valid cases (300 - 50)
  # c_2 has 300 valid cases in ex_survey

  # Create int_plot_html
  plot <- saros::makeme(
    data = test_data,
    dep = c_1:c_2,
    type = "int_plot_html",
    label_separator = NULL
  )

  result <- saros::n_range2(plot)

  # Should report range [250-300], not total count
  testthat::expect_equal(result, "[250-300]")
})

testthat::test_that("n_range2 accounts for indep NAs in int_plot_html", {
  # Create test data with NAs in both dep and indep
  test_data <- saros::ex_survey
  test_data$c_1[1:50] <- NA
  test_data$x1_sex[51:60] <- NA

  plot <- saros::makeme(
    data = test_data,
    dep = c_1:c_2,
    indep = x1_sex,
    type = "int_plot_html",
    label_separator = NULL
  )

  result <- saros::n_range2(plot)

  # Should account for NAs in both variables
  # c_1: 250 valid, but 10 have NA in x1_sex = 240
  # c_2: 300 valid, but 10 have NA in x1_sex = 290
  testthat::expect_equal(result, "[240-290]")
})

testthat::test_that("n_range2 returns single value for single variable int_plot_html", {
  test_data <- saros::ex_survey
  test_data$c_1[1:50] <- NA

  plot <- saros::makeme(
    data = test_data,
    dep = c_1,
    type = "int_plot_html",
    label_separator = NULL
  )

  result <- saros::n_range2(plot)

  # Should return single value, not a range
  testthat::expect_equal(result, "250")
})

testthat::test_that("n_range2 handles empty .count_per_indep_group", {
  # Create a makeme plot and manually set empty vector
  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  # Remove all rows (edge case)
  plot$data <- plot$data[0, , drop = FALSE]

  # Should warn about no valid sample sizes
  testthat::expect_warning(
    result <- saros::n_range2(plot),
    "No valid sample sizes"
  )

  # Should return "0"
  testthat::expect_equal(result, "0")
})
