test_that("find_test2 handles one-sample numeric tests", {
  # One-sample t-test for numeric data
  result <- saros:::find_test2(c(1, 2, 3, 4, 5))

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c(".bi_test", ".p_value", "y_type", "x_type"))
  expect_equal(result$.bi_test, "One-sample t-test")
  expect_equal(result$y_type, "numeric")
  expect_true(is.na(result$x_type))
  expect_type(result$.p_value, "double")
  expect_false(is.na(result$.p_value))
})

test_that("find_test2 handles one-sample factor tests", {
  # Chi-squared goodness-of-fit test for factor data
  # May produce warning about chi-squared approximation
  suppressWarnings(
    result <- saros:::find_test2(factor(c("a", "b", "a", "c", "b")))
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$.bi_test, "Chi-squared Goodness-of-Fit Test")
  expect_equal(result$y_type, "factor")
  expect_true(is.na(result$x_type))
  expect_type(result$.p_value, "double")
})

test_that("find_test2 handles ANOVA (numeric ~ factor)", {
  y <- c(1, 2, 3, 4, 5, 6)
  x <- factor(c("A", "A", "B", "B", "C", "C"))
  result <- saros:::find_test2(y, x)

  expect_equal(result$.bi_test, "ANOVA")
  expect_type(result$.p_value, "double")
})

test_that("find_test2 handles factor ~ numeric ANOVA", {
  y <- factor(c("A", "A", "B", "B", "C", "C"))
  x <- c(1, 2, 3, 4, 5, 6)
  result <- saros:::find_test2(y, x)

  expect_equal(result$.bi_test, "ANOVA")
  expect_type(result$.p_value, "double")
})

test_that("find_test2 handles chi-squared test (factor ~ factor)", {
  y <- factor(c("A", "A", "B", "B", "C", "C"))
  x <- factor(c("X", "Y", "X", "Y", "X", "Y"))
  result <- saros:::find_test2(y, x)

  expect_equal(result$.bi_test, "Chi-squared Goodness-of-Fit Test")
  expect_type(result$.p_value, "double")
})

test_that("find_test2 handles correlation test (numeric ~ numeric)", {
  y <- c(1, 2, 3, 4, 5)
  x <- c(2, 4, 6, 8, 10)
  result <- saros:::find_test2(y, x)

  expect_equal(result$.bi_test, "Pearson Correlation")
  expect_type(result$.p_value, "double")
})

test_that("find_test2 handles Kruskal-Wallis test", {
  y <- ordered(c("Low", "Med", "High", "Low", "Med", "High"))
  x <- factor(c("A", "A", "B", "B", "C", "C"))
  result <- saros:::find_test2(y, x)

  expect_equal(result$.bi_test, "Kruskal-Wallis chisq")
  expect_type(result$.p_value, "double")
})

test_that("find_test2 handles Spearman correlation (ordered ~ numeric)", {
  y <- ordered(c("Low", "Med", "High", "Low", "Med"))
  x <- c(1, 2, 3, 1, 2)
  result <- saros:::find_test2(y, x)

  expect_equal(result$.bi_test, "Spearman Rank Correlation")
  expect_type(result$.p_value, "double")
})

test_that("find_test2 handles errors gracefully", {
  # Test with insufficient data
  y <- c(1)
  x <- c(1)
  result <- saros:::find_test2(y, x)

  expect_true(is.na(result$.p_value))
  expect_type(result$.bi_test, "character")
})

test_that("find_test2 handles NA values", {
  y <- c(1, 2, NA, 4, 5)
  x <- c(2, 4, 6, NA, 10)
  result <- saros:::find_test2(y, x)

  expect_equal(result$.bi_test, "Pearson Correlation")
  # Should still work with complete.obs
  expect_type(result$.p_value, "double")
})
