testthat::test_that("simple_descriptives works with numeric y_var without x_var", {
  data <- data.frame(y_var = c(1, 2, 3, 4, 5))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(all(c("mean", "sd", "median", "mad") %in% names(result)))
})

testthat::test_that("simple_descriptives works with numeric y_var and x_var", {
  data <- data.frame(y_var = c(1, 2, 3, 4, 5), x_var = c("A", "A", "B", "B", "B"))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var", x_var = "x_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, c(1.5, 4.0))
  testthat::expect_equal(result$n, c(2, 3))
})

testthat::test_that("simple_descriptives removes NA in x_var if na.rm = TRUE", {
  data <- data.frame(y_var = c(1, 2, 3, 4, 5), x_var = c("A", NA, "B", "B", "B"))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var", x_var = "x_var", na.rm = TRUE)
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 11)
})

testthat::test_that("simple_descriptives does not remove NA in x_var if na.rm = FALSE", {
  data <- data.frame(y_var = c(1, 2, 3, 4, 5), x_var = c("A", NA, "B", "B", "B"))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var", x_var = "x_var", na.rm = FALSE)
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 11)
  testthat::expect_equal(nrow(result), 3)
})

testthat::test_that("simple_descriptives returns data frame if y_var is categorical", {
  data <- data.frame(y_var = c("a", "b", "c"), x_var = c("A", "B", "B"))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var", x_var = "x_var", table_wide = F)
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(dim(result), c(2, 3))
})

testthat::test_that("simple_descriptives handles single row input", {
  data <- data.frame(y_var = c(1))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, 1)
})
testthat::test_that("simple_descriptives handles single row input", {
  data <- data.frame(y_var = c(1), x_var = c("A"))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var", x_var = "x_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, 1)
})

testthat::test_that("simple_descriptives handles missing y_var gracefully", {
  data <- data.frame(x_var = c("A", "B", "B"))
  testthat::expect_error(saros:::simple_descriptives(data = data, y_var = "y_var", x_var = "x_var"))
})

testthat::test_that("simple_descriptives works with numeric y_var and no na.rm", {
  data <- data.frame(y_var = c(1, 2, 3, 4, NA), x_var = c("A", "A", "B", "B", "B"))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var", x_var = "x_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, c(1.5, 3.5))
})

testthat::test_that("simple_descriptives handles missing x_var correctly", {
  data <- data.frame(y_var = c(1, 2, 3, 4, 5))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(all(c("mean", "sd", "median", "mad") %in% names(result)))
})
