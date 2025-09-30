test_that("get_common_levels returns empty character for NULL col_pos", {
  data <- data.frame(
    a = factor(c("x", "y", "z")),
    b = factor(c("p", "q", "r"))
  )

  expect_equal(get_common_levels(data, col_pos = NULL), character())
})

test_that("get_common_levels works with single column", {
  data <- data.frame(
    a = factor(c("x", "y", "z"), levels = c("x", "y", "z", "w"))
  )

  result <- get_common_levels(data, col_pos = "a")
  expect_equal(result, c("x", "y", "z", "w"))
})

test_that("get_common_levels works with multiple columns", {
  data <- data.frame(
    a = factor(c("x", "y"), levels = c("x", "y", "z")),
    b = factor(c("p", "q"), levels = c("p", "q", "r"))
  )

  result <- get_common_levels(data, col_pos = c("a", "b"))
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("get_common_levels handles survey design objects", {
  skip_if_not_installed("survey")

  data <- data.frame(
    a = factor(c("x", "y", "z")),
    b = factor(c("p", "q", "r")),
    weights = c(1, 1, 1)
  )

  svy_data <- survey::svydesign(ids = ~1, weights = ~weights, data = data)

  result <- get_common_levels(svy_data, col_pos = "a")
  expect_equal(result, c("x", "y", "z"))
})

test_that("get_common_data_type returns single type for homogeneous data", {
  data <- data.frame(
    a = factor(c("x", "y")),
    b = factor(c("p", "q"))
  )

  result <- get_common_data_type(data, col_pos = c("a", "b"))
  expect_equal(result, "factor")
})

test_that("get_common_data_type handles mixed factor types", {
  data <- data.frame(
    a = factor(c("x", "y")),
    b = ordered(c("p", "q"))
  )

  result <- get_common_data_type(data, col_pos = c("a", "b"))
  expect_equal(result, "factor")
})

test_that("get_common_data_type returns integer for mixed types", {
  data <- data.frame(
    a = factor(c("x", "y")),
    b = c(1, 2)
  )

  result <- get_common_data_type(data, col_pos = c("a", "b"))
  expect_equal(result, "integer")
})

test_that("get_common_data_type handles single column", {
  data <- data.frame(
    a = c(1, 2, 3)
  )

  result <- get_common_data_type(data, col_pos = "a")
  expect_equal(result, "numeric")
})
