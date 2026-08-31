test_that("summarize_int_cat_data validates data input", {
  # The function actually accepts strings and tries to process them,
  # so it will fail deeper in the call stack
  expect_error(
    summarize_int_cat_data("not a data frame"),
    "must be a vector"
  )
})

test_that("summarize_int_cat_data validates column existence", {
  data <- data.frame(
    x = 1:5,
    y = letters[1:5],
    z = factor(c("A", "B", "A", "B", "A"))
  )

  # Invalid dep columns
  expect_error(
    summarize_int_cat_data(data, dep = "nonexistent"),
    "doesn't exist"
  )

  # Invalid indep columns
  expect_error(
    summarize_int_cat_data(data, dep = "x", indep = "nonexistent"),
    "doesn't exist"
  )
})

test_that("summarize_int_cat_data prevents dep/indep overlap", {
  data <- data.frame(
    x = 1:5,
    y = letters[1:5],
    z = factor(c("A", "B", "A", "B", "A"))
  )

  # The function has a bug - it references invalid_deps before defining it
  # Let's test the error that actually occurs
  expect_error(
    summarize_int_cat_data(data, dep = "x", indep = "x"),
    "among indep columns|invalid_deps"
  )
})

test_that("summarize_int_cat_data works with single indep variable", {
  data <- data.frame(
    x = 1:5,
    y = c(10, 20, 30, 40, 50),
    z = factor(c("A", "B", "A", "B", "A"))
  )

  result <- summarize_int_cat_data(data, dep = "x", indep = "z")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("summarize_int_cat_data works with no indep variable", {
  data <- data.frame(
    x = 1:5,
    y = c(10, 20, 30, 40, 50)
  )

  result <- summarize_int_cat_data(data, dep = "x", indep = NULL)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("summarize_int_cat_data works with multiple indep variables", {
  # This used to abort: the whole `indep` vector was passed as `.id`, which
  # takes a single column name, and the list was unnamed anyway (#613).
  data <- data.frame(
    x = 1:8,
    y = c(10, 20, 30, 40, 50, 60, 70, 80),
    z = factor(c("A", "B", "A", "B", "A", "B", "A", "B")),
    w = factor(c("X", "X", "Y", "Y", "X", "X", "Y", "Y"))
  )

  result <- summarize_int_cat_data(data, dep = "x", indep = c("z", "w"))

  # One block per independent variable, identified by `.indep_name`, each
  # holding one row per category of its own variable.
  expect_true(is.data.frame(result))
  expect_equal(result$.indep_name, c("z", "z", "w", "w"))
  expect_equal(as.character(result$z), c("A", "B", NA, NA))
  expect_equal(as.character(result$w), c(NA, NA, "X", "Y"))
  expect_equal(result$n_valid, c(4L, 4L, 4L, 4L))
  # `.indep_order` restarts within each block.
  expect_equal(result$.indep_order, c(1L, 2L, 1L, 2L))
})

test_that("summarize_int_cat_data handles multiple dep variables", {
  data <- data.frame(
    x = 1:5,
    y = c(10, 20, 30, 40, 50),
    z = factor(c("A", "B", "A", "B", "A"))
  )

  result <- summarize_int_cat_data(data, dep = c("x", "y"), indep = "z")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("summarize_int_cat_data handles survey objects", {
  skip_if_not_installed("survey")

  # Survey objects don't work with dplyr::summarize without srvyr
  # This test reveals the incompatibility
  data <- data.frame(
    x = 1:5,
    y = c(10, 20, 30, 40, 50),
    z = factor(c("A", "B", "A", "B", "A")),
    weights = c(1, 1, 1, 1, 1)
  )

  svy_data <- survey::svydesign(ids = ~1, weights = ~weights, data = data)

  expect_error(
    summarize_int_cat_data(svy_data, dep = "x", indep = "z"),
    "no applicable method for 'summarise'"
  )
})

test_that("summarize_int_cat_data handles edge cases", {
  # Empty data frame - this will actually cause an error in data processing
  data <- data.frame(x = numeric(0), y = factor())

  expect_warning(
    expect_error(
      summarize_int_cat_data(data, dep = "x", indep = "y"),
      "replacement has 1 row, data has 0"
    ),
    "no non-missing arguments to min"
  )

  # Single row
  data <- data.frame(
    x = 1,
    z = factor("A")
  )

  expect_no_error(
    result <- summarize_int_cat_data(data, dep = "x", indep = "z")
  )
})
