testthat::test_that("simple_descriptives works with numeric y_var without x_var", {
  data <- data.frame(y_var = c(1, 2, 3, 4, 5))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(all(
    c("mean", "sd", "median", "mad") %in% names(result)
  ))
})

testthat::test_that("simple_descriptives works with numeric y_var and x_var", {
  data <- data.frame(
    y_var = c(1, 2, 3, 4, 5),
    x_var = c("A", "A", "B", "B", "B")
  )
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var"
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, c(1.5, 4.0))
  testthat::expect_equal(result$n, c(2, 3))
})

testthat::test_that("simple_descriptives removes NA in x_var if na.rm = TRUE", {
  data <- data.frame(
    y_var = c(1, 2, 3, 4, 5),
    x_var = c("A", NA, "B", "B", "B")
  )
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    na.rm = TRUE
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 15)
})

testthat::test_that("simple_descriptives does not remove NA in x_var if na.rm = FALSE", {
  data <- data.frame(
    y_var = c(1, 2, 3, 4, 5),
    x_var = c("A", NA, "B", "B", "B")
  )
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    na.rm = FALSE
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 15)
  testthat::expect_equal(nrow(result), 3)
})

testthat::test_that("simple_descriptives returns data frame if y_var is categorical", {
  data <- data.frame(y_var = c("a", "b", "c"), x_var = c("A", "B", "B"))
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    table_wide = F
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(dim(result), c(2, 7))
})

testthat::test_that("simple_descriptives handles single row input", {
  data <- data.frame(y_var = c(1))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, 1)
})
testthat::test_that("simple_descriptives handles single row input", {
  data <- data.frame(y_var = c(1), x_var = c("A"))
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var"
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, 1)
})

testthat::test_that("simple_descriptives handles missing y_var gracefully", {
  data <- data.frame(x_var = c("A", "B", "B"))
  testthat::expect_error(saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var"
  ))
})

testthat::test_that("simple_descriptives works with numeric y_var and no na.rm", {
  data <- data.frame(
    y_var = c(1, 2, 3, 4, NA),
    x_var = c("A", "A", "B", "B", "B")
  )
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var"
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(result$mean, c(1.5, 3.5))
})

testthat::test_that("simple_descriptives handles missing x_var correctly", {
  data <- data.frame(y_var = c(1, 2, 3, 4, 5))
  result <- saros:::simple_descriptives(data = data, y_var = "y_var")
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(all(
    c("mean", "sd", "median", "mad") %in% names(result)
  ))
})

testthat::test_that("simple_descriptives keeps x_var when it has many categories (#603)", {
  data <- data.frame(
    y_var = rep(c("a", "b"), each = 10),
    x_var = rep(LETTERS[1:10], times = 2)
  )
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var"
  )
  testthat::expect_true("x_var" %in% names(result))
  testthat::expect_equal(nrow(result), 10)
})

testthat::test_that("simple_descriptives does not count NA towards the categories of x_var (#603)", {
  # Five observed categories plus NA used to be counted as six and silently
  # dropped the grouping, which then broke the wide pivot below.
  data <- data.frame(
    y_var = rep(c("a", "b"), each = 6),
    x_var = c(LETTERS[1:5], NA, LETTERS[1:5], NA)
  )
  long <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    na.rm = TRUE
  )
  testthat::expect_true("x_var" %in% names(long))
  testthat::expect_setequal(long$x_var, LETTERS[1:5])

  wide <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    na.rm = TRUE,
    table_wide = TRUE
  )
  testthat::expect_equal(nrow(wide), 1)
  testthat::expect_true(all(paste0("n_valid_", LETTERS[1:5]) %in% names(wide)))
})

testthat::test_that("simple_descriptives keeps NA as its own group when na.rm = FALSE (#603)", {
  data <- data.frame(
    y_var = rep(c("a", "b"), each = 6),
    x_var = c(LETTERS[1:5], NA, LETTERS[1:5], NA)
  )
  wide <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    na.rm = FALSE,
    table_wide = TRUE
  )
  testthat::expect_true("n_valid_NA" %in% names(wide))
})

testthat::test_that("simple_descriptives returns long format above n_categories_limit", {
  data <- data.frame(
    y_var = rep(c("a", "b"), each = 10),
    x_var = rep(LETTERS[1:10], times = 2)
  )
  wide <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    table_wide = TRUE,
    n_categories_limit = 12
  )
  testthat::expect_equal(nrow(wide), 1)

  long <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    table_wide = TRUE,
    n_categories_limit = 5
  )
  testthat::expect_equal(nrow(long), 10)
  testthat::expect_true("x_var" %in% names(long))
})

testthat::test_that("simple_descriptives summarises a variable grouped by itself ungrouped", {
  data <- data.frame(x_var = rep(LETTERS[1:3], times = 4))
  result <- saros:::simple_descriptives(
    data = data,
    y_var = "x_var",
    x_var = "x_var",
    table_wide = TRUE
  )
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$n, 12)
})

testthat::test_that("simple_descriptives keeps variable labels when dropping NA rows in x_var", {
  # `[.data.frame` drops the plain `label` attribute that
  # labelled::var_label() sets on a bare vector, so the NA filtering must run
  # after the labels have been read.
  data <- data.frame(
    y_var = c(1, 2, 3, 4, 5),
    x_var = c("A", NA, "B", "B", "B")
  )
  attr(data$y_var, "label") <- "A labelled variable"

  result <- saros:::simple_descriptives(
    data = data,
    y_var = "y_var",
    x_var = "x_var",
    na.rm = TRUE
  )
  testthat::expect_equal(
    as.character(unique(result$.variable_label)),
    "A labelled variable"
  )
})
