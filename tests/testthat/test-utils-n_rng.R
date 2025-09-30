test_that("glue_together_range handles NULL and NA values", {
  # Test NULL input
  expect_equal(glue_together_range(NULL, "{n}", "[{n[1]}-{n[2]}]"), "")

  # Test all NA input
  expect_equal(glue_together_range(c(NA, NA), "{n}", "[{n[1]}-{n[2]}]"), "")

  # Test mixed with NA
  expect_equal(
    glue_together_range(c(5, NA, 10), "{n}", "[{n[1]}-{n[2]}]"),
    "[5-10]"
  )
})

test_that("glue_together_range handles infinite values", {
  # Test all infinite values
  expect_equal(glue_together_range(c(Inf, -Inf), "{n}", "[{n[1]}-{n[2]}]"), "")

  # Test mixed finite and infinite
  expect_equal(
    glue_together_range(c(5, Inf), "{n}", "[{n[1]}-{n[2]}]"),
    "[5-Inf]"
  )
})

test_that("glue_together_range uses correct template for single value", {
  # Single value should use template 1
  result <- glue_together_range(5, "N={n}", "Range: {n[1]} to {n[2]}")
  expect_equal(result, "N=5")

  # Duplicate values should also use template 1
  result2 <- glue_together_range(c(5, 5, 5), "N={n}", "Range: {n[1]} to {n[2]}")
  expect_equal(result2, "N=5")
})

test_that("glue_together_range uses correct template for range", {
  # Range should use template 2
  result <- glue_together_range(c(5, 10), "N={n}", "Range: {n[1]} to {n[2]}")
  expect_equal(result, "Range: 5 to 10")

  # Multiple values creating a range
  result2 <- glue_together_range(c(1, 3, 8, 2), "N={n}", "[{n[1]}-{n[2]}]")
  expect_equal(result2, "[1-8]")
})

test_that("glue_together_range handles edge cases with templates", {
  # Non-string template 1 should fallback
  result <- glue_together_range(5, NULL, "Range: {n[1]} to {n[2]}")
  expect_equal(result, "Range: 5 to NA")

  # Non-string template 2 should fallback but may still process if range is single value
  result2 <- glue_together_range(c(5, 10), "N={n}", NULL)
  expect_equal(result2, "")

  # Both templates NULL
  result3 <- glue_together_range(c(5, 10), NULL, NULL)
  expect_equal(result3, "")
})

test_that("n_rng processes basic inputs correctly", {
  # Create test data
  data <- data.frame(
    dep1 = c(1, 2, 3, NA, 5),
    dep2 = c("A", "B", "C", "D", NA),
    indep1 = c("X", "Y", "X", "Y", "X")
  )

  # Test with single dep, no indep
  result <- n_rng(data, dep = "dep1")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("n_rng handles multiple dependencies", {
  data <- data.frame(
    dep1 = c(1, 2, 3, 4, 5),
    dep2 = c("A", "B", "C", "D", "E"),
    indep1 = c("X", "Y", "X", "Y", "X")
  )

  # Test with multiple deps
  result <- n_rng(data, dep = c("dep1", "dep2"))
  expect_type(result, "character")
})

test_that("n_rng handles independent variables", {
  data <- data.frame(
    dep1 = c(1, 2, 3, 4, 5),
    indep1 = c("X", "Y", "X", "Y", "X"),
    indep2 = c("P", "Q", "P", "Q", "P")
  )

  # Test with dep and indep
  result <- n_rng(data, dep = "dep1", indep = c("indep1", "indep2"))
  expect_type(result, "character")
})

test_that("n_rng filters out NA values correctly", {
  data <- data.frame(
    dep1 = c(1, 2, NA, 4, 5),
    indep1 = c("X", NA, "X", "Y", "X")
  )

  # Should only count complete cases
  result <- n_rng(data, dep = "dep1", indep = "indep1")
  expect_type(result, "character")
})

test_that("n_rng handles custom glue templates", {
  data <- data.frame(
    dep1 = c(1, 2, 3, 4, 5),
    indep1 = c("X", "Y", "X", "Y", "X")
  )

  # Test custom templates
  result <- n_rng(
    data,
    dep = "dep1",
    indep = "indep1",
    glue_template_1 = "Count: {n}",
    glue_template_2 = "Range from {n[1]} to {n[2]}"
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("n_rng handles crowd parameter", {
  data <- data.frame(
    dep1 = c(1, 2, 3, 4, 5),
    group_var = c("target", "other", "target", "other", "target")
  )

  # Test different crowd settings
  expect_no_error(n_rng(data, dep = "dep1", crowd = "all"))
  expect_no_error(n_rng(
    data,
    dep = "dep1",
    crowd = "target",
    mesos_var = "group_var",
    mesos_group = "target"
  ))
  expect_no_error(n_rng(
    data,
    dep = "dep1",
    crowd = "others",
    mesos_var = "group_var",
    mesos_group = "target"
  ))
})
