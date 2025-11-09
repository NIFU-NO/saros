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
