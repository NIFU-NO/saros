testthat::test_that("n_rng returns correct string for single dependent variable", {
  data <- data.frame(dep1 = c(1, NA, 3),
                     indep1 = c(NA, 2, 3))
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
  result <- saros:::n_rng(data, dep = "dep1", indep = "indep1", glue_template_1 = "{n}")
  testthat::expect_equal(result, "0")
})

testthat::test_that("n_rng returns 0 for all NA values in dependent variables", {
  data <- data.frame(dep1 = c(NA, NA, NA))
  result <- saros:::n_rng(data, dep = "dep1", glue_template_1 = "{n}")
  testthat::expect_equal(result, "0")
})

testthat::test_that("n_rng returns 0 for all NA values in independent variables", {
  data <- data.frame(dep1 = c(1, 2, 3), indep1 = c(NA, NA, NA))
  result <- saros:::n_rng(data, dep = "dep1", indep = "indep1", glue_template_1 = "{n}")
  testthat::expect_equal(result, "0")
})

testthat::test_that("n_rng handles empty dependent variable list", {
  data <- data.frame(dep1 = c(1, 2, 3))
  result <- saros:::n_rng(data, dep = character(0), glue_template_1 = "{n}")
  testthat::expect_equal(result, "")
})
