testthat::test_that("check_sort_by accepts NULL", {
  x <- factor(c("A", "B", "C"))
  testthat::expect_no_error(
    saros:::check_sort_by(x, sort_by = NULL)
  )
})

testthat::test_that("check_sort_by accepts valid set options", {
  # Skip if .saros.env is not available
  testthat::skip_if_not(exists(".saros.env", envir = globalenv()) || 
                       exists(".saros.env", envir = asNamespace("saros")))
  
  x <- factor(c("A", "B", "C"))
  
  # Test with first summary data sort option if available
  if (exists(".saros.env", envir = asNamespace("saros"))) {
    env <- get(".saros.env", envir = asNamespace("saros"))
    if (length(env$summary_data_sort1) > 0) {
      testthat::expect_no_error(
        saros:::check_sort_by(x, sort_by = env$summary_data_sort1[1])
      )
    }
  }
})

testthat::test_that("check_sort_by accepts categories present in data", {
  x <- factor(c("Category1", "Category2", "Category3"))
  testthat::expect_no_error(
    saros:::check_sort_by(x, sort_by = c("Category1", "Category2"))
  )
})

testthat::test_that("check_sort_by accepts single category from data", {
  x <- factor(c("A", "B", "C"))
  testthat::expect_no_error(
    saros:::check_sort_by(x, sort_by = "A")
  )
})

testthat::test_that("check_sort_by errors on invalid category", {
  x <- factor(c("A", "B", "C"))
  testthat::expect_error(
    saros:::check_sort_by(x, sort_by = "D"),
    regexp = "Invalid.*sort_by"
  )
})

testthat::test_that("check_sort_by errors on mixed valid and invalid categories", {
  x <- factor(c("A", "B", "C"))
  testthat::expect_error(
    saros:::check_sort_by(x, sort_by = c("A", "D")),
    regexp = "Invalid.*sort_by"
  )
})

testthat::test_that("check_sort_by errors on empty character vector", {
  x <- factor(c("A", "B", "C"))
  testthat::expect_error(
    saros:::check_sort_by(x, sort_by = character(0)),
    regexp = "Invalid.*sort_by"
  )
})

testthat::test_that("check_sort_by handles numeric input converted to character", {
  x <- c(1, 2, 3)  # Will be converted to character
  testthat::expect_no_error(
    saros:::check_sort_by(x, sort_by = "1")
  )
})

testthat::test_that("check_sort_by handles character vector input", {
  x <- c("red", "blue", "green")
  testthat::expect_no_error(
    saros:::check_sort_by(x, sort_by = c("red", "blue"))
  )
})

testthat::test_that("check_sort_by errors with helpful message", {
  x <- factor(c("A", "B", "C"))
  testthat::expect_error(
    saros:::check_sort_by(x, sort_by = "invalid"),
    regexp = "must be either NULL.*or all valid categories"
  )
})

testthat::test_that("check_sort_by handles empty data", {
  x <- factor(character(0))
  testthat::expect_error(
    saros:::check_sort_by(x, sort_by = "anything"),
    regexp = "Invalid.*sort_by"
  )
})

testthat::test_that("check_sort_by handles data with NA values", {
  x <- factor(c("A", NA, "B"))
  testthat::expect_no_error(
    saros:::check_sort_by(x, sort_by = "A")
  )
})

testthat::test_that("check_sort_by errors on non-character sort_by", {
  x <- factor(c("A", "B", "C"))
  testthat::expect_error(
    saros:::check_sort_by(x, sort_by = 1),
    regexp = "Invalid.*sort_by"
  )
})