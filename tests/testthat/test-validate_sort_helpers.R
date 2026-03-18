# validate_sort_column() -------------------------------------------------------

testthat::test_that("validate_sort_column passes for NULL sort_by", {
  df <- data.frame(.count = 1:3)
  testthat::expect_invisible(saros:::validate_sort_column(NULL, df))
})

testthat::test_that("validate_sort_column passes for column present in data", {
  df <- data.frame(.count = 1:3, .proportion = 0.1 * 1:3)
  testthat::expect_invisible(saros:::validate_sort_column(".count", df))
})

testthat::test_that("validate_sort_column passes for non-whitelisted sort_by", {
  df <- data.frame(x = 1:3)
  # ".variable_position" is not in allowed_dep_sort_columns, so it should pass through
  testthat::expect_invisible(saros:::validate_sort_column(".variable_position", df))
})

testthat::test_that("validate_sort_column errors for missing whitelisted column", {
  df <- data.frame(.proportion = 0.1 * 1:3)
  testthat::expect_error(
    saros:::validate_sort_column(".count", df,
      allowed = c(".count", ".proportion")
    ),
    regexp = "not found in data"
  )
})

testthat::test_that("validate_sort_column error includes available columns", {
  df <- data.frame(.proportion = 0.1 * 1:3)
  testthat::expect_error(
    saros:::validate_sort_column(".count", df,
      allowed = c(".count", ".proportion")
    ),
    regexp = "\\.proportion"
  )
})

testthat::test_that("validate_sort_column error handles no available columns", {
  df <- data.frame(x = 1:3)
  testthat::expect_error(
    saros:::validate_sort_column(".count", df,
      allowed = c(".count", ".proportion")
    ),
    regexp = "None of the sortable columns"
  )
})

# validate_sort_category() -----------------------------------------------------

testthat::test_that("validate_sort_category passes for NULL sort_by", {
  df <- data.frame(.category = c("A", "B", "C"))
  testthat::expect_invisible(saros:::validate_sort_category(NULL, df))
})

testthat::test_that("validate_sort_category passes for dot-prefixed methods", {
  df <- data.frame(.category = c("A", "B"))
  testthat::expect_invisible(saros:::validate_sort_category(".upper", df))
  testthat::expect_invisible(saros:::validate_sort_category(".variable_position", df))
})

testthat::test_that("validate_sort_category passes for categories present in data", {
  df <- data.frame(.category = c("Agree", "Disagree", "Neutral"))
  testthat::expect_invisible(saros:::validate_sort_category("Agree", df))
  testthat::expect_invisible(saros:::validate_sort_category(c("Agree", "Disagree"), df))
})

testthat::test_that("validate_sort_category errors for missing categories", {
  df <- data.frame(.category = c("Agree", "Disagree", "Neutral"))
  testthat::expect_error(
    saros:::validate_sort_category("Strongly Agree", df),
    regexp = "not found in data"
  )
})

testthat::test_that("validate_sort_category error lists missing and available categories", {
  df <- data.frame(.category = c("Agree", "Disagree", "Neutral"))
  testthat::expect_error(
    saros:::validate_sort_category(c("Agree", "Strongly Agree"), df),
    regexp = "Strongly Agree"
  )
  testthat::expect_error(
    saros:::validate_sort_category(c("Agree", "Strongly Agree"), df),
    regexp = "Agree.*Disagree.*Neutral"
  )
})

testthat::test_that("validate_sort_category passes when data has no .category column", {
  df <- data.frame(x = 1:3)
  testthat::expect_invisible(saros:::validate_sort_category("foo", df))
})
