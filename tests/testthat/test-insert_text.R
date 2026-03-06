test_data <- data.frame(
  chunk = c("intro", "intro", "methods", "summary"),
  before = c(TRUE, FALSE, TRUE, TRUE),
  text = c("Before intro.", "After intro.", "Before methods.", "Summary text."),
  stringsAsFactors = FALSE
)

testthat::test_that("insert_text returns correct text for before=TRUE", {
  result <- saros::insert_text(test_data, "intro", before = TRUE)
  testthat::expect_equal(as.character(result), "Before intro.")
  testthat::expect_s3_class(result, "AsIs")
})

testthat::test_that("insert_text returns correct text for before=FALSE", {
  result <- saros::insert_text(test_data, "intro", before = FALSE)
  testthat::expect_equal(as.character(result), "After intro.")
})

testthat::test_that("insert_text returns empty AsIs when chunk not found and error_on_empty is NULL", {
  result <- saros::insert_text(test_data, "nonexistent", error_on_empty = NULL)
  testthat::expect_length(result, 0)
  testthat::expect_s3_class(result, "AsIs")
})

testthat::test_that("insert_text errors when chunk not found and error_on_empty is TRUE", {
  testthat::expect_error(
    saros::insert_text(test_data, "nonexistent", error_on_empty = TRUE)
  )
})

testthat::test_that("insert_text warns when chunk not found and error_on_empty is FALSE", {
  testthat::expect_warning(
    saros::insert_text(test_data, "nonexistent", error_on_empty = FALSE)
  )
})

testthat::test_that("insert_text errors on duplicate chunk-text entries", {
  dup_data <- data.frame(
    chunk = c("intro", "intro"),
    before = c(TRUE, TRUE),
    text = c("First.", "Second."),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    saros::insert_text(dup_data, "intro", before = TRUE),
    "Duplicate"
  )
})

testthat::test_that("insert_text strips rmarkdown extension from chunk name", {
  result <- saros::insert_text(test_data, "intro.rmarkdown", before = TRUE)
  testthat::expect_equal(as.character(result), "Before intro.")
})

testthat::test_that("insert_text returns empty immediately when enabled=FALSE", {
  result <- saros::insert_text(test_data, "intro", enabled = FALSE)
  testthat::expect_length(result, 0)
  testthat::expect_s3_class(result, "AsIs")
})

testthat::test_that("insert_text expands knitr templates", {
  tmpl_data <- data.frame(
    chunk = "tmpl",
    before = TRUE,
    text = "Value is {{1 + 1}}.",
    stringsAsFactors = FALSE
  )
  result <- saros::insert_text(tmpl_data, "tmpl", before = TRUE)
  testthat::expect_equal(as.character(result), "Value is 2.")
})
