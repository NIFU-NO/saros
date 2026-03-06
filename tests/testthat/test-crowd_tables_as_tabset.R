testthat::test_that("crowd_tables_as_tabset outputs headings and tables", {
  tbl_list <- list(
    "Group A" = head(mtcars, 2),
    "Group B" = tail(mtcars, 2)
  )
  out <- testthat::capture_output(saros::crowd_tables_as_tabset(tbl_list))
  testthat::expect_match(out, "##### Group A")
  testthat::expect_match(out, "##### Group B")
})

testthat::test_that("crowd_tables_as_tabset uses custom table_fn", {
  tbl_list <- list("Test" = data.frame(x = 1:3))
  custom_fn <- function(df) paste("custom:", nrow(df), "rows")
  out <- testthat::capture_output(
    saros::crowd_tables_as_tabset(tbl_list, table_fn = custom_fn)
  )
  testthat::expect_match(out, "##### Test")
  testthat::expect_match(out, "custom: 3 rows")
})

testthat::test_that("crowd_tables_as_tabset handles single-element list", {
  tbl_list <- list("Only" = data.frame(a = 1))
  out <- testthat::capture_output(saros::crowd_tables_as_tabset(tbl_list))
  testthat::expect_match(out, "##### Only")
})

testthat::test_that("crowd_tables_as_tabset handles empty list", {
  out <- testthat::capture_output(saros::crowd_tables_as_tabset(list()))
  testthat::expect_equal(out, "")
})
