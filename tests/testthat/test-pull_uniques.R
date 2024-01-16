testthat::test_that("pull_uniques", {
  testthat::expect_equal(saros:::pull_uniques(c("a", "a", "b", NA)), c("a", "b"))
  testthat::expect_equal(saros:::pull_uniques(factor(c("a", "a", "b", NA))), c("a", "b"))
})
