testthat::test_that("get_common_name", {
  x <- saros:::get_common_name(c("apple_b1", "apple_b2", "apple_b3"))
  testthat::expect_equal(x, "aple_b")
  x <- saros:::get_common_name(c("apple_b1", "apple_b2", NA))
  testthat::expect_equal(x, c("apple_b1", "apple_b2", NA))
  x <- saros:::get_common_name(c("apple_b1", "apple_b2", "ap"))
  testthat::expect_equal(x, c("ap"))

})
