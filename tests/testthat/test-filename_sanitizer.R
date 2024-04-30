testthat::test_that("filename_sanitizer", {
  testthat::expect_equal(saros:::filename_sanitizer("alphabet"), "alphabet")
  testthat::expect_equal(saros:::filename_sanitizer("alphabet", 5), "alpha")
  testthat::expect_equal(saros:::filename_sanitizer("alpha 5%&*^!\"¤\\/()[]{}=?+"),
                         "alpha_5_+")
})
