testthat::test_that("prepare_chunk", {
  testthat::expect_warning(saros:::prepare_chunk("nothing_like_this_exists"),
                           regexp = "Invalid element_name")
  testthat::expect_match(saros:::prepare_chunk.hline(), "--|__")

})
