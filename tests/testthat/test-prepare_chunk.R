testthat::test_that("prepare_chunk", {
  testthat::expect_error(saros:::prepare_chunk("nothing_like_this_exists"))
  testthat::expect_match(saros:::prepare_chunk.hline(), "--|__")

})
