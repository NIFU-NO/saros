testthat::test_that("output_format returns 'officer' outside knitr context", {
  result <- saros::output_format()
  testthat::expect_equal(result, "officer")
})

testthat::test_that("output_format returns pandoc format when inside knitr", {
  testthat::local_mocked_bindings(
    pandoc_to = function(...) "html",
    .package = "knitr"
  )
  result <- saros::output_format()
  testthat::expect_equal(result, "html")
})

testthat::test_that("output_format returns docx when pandoc_to says docx", {
  testthat::local_mocked_bindings(
    pandoc_to = function(...) "docx",
    .package = "knitr"
  )
  result <- saros::output_format()
  testthat::expect_equal(result, "docx")
})
