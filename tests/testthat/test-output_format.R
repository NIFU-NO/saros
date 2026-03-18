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

# is_html_output_or_officer() --------------------------------------------------

testthat::test_that("is_html_output_or_officer returns TRUE outside knitr", {
  withr::local_options(knitr.in.progress = NULL)
  testthat::expect_true(saros:::is_html_output_or_officer())
})

testthat::test_that("is_html_output_or_officer returns TRUE for Typst", {
  withr::local_options(knitr.in.progress = TRUE)
  testthat::local_mocked_bindings(
    pandoc_to = function(...) "typst",
    is_html_output = function(...) FALSE,
    .package = "knitr"
  )
  testthat::expect_true(saros:::is_html_output_or_officer())
})

testthat::test_that("is_html_output_or_officer returns TRUE for HTML", {
  withr::local_options(knitr.in.progress = TRUE)
  testthat::local_mocked_bindings(
    pandoc_to = function(...) "html",
    is_html_output = function(...) TRUE,
    .package = "knitr"
  )
  testthat::expect_true(saros:::is_html_output_or_officer())
})

testthat::test_that("is_html_output_or_officer returns FALSE for LaTeX/PDF", {
  withr::local_options(knitr.in.progress = TRUE)
  testthat::local_mocked_bindings(
    pandoc_to = function(...) "latex",
    is_html_output = function(...) FALSE,
    .package = "knitr"
  )
  testthat::expect_false(saros:::is_html_output_or_officer())
})
