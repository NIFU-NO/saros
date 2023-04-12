# Test 1: Test that the function correctly wraps a simple R code string
testthat::test_that("rcode_to_quarto wraps a simple R code string", {
  input_code <- "print('Hello, world!')"
  expected_output <- paste0("```{r}\nprint('Hello, world!')\n``` \n")
  result <- saros:::rcode_to_quarto(input_code)
  testthat::expect_equal(result, expected_output)
})
testthat::test_that("rcode_to_quarto wraps a simple R code string", {
  input_code <- NULL
  testthat::expect_error(
    saros:::rcode_to_quarto(code = input_code),
    regexp = "`code` must be a character vector of length 1, not NULL")
})
