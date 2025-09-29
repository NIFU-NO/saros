testthat::test_that("string_wrap wraps text at specified width", {
  text <- "This is a long string that needs to be wrapped"
  result <- saros:::string_wrap(text, width = 20)
  
  testthat::expect_type(result, "character")
  testthat::expect_true(grepl("\\n", result))
  
  # Check that lines don't exceed the width (approximately)
  lines <- strsplit(result, "\\n")[[1]]
  testthat::expect_true(all(nchar(lines) <= 25))  # Allow some flexibility
})

testthat::test_that("string_wrap handles short text", {
  text <- "Short text"
  result <- saros:::string_wrap(text, width = 50)
  
  testthat::expect_equal(result, text)
  testthat::expect_false(grepl("\\n", result))
})

testthat::test_that("string_wrap handles empty string", {
  text <- ""
  result <- saros:::string_wrap(text, width = 10)
  
  testthat::expect_equal(result, "")
})

testthat::test_that("string_wrap handles single word longer than width", {
  text <- "supercalifragilisticexpialidocious"
  result <- saros:::string_wrap(text, width = 10)
  
  testthat::expect_type(result, "character")
  # stringi will handle long words by breaking them or keeping them
  testthat::expect_true(nchar(result) > 0)
})

testthat::test_that("string_wrap handles multiple strings", {
  text <- c("First string to wrap", "Second string to wrap")
  result <- saros:::string_wrap(text, width = 10)
  
  testthat::expect_length(result, 2)
  testthat::expect_true(all(grepl("\\n", result)))
})

testthat::test_that("string_wrap handles width of 1", {
  text <- "abc def"
  result <- saros:::string_wrap(text, width = 1)
  
  testthat::expect_type(result, "character")
  # Should have line breaks between words when width is very small
  testthat::expect_true(grepl("\\n", result))
})

testthat::test_that("string_wrap handles NA values", {
  text <- c("Normal text", NA, "More text")
  result <- saros:::string_wrap(text, width = 10)
  
  testthat::expect_length(result, 3)
  # stringi converts NA to "NA" string, so we expect "NA" not actual NA
  testthat::expect_equal(result[2], "NA")
})

testthat::test_that("string_wrap preserves character encoding", {
  text <- "Café münü naïve résumé"
  result <- saros:::string_wrap(text, width = 10)
  
  testthat::expect_type(result, "character")
  # Should contain the special characters
  testthat::expect_true(grepl("é", result))
  testthat::expect_true(grepl("ü", result))
})

testthat::test_that("string_wrap handles newlines in input", {
  text <- "Line 1\\nLine 2\\nLine 3"
  result <- saros:::string_wrap(text, width = 50)
  
  testthat::expect_type(result, "character")
  # Should preserve or handle existing newlines appropriately
  testthat::expect_true(nchar(result) > 0)
})

testthat::test_that("string_wrap handles very large width", {
  text <- "This is a normal sentence."
  result <- saros:::string_wrap(text, width = 1000)
  
  testthat::expect_equal(result, text)
  testthat::expect_false(grepl("\\n", result))
})