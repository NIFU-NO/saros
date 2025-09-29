testthat::test_that("keep_subitem extracts subitems with separator", {
  fct <- factor(c("Main: Sub1", "Main: Sub2", "Main: Sub3"))
  result <- saros:::keep_subitem(fct, label_separator = ": ")
  
  testthat::expect_equal(levels(result), c("Sub1", "Sub2", "Sub3"))
  testthat::expect_equal(as.character(result), c("Sub1", "Sub2", "Sub3"))
})

testthat::test_that("keep_subitem preserves factor without separator", {
  fct <- factor(c("Item1", "Item2", "Item3"))
  result <- saros:::keep_subitem(fct, label_separator = NULL)
  
  testthat::expect_equal(levels(result), c("Item1", "Item2", "Item3"))
  testthat::expect_equal(as.character(result), c("Item1", "Item2", "Item3"))
})

testthat::test_that("keep_subitem handles ordered factor", {
  fct <- factor(c("Q1: Low", "Q1: Medium", "Q1: High"), 
                levels = c("Q1: Low", "Q1: Medium", "Q1: High"), 
                ordered = TRUE)
  result <- saros:::keep_subitem(fct, label_separator = ": ", ordered = TRUE)
  
  testthat::expect_true(is.ordered(result))
  testthat::expect_equal(levels(result), c("Low", "Medium", "High"))
  testthat::expect_equal(as.character(result), c("Low", "Medium", "High"))
})

testthat::test_that("keep_subitem creates unordered factor when requested", {
  fct <- factor(c("Q1: A", "Q1: B", "Q1: C"), ordered = TRUE)
  result <- saros:::keep_subitem(fct, label_separator = ": ", ordered = FALSE)
  
  testthat::expect_false(is.ordered(result))
  testthat::expect_equal(levels(result), c("A", "B", "C"))
  testthat::expect_equal(as.character(result), c("A", "B", "C"))
})

testthat::test_that("keep_subitem handles character input", {
  chr <- c("Question: Answer1", "Question: Answer2")
  result <- saros:::keep_subitem(chr, label_separator = ": ")
  
  testthat::expect_s3_class(result, "factor")
  testthat::expect_equal(levels(result), c("Answer1", "Answer2"))
  testthat::expect_equal(as.character(result), c("Answer1", "Answer2"))
})

testthat::test_that("keep_subitem handles complex separator", {
  fct <- factor(c("Main :: Sub1", "Main :: Sub2"))
  result <- saros:::keep_subitem(fct, label_separator = " :: ")
  
  testthat::expect_equal(levels(result), c("Sub1", "Sub2"))
  testthat::expect_equal(as.character(result), c("Sub1", "Sub2"))
})

testthat::test_that("keep_subitem handles missing values", {
  fct <- factor(c("Q: A", NA, "Q: B"))
  result <- saros:::keep_subitem(fct, label_separator = ": ")
  
  testthat::expect_equal(length(result), 3)
  testthat::expect_true(is.na(result[2]))
  testthat::expect_equal(as.character(result[c(1, 3)]), c("A", "B"))
})

testthat::test_that("keep_subitem handles empty separator match", {
  fct <- factor(c("NoSeparator1", "NoSeparator2"))
  result <- saros:::keep_subitem(fct, label_separator = ": ")
  
  # Should return original values when no separator found
  testthat::expect_equal(levels(result), c("NoSeparator1", "NoSeparator2"))
  testthat::expect_equal(as.character(result), c("NoSeparator1", "NoSeparator2"))
})

testthat::test_that("keep_subitem handles duplicate values", {
  fct <- factor(c("Q: A", "Q: B", "Q: A"))
  result <- saros:::keep_subitem(fct, label_separator = ": ")
  
  testthat::expect_length(result, 3)
  testthat::expect_equal(as.character(result), c("A", "B", "A"))
})

testthat::test_that("keep_subitem preserves unique levels correctly", {
  fct <- factor(c("Main: Sub1", "Main: Sub1", "Main: Sub2"))
  result <- saros:::keep_subitem(fct, label_separator = ": ")
  
  # Should have unique levels but preserve all values
  testthat::expect_equal(length(levels(result)), 2)
  testthat::expect_equal(length(result), 3)
})