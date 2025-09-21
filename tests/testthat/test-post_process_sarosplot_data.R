testthat::test_that("post_process_makeme_data reverses factor levels for independent variable", {
  data <- data.frame(indep1 = factor(c("A", "B", "C")), .category = c(1, 2, 3))
  result <- saros:::post_process_makeme_data(data, indep = "indep1")
  testthat::expect_equal(
    levels(result$indep1),
    rev(levels(factor(c("A", "B", "C"))))
  )
})

testthat::test_that("post_process_makeme_data does not reverse factor levels if no independent variable", {
  data <- data.frame(.category = c(1, 2, 3))
  result <- saros:::post_process_makeme_data(data)
  testthat::expect_equal(result, data)
})

testthat::test_that("post_process_makeme_data does not reverse factor levels for NA categories if showNA is never", {
  data <- data.frame(indep1 = factor(c("A", "B", "C")), .category = c(1, 2, NA))
  result <- saros:::post_process_makeme_data(
    data,
    indep = "indep1",
    showNA = "never"
  )
  testthat::expect_equal(result$.category, c(1, 2, NA))
})

testthat::test_that("post_process_makeme_data reverses .category for binary categories with colour_2nd_binary_cat", {
  data <- data.frame(.category = factor(c("Yes", "No")))
  result <- saros:::post_process_makeme_data(
    data,
    colour_2nd_binary_cat = "#FFFFFF"
  )
  testthat::expect_equal(
    levels(result$.category),
    rev(levels(factor(c("Yes", "No"))))
  )
})

testthat::test_that("post_process_makeme_data does not reverse .category for non-binary categories", {
  data <- data.frame(.category = factor(c("Yes", "No", "Maybe")))
  result <- saros:::post_process_makeme_data(
    data,
    colour_2nd_binary_cat = "#FFFFFF"
  )
  testthat::expect_equal(
    levels(result$.category),
    levels(factor(c("Yes", "No", "Maybe")))
  )
})

testthat::test_that("post_process_makeme_data does not reverse .category if colour_2nd_binary_cat is NULL", {
  data <- data.frame(.category = factor(c("Yes", "No")))
  result <- saros:::post_process_makeme_data(data)
  testthat::expect_equal(
    levels(result$.category),
    levels(factor(c("Yes", "No")))
  )
})

testthat::test_that("post_process_makeme_data handles empty data frame", {
  data <- data.frame(indep1 = factor(), .category = factor())
  result <- saros:::post_process_makeme_data(data, indep = "indep1")
  testthat::expect_equal(result, data)
})

# testthat::test_that("post_process_makeme_data handles missing .category", {
#   data <- data.frame(indep1 = factor(c("A", "B", "C")))
#   result <- saros:::post_process_makeme_data(data, indep = "indep1")
#   testthat::expect_equal(result, data)
# })

testthat::test_that("post_process_makeme_data handles NA in .category with showNA as always", {
  data <- data.frame(.category = factor(c("Yes", "No", NA)))
  result <- saros:::post_process_makeme_data(data, showNA = "always")
  testthat::expect_equal(result$.category, factor(c("Yes", "No", NA)))
})

testthat::test_that("post_process_makeme_data handles showNA as ifany", {
  data <- data.frame(.category = factor(c("Yes", "No", NA)))
  result <- saros:::post_process_makeme_data(data, showNA = "ifany")
  testthat::expect_equal(result$.category, factor(c("Yes", "No", NA)))
})
