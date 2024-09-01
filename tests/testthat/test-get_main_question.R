testthat::test_that("get_main_question returns empty string for empty input", {
  result <- saros:::get_main_question(character(0), label_separator = ": ")
  testthat::expect_equal(result, "")
})

testthat::test_that("get_main_question returns main question for single input", {
  result <- saros:::get_main_question("What is your age? : Age", label_separator = ": ")
  testthat::expect_equal(result, "What is your age? ")
})

testthat::test_that("get_main_question handles multiple questions and warns", {
  testthat::expect_warning(result <- saros:::get_main_question(
    c("What is your age? : Age", "What is your height? : Height"),
    label_separator = ": "
  ), regexp = "There are multiple main questions for these variables")
  testthat::expect_true(grepl("What is your age\\? \\nWhat is your height\\? ", result))
})

testthat::test_that("get_main_question throws error for non-character input", {
  testthat::expect_error(saros:::get_main_question(1:10, label_separator = ": "))
})

testthat::test_that("get_main_question returns unique main question", {
  result <- saros:::get_main_question(
    c("What is your age? : Age", "What is your age? : Years"),
    label_separator = ": "
  )
  testthat::expect_equal(result, "What is your age? ")
})

testthat::test_that("get_main_question handles NA values", {
  result <- saros:::get_main_question(
    c("What is your age? : Age", NA),
    label_separator = ": "
  )
  testthat::expect_equal(result, "What is your age? ")
})

testthat::test_that("get_main_question warns if no main question found", {
  testthat::expect_warning(result <- saros:::get_main_question(
    ": Age",
    label_separator = ": "
  ))
  testthat::expect_equal(result, "")
})

testthat::test_that("get_main_question handles factor input", {
  result <- saros:::get_main_question(
    factor(c("What is your age? : Age", "What is your age? : Years")),
    label_separator = ": "
  )
  testthat::expect_equal(result, "What is your age? ")
})

testthat::test_that("get_main_question handles ordered factor input", {
  result <- saros:::get_main_question(
    ordered(c("What is your age? : Age", "What is your age? : Years")),
    label_separator = ": "
  )
  testthat::expect_equal(result, "What is your age? ")
})

testthat::test_that("get_main_question can disable warning for multiple questions", {
  result <- saros:::get_main_question(
    c("What is your age? : Age", "What is your height? : Height"),
    label_separator = ": ",
    warn_multiple = FALSE
  )
  testthat::expect_true(grepl("What is your age\\? \\nWhat is your height\\? ", result))
})
