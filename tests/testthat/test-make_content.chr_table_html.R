# Test make_content.chr_table_html via saros::makeme()

test_that("makeme with chr_table_html works with character responses only", {
  # Create test data with character responses
  test_data <- data.frame(
    q1 = c("Response A", "Response B", "Response C"),
    respondent_id = 1:3,
    stringsAsFactors = FALSE
  )

  result <- saros::makeme(
    data = test_data,
    dep = "q1",
    type = "chr_table_html",
    label_separator = NULL
  )

  expect_true(is.data.frame(result))
  expect_equal(result$q1, c("Response A", "Response B", "Response C"))
})

test_that("makeme with chr_table_html works with single indep variable", {
  # Create test data with character responses and background info
  test_data <- data.frame(
    q1 = c("Great service", "Could be better", "Excellent!"),
    gender = c("Male", "Female", "Male"),
    age_group = c("18-25", "26-35", "18-25"),
    stringsAsFactors = FALSE
  )

  result <- saros::makeme(
    data = test_data,
    dep = q1,
    indep = gender,
    type = "chr_table_html",
    label_separator = NULL
  )

  expect_true(is.data.frame(result))
  expect_equal(
    result$q1,
    c("Great service", "Could be better", "Excellent!")
  )
  expect_equal(result$gender, c("Male", "Female", "Male"))
})

test_that("makeme with chr_table_html works with multiple indep variables", {
  # Create test data with multiple background variables
  test_data <- data.frame(
    feedback = c("Love it!", "Not bad", "Could improve", "Amazing!"),
    gender = c("Male", "Female", "Female", "Male"),
    age_group = c("18-25", "26-35", "36-45", "18-25"),
    region = c("North", "South", "North", "East"),
    stringsAsFactors = FALSE
  )

  result <- saros::makeme(
    data = test_data,
    dep = feedback,
    indep = c(gender, age_group, region),
    type = "chr_table_html",
    label_separator = NULL
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_true("feedback" %in% colnames(result))
  expect_true("gender" %in% colnames(result))
  expect_true("age_group" %in% colnames(result))
  expect_true("region" %in% colnames(result))
  expect_equal(
    result$feedback,
    c("Love it!", "Not bad", "Could improve", "Amazing!")
  )
})

test_that("makeme with chr_table_html filters out NA and empty responses", {
  # Create test data with NA and empty responses
  test_data <- data.frame(
    comments = c("Good", "", NA, "Bad", "   "),
    user_type = c("New", "Regular", "New", "Premium", "Regular"),
    stringsAsFactors = FALSE
  )

  result <- saros::makeme(
    data = test_data,
    dep = comments,
    indep = user_type,
    type = "chr_table_html",
    label_separator = NULL
  )

  expect_true(is.data.frame(result))
  expect_equal(result$comments, c("Good", "Bad", "   "))
  expect_equal(result$user_type, c("New", "Premium", "Regular"))
})

test_that("makeme with chr_table_html handles empty data", {
  # Test with empty data
  empty_data <- data.frame(
    q1 = character(0),
    stringsAsFactors = FALSE
  )

  result <- saros::makeme(
    data = empty_data,
    dep = q1,
    type = "chr_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("makeme with chr_table_html handles all NA responses", {
  # Create test data where all responses are NA
  test_data <- data.frame(
    comments = c(NA, NA, NA),
    gender = c("Male", "Female", "Male"),
    stringsAsFactors = FALSE
  )

  result <- saros::makeme(
    data = test_data,
    dep = comments,
    indep = gender,
    type = "chr_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0) # All responses filtered out
})

test_that("makeme with chr_table_html handles factor responses", {
  # Create test data with factor responses (should convert to character)
  test_data <- data.frame(
    rating_text = factor(c("Excellent", "Good", "Poor")),
    department = c("Sales", "Support", "Marketing"),
    stringsAsFactors = FALSE
  )

  result <- saros::makeme(
    data = test_data,
    dep = rating_text,
    indep = department,
    type = "chr_table_html",
    label_separator = NULL
  )

  expect_true(is.data.frame(result))
  expect_equal(as.character(result$rating_text), c("Excellent", "Good", "Poor"))
  expect_equal(
    as.character(result$department),
    c("Sales", "Support", "Marketing")
  )
})

test_that("makeme with chr_table_html errors with multiple dep variables", {
  # Test error when multiple dep variables provided
  test_data <- data.frame(
    q1 = c("Response 1", "Response 2"),
    q2 = c("Other response", "Another"),
    gender = c("Male", "Female"),
    stringsAsFactors = FALSE
  )

  expect_error(
    saros::makeme(
      data = test_data,
      dep = c(q1, q2), # Multiple dep variables should error
      indep = gender,
      type = "chr_table_html"
    ),
    "`type = 'chr_table_html'` only supports a single dependent variable"
  )
})

test_that("makeme with chr_table_html handles missing indep variables gracefully", {
  # Test when indep variable doesn't exist in data
  test_data <- data.frame(
    feedback = c("Great", "Okay", "Poor"),
    existing_var = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  expect_error(
    saros::makeme(
      data = test_data,
      dep = feedback,
      indep = c(existing_var, nonexistent_var),
      type = "chr_table_html"
    ),
    "Can't select columns that don't exist"
  )
})

test_that("makeme with chr_table_html uses variable labels when available", {
  # Create test data with labeled variables
  test_data <- data.frame(
    feedback = c("Good service", "Bad experience"),
    dept = c("Sales", "Support"),
    stringsAsFactors = FALSE
  )
  attr(test_data$dept, "label") <- "Department"
  attr(test_data$feedback, "label") <- "Feedback from customers"

  result <- saros::makeme(
    data = test_data,
    dep = feedback,
    indep = dept,
    type = "chr_table_html",
    table_main_question_as_header = TRUE
  )

  expect_true(is.data.frame(result))
  expect_true("Department" %in% colnames(result))
  expect_true("Feedback from customers" %in% colnames(result))
})

# test_that("makeme with chr_table_html works with survey data", {
#   # Test with survey data structure
#   library(survey)

#   test_data <- data.frame(
#     open_feedback = c(
#       "Great product",
#       "Needs improvement",
#       "Love it",
#       "Could be better"
#     ),
#     age_group = c("25-34", "35-44", "25-34", "45-54"),
#     gender = c("Male", "Female", "Female", "Male"),
#     weights = c(1.2, 0.8, 1.0, 1.1),
#     stringsAsFactors = FALSE
#   )

#   survey_data <- svydesign(ids = ~1, weights = ~weights, data = test_data)

#   result <- saros::makeme(
#     data = survey_data,
#     dep = open_feedback,
#     indep = c(age_group, gender),
#     type = "chr_table_html"
#   )

#   expect_true(is.data.frame(result))
#   expect_equal(nrow(result), 4)
#   expect_true("open_feedback" %in% colnames(result))
#   expect_true("age_group" %in% colnames(result))
#   expect_true("gender" %in% colnames(result))
# })
