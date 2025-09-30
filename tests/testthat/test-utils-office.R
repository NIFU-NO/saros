test_that("use_docx creates officer document from template path", {
  skip_if_not_installed("officer")

  # Create a temporary docx file for testing
  temp_docx <- tempfile(fileext = ".docx")
  doc <- officer::read_docx()
  print(doc, target = temp_docx)

  # Test with valid template path
  result <- use_docx(temp_docx)
  expect_s3_class(result, "rdocx")

  # Clean up
  unlink(temp_docx)
})

test_that("use_docx handles existing officer object", {
  skip_if_not_installed("officer")

  # Create an officer document
  doc <- officer::read_docx()

  # Pass existing object
  result <- use_docx(doc)
  expect_s3_class(result, "rdocx")
  expect_identical(result, doc)
})

test_that("use_docx creates default document when NULL", {
  skip_if_not_installed("officer")

  result <- use_docx(NULL)
  expect_s3_class(result, "rdocx")
})

test_that("use_docx handles invalid template path", {
  skip_if_not_installed("officer")

  # Non-existent file should return the input
  invalid_path <- "non_existent_file.docx"
  result <- use_docx(invalid_path)
  expect_equal(result, invalid_path)
})

test_that("use_docx handles invalid input types", {
  skip_if_not_installed("officer")

  # Non-character, non-single length should return input
  result <- use_docx(123)
  expect_equal(result, 123)

  result2 <- use_docx(c("file1.docx", "file2.docx"))
  expect_equal(result2, c("file1.docx", "file2.docx"))
})

test_that("get_docx_dims calculates document dimensions", {
  skip_if_not_installed("officer")

  doc <- officer::read_docx()
  dims <- get_docx_dims(doc)

  expect_type(dims, "double")
  expect_named(dims, c("w", "h"))
  expect_true(dims["w"] > 0)
  expect_true(dims["h"] > 0)
})

test_that("get_docx_dims accounts for margins", {
  skip_if_not_installed("officer")

  doc <- officer::read_docx()
  dims <- get_docx_dims(doc)

  # Get raw dimensions
  raw_dims <- officer::docx_dim(doc)

  # Check that margins are subtracted
  expect_true(dims["w"] < raw_dims$page[["width"]])
  expect_true(dims["h"] < raw_dims$page[["height"]])
})

test_that("get_block_caption basic functionality", {
  skip_if_not_installed("officer")

  # Create test data
  data <- data.frame(
    q1_some_suffix = factor(c("A", "B", "C"))
  )
  attr(data$q1_some_suffix, "label") <- "Main Question_Some Suffix"

  doc <- officer::read_docx()

  # Test with label separator - function should run without error
  expect_no_error(
    result <- get_block_caption(
      data = data,
      cols_pos = "q1_some_suffix",
      docx_file = doc,
      label_separator = "_",
      caption_style = "Normal", # Use existing style
      caption_autonum = NULL
    )
  )
})

test_that("get_block_caption handles no label separator", {
  skip_if_not_installed("officer")

  data <- data.frame(
    q1 = factor(c("A", "B", "C"))
  )
  attr(data$q1, "label") <- "Question 1"

  doc <- officer::read_docx()

  # Test without label separator should return NULL
  result <- get_block_caption(
    data = data,
    cols_pos = "q1",
    docx_file = doc,
    label_separator = NULL
  )

  expect_null(result)
})

test_that("get_block_caption uses default caption style", {
  skip_if_not_installed("officer")

  data <- data.frame(
    q1_suffix = factor(c("A", "B", "C"))
  )
  attr(data$q1_suffix, "label") <- "Main Question_Suffix"

  doc <- officer::read_docx()

  # Test with NULL caption_style (should default to "Normal")
  expect_no_error(
    result <- get_block_caption(
      data = data,
      cols_pos = "q1_suffix",
      docx_file = doc,
      label_separator = "_",
      caption_style = NULL,
      caption_autonum = NULL
    )
  )
})
