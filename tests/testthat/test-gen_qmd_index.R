
################################################################################
testthat::test_that("gen_qmd_index works as expected", {
  # Test 1: Basic test with title, authors, and chapter files
  index_filepath <- tempfile(fileext = ".qmd")
  chapter_filepaths <- c(tempfile(fileext = ".qmd"), tempfile(fileext = ".qmd"))
  result <- saros:::gen_qmd_index(
    authors = c("Author 1", "Author 2"),
    index_filepath = index_filepath,
    chapter_filepaths = chapter_filepaths
  )
  testthat::expect_equal(result, index_filepath)
  testthat::expect_true(file.exists(index_filepath))


  # Test 2: Test with only title and chapter files
  index_filepath <- tempfile(fileext = ".qmd")
  result <- saros:::gen_qmd_index(
    index_filepath = index_filepath,
    chapter_filepaths = chapter_filepaths
  )
  testthat::expect_equal(result, index_filepath)
  testthat::expect_true(file.exists(index_filepath))

  # Test 3: Test with only authors and chapter files
  index_filepath <- tempfile(fileext = ".qmd")
  result <- saros:::gen_qmd_index(
    authors = c("Author 1", "Author 2"),
    index_filepath = index_filepath,
    chapter_filepaths = chapter_filepaths
  )
  testthat::expect_equal(result, index_filepath)
  testthat::expect_true(file.exists(index_filepath))

  # Test 4: Test with only chapter files
  index_filepath <- tempfile(fileext = ".qmd")
  result <- saros:::gen_qmd_index(
    index_filepath = index_filepath,
    chapter_filepaths = chapter_filepaths
  )
  testthat::expect_equal(result, index_filepath)
  testthat::expect_true(file.exists(index_filepath))


  # Test 5: Test with empty chapter filepaths
  index_filepath <- tempfile(fileext = ".qmd")
  result <- saros:::gen_qmd_index(
    authors = c("Author 1", "Author 2"),
    index_filepath = index_filepath,
    chapter_filepaths = NULL
  )
  testthat::expect_equal(result, index_filepath)
  testthat::expect_true(file.exists(index_filepath))

})
