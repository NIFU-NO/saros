test_that("tabular_read loads data from different formats", {
  # Create test data
  test_data <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie"),
    value = c(10.5, 20.3, 30.1),
    stringsAsFactors = FALSE
  )

  # Test CSV round-trip
  csv_file <- tempfile(fileext = ".csv")
  tabular_write(test_data, csv_file, "csv")
  loaded_csv <- tabular_read(csv_file, "csv")
  expect_equal(nrow(loaded_csv), 3)
  expect_equal(ncol(loaded_csv), 3)
  expect_contains(names(loaded_csv), c("id", "name", "value"))

  # Test TSV round-trip
  tsv_file <- tempfile(fileext = ".tsv")
  tabular_write(test_data, tsv_file, "tsv")
  loaded_tsv <- tabular_read(tsv_file, "tsv")
  expect_equal(nrow(loaded_tsv), 3)
  expect_equal(ncol(loaded_tsv), 3)

  # Test delim round-trip
  delim_file <- tempfile(fileext = ".txt")
  tabular_write(test_data, delim_file, "delim")
  loaded_delim <- tabular_read(delim_file, "delim")
  expect_equal(nrow(loaded_delim), 3)
  expect_equal(ncol(loaded_delim), 3)

  # Clean up
  unlink(c(csv_file, tsv_file, delim_file))
})

test_that("tabular_read handles Excel format when readxl is available", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  test_data <- data.frame(x = 1:3, y = letters[1:3])
  xlsx_file <- tempfile(fileext = ".xlsx")

  tabular_write(test_data, xlsx_file, "xlsx")
  loaded_data <- tabular_read(xlsx_file, "xlsx")

  expect_equal(nrow(loaded_data), 3)
  expect_equal(ncol(loaded_data), 2)
  expect_contains(names(loaded_data), c("x", "y"))

  unlink(xlsx_file)
})

test_that("tabular_read handles SPSS format when haven is available", {
  skip_if_not_installed("haven")

  test_data <- data.frame(x = 1:3, y = letters[1:3])
  sav_file <- tempfile(fileext = ".sav")

  tabular_write(test_data, sav_file, "sav")
  loaded_data <- tabular_read(sav_file, "sav")

  expect_equal(nrow(loaded_data), 3)
  expect_equal(ncol(loaded_data), 2)

  unlink(sav_file)
})

test_that("tabular_read handles Stata format when haven is available", {
  skip_if_not_installed("haven")

  test_data <- data.frame(x = 1:3, y = letters[1:3])
  dta_file <- tempfile(fileext = ".dta")

  tabular_write(test_data, dta_file, "dta")
  loaded_data <- tabular_read(dta_file, "dta")

  expect_equal(nrow(loaded_data), 3)
  expect_equal(ncol(loaded_data), 2)

  unlink(dta_file)
})

test_that("tabular_read throws error for non-existent files", {
  non_existent_file <- "this_file_does_not_exist.csv"
  expect_error(
    tabular_read(non_existent_file, "csv"),
    "File does not exist:"
  )
})

test_that("tabular_read throws error for unsupported formats", {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".txt")
  writeLines("test", temp_file)

  expect_error(
    tabular_read(temp_file, "unsupported_format"),
    "Unsupported format: unsupported_format"
  )

  unlink(temp_file)
})

test_that("tabular functions handle empty data frames", {
  empty_data <- data.frame()
  temp_file <- tempfile(fileext = ".csv")

  # Should handle empty data frame writing
  expect_no_error(tabular_write(empty_data, temp_file, "csv"))
  expect_true(file.exists(temp_file))

  # Reading empty file should work
  loaded_empty <- tabular_read(temp_file, "csv")
  expect_equal(nrow(loaded_empty), 0)

  unlink(temp_file)
})

test_that("tabular functions handle data with special characters", {
  special_data <- data.frame(
    text = c("Hello, World!", "Café münü", "データ"),
    numbers = c(1.5, 2.7, 3.9),
    stringsAsFactors = FALSE
  )

  csv_file <- tempfile(fileext = ".csv")
  tabular_write(special_data, csv_file, "csv")
  loaded_data <- tabular_read(csv_file, "csv")

  expect_equal(nrow(loaded_data), 3)
  expect_equal(ncol(loaded_data), 2)
  # Note: exact character matching depends on encoding, so we check structure
  expect_contains(names(loaded_data), c("text", "numbers"))

  unlink(csv_file)
})

test_that("tabular_read passes additional arguments correctly", {
  # Create test data with custom column names
  test_data <- data.frame(
    col1 = 1:3,
    col2 = c("a", "b", "c")
  )

  csv_file <- tempfile(fileext = ".csv")
  tabular_write(test_data, csv_file, "csv")

  # Test with additional arguments for read_csv
  loaded_data <- tabular_read(csv_file, "csv", col_types = "ic")
  expect_equal(nrow(loaded_data), 3)
  expect_equal(ncol(loaded_data), 2)

  unlink(csv_file)
})
