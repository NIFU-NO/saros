test_that("tabular_write creates files in different formats", {
  # Create test data
  test_data <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie"),
    value = c(10.5, 20.3, 30.1),
    stringsAsFactors = FALSE
  )

  # Test CSV writing
  csv_file <- tempfile(fileext = ".csv")
  expect_no_error(tabular_write(test_data, csv_file, "csv"))
  expect_true(file.exists(csv_file))

  # Test TSV writing
  tsv_file <- tempfile(fileext = ".tsv")
  expect_no_error(tabular_write(test_data, tsv_file, "tsv"))
  expect_true(file.exists(tsv_file))

  # Test delim writing
  delim_file <- tempfile(fileext = ".txt")
  expect_no_error(tabular_write(test_data, delim_file, "delim"))
  expect_true(file.exists(delim_file))

  # Clean up
  unlink(c(csv_file, tsv_file, delim_file))
})

test_that("tabular_write handles Excel format when writexl is available", {
  skip_if_not_installed("writexl")

  test_data <- data.frame(x = 1:2, y = c("a", "b"))
  xlsx_file <- tempfile(fileext = ".xlsx")

  expect_no_error(tabular_write(test_data, xlsx_file, "xlsx"))
  expect_true(file.exists(xlsx_file))

  unlink(xlsx_file)
})

test_that("tabular_write handles SPSS format when haven is available", {
  skip_if_not_installed("haven")

  test_data <- data.frame(x = 1:2, y = c("a", "b"))
  sav_file <- tempfile(fileext = ".sav")

  expect_no_error(tabular_write(test_data, sav_file, "sav"))
  expect_true(file.exists(sav_file))

  unlink(sav_file)
})

test_that("tabular_write handles Stata format when haven is available", {
  skip_if_not_installed("haven")

  test_data <- data.frame(x = 1:2, y = c("a", "b"))
  dta_file <- tempfile(fileext = ".dta")

  expect_no_error(tabular_write(test_data, dta_file, "dta"))
  expect_true(file.exists(dta_file))

  unlink(dta_file)
})
