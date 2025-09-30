test_that("get_data_label_opts returns character vector", {
  result <- get_data_label_opts()
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("get_data_label_opts contains expected values", {
  result <- get_data_label_opts()

  # Should contain common data label options (based on actual function output)
  expect_true("count" %in% result)
  expect_true("proportion" %in% result)
  expect_true("percentage" %in% result)
  expect_true("mean" %in% result)
  expect_true("median" %in% result)
})

test_that("get_data_label_opts is consistent", {
  # Should return the same result on multiple calls
  result1 <- get_data_label_opts()
  result2 <- get_data_label_opts()

  expect_equal(result1, result2)
})

test_that("get_data_label_opts returns no duplicates", {
  result <- get_data_label_opts()
  expect_equal(length(result), length(unique(result)))
})

test_that("saros.env environment exists and contains data_label_opts", {
  # Check that the internal environment is properly set up
  expect_true(
    exists(".saros.env", envir = globalenv()) ||
      exists(".saros.env", envir = getNamespace("saros"))
  )

  # The function should not error
  expect_no_error(get_data_label_opts())
})
