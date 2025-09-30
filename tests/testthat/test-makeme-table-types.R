test_that("makeme cat_table_html basic functionality", {
  data("ex_survey", package = "saros")

  # Basic cat_table_html test with exact expected values for a_1
  result <- makeme(
    data = ex_survey,
    dep = a_1,
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 1)
  expect_equal(result$`No (%)`, "55")
  expect_equal(result$`Yes (%)`, "36")
  expect_equal(result$`NA (%)`, "8")
  expect_equal(result$`Total (N)`, 300)
})

test_that("makeme cat_table_html with count data_label", {
  data("ex_survey", package = "saros")

  # Test count option - should give actual counts instead of percentages
  result <- makeme(
    data = ex_survey,
    dep = a_1,
    type = "cat_table_html",
    data_label = "count"
  )

  expect_true(is.data.frame(result))
  expect_equal(result$`Total (N)`, 300)
  # With count, column names are just the category names without (count) suffix
  expect_equal(result$No, "166")
  expect_equal(result$Yes, "109")
  expect_equal(result$`NA`, "25")
})

test_that("makeme cat_table_html with independent variable", {
  data("ex_survey", package = "saros")

  # Test with independent variable - should show breakdown by gender
  result <- makeme(
    data = ex_survey,
    dep = a_1,
    indep = x1_sex,
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5) # Gender + No (%) + Yes (%) + NA (%) + Total (N)
  expect_equal(nrow(result), 2) # Males and Females
  expect_equal(as.character(result$Gender), c("Males", "Females"))
  expect_equal(result$`Total (N)`, c(151, 149))
  expect_equal(result$`No (%)`[1], "56") # Males
  expect_equal(result$`No (%)`[2], "54") # Females
})

test_that("makeme cat_table_html with multiple variables", {
  data("ex_survey", package = "saros")

  # Test with multiple dependent variables
  result <- makeme(
    data = ex_survey,
    dep = c(a_1, a_2),
    type = "cat_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5) # .variable_label + No (%) + Yes (%) + NA (%) + Total (N)
  expect_equal(nrow(result), 2) # Two variables: a_1 and a_2
  expect_true(all(result$`Total (N)` == 300))

  # Check that a_1 values are present (second row due to ordering)
  expect_equal(result$`No (%)`[2], "55") # a_1
  expect_equal(result$`Yes (%)`[2], "36") # a_1
  expect_equal(result$`NA (%)`[2], "8") # a_1
})

test_that("makeme int_table_html basic functionality", {
  data("ex_survey", package = "saros")

  # Test with c_1 (Company A experience) - exact expected values
  result <- makeme(
    data = ex_survey,
    dep = c_1,
    type = "int_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 11)
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$.variable_label), "Company A")
  expect_equal(result$N, 300)
  expect_equal(result$N_valid, 300)
  expect_equal(result$N_missing, 0)
  expect_equal(result$Mean, 21, tolerance = 0.1)
  expect_equal(result$SD, 5, tolerance = 0.1)
  expect_equal(result$Median, 21, tolerance = 0.1)
  expect_equal(result$Min, 8, tolerance = 0.1)
  expect_equal(result$Max, 32, tolerance = 0.1)
})

test_that("makeme int_table_html with independent variable", {
  data("ex_survey", package = "saros")

  # Test with independent variable - should show stats by gender
  result <- makeme(
    data = ex_survey,
    dep = c_1,
    indep = x1_sex,
    type = "int_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2) # Males and Females
  expect_equal(ncol(result), 12) # Additional Gender column
  expect_true("Gender" %in% colnames(result))
  expect_equal(as.character(result$Gender), c("Males", "Females"))
  # Should have valid sample sizes for both groups
  expect_true(all(result$N > 140))
  expect_true(all(result$N < 160))
})

test_that("makeme int_table_html with multiple variables", {
  data("ex_survey", package = "saros")

  # Test with multiple numeric variables
  result <- makeme(
    data = ex_survey,
    dep = c(c_1, c_2),
    type = "int_table_html"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2) # Two variables: c_1 and c_2
  expect_equal(ncol(result), 11) # Same column structure
  expect_true(all(result$N == 300))
  expect_true(all(result$N_valid == 300))
  expect_true(all(result$N_missing == 0))
})

test_that("makeme handles error conditions", {
  data("ex_survey", package = "saros")

  # Test with invalid type
  expect_error(
    makeme(
      data = ex_survey,
      dep = a_1,
      type = "invalid_type"
    ),
    "Invalid make_content-type"
  )
})
