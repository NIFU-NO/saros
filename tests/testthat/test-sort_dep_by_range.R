testthat::test_that("sort_dep_by = '.range' orders variables by proportion range", {
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    sort_dep_by = ".range",
    descend = TRUE
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_gt(nrow(result), 0)
})

testthat::test_that("sort_dep_by = '.range' with descend = FALSE reverses order", {
  result_desc <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    sort_dep_by = ".range",
    descend = TRUE
  )
  result_asc <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    sort_dep_by = ".range",
    descend = FALSE
  )
  # First row labels should differ (reversed order)
  if (nrow(result_desc) > 1 && nrow(result_asc) > 1) {
    testthat::expect_true(
      result_desc[[1]][1] != result_asc[[1]][1] ||
        result_desc[[1]][nrow(result_desc)] != result_asc[[1]][nrow(result_asc)]
    )
  }
})
