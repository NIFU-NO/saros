testthat::test_that("sort_indep_by = '.count_per_indep_group' orders by indep totals", {
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "cat_table_html",
    sort_indep_by = ".count_per_indep_group",
    descend_indep = TRUE
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_gt(nrow(result), 0)
})

testthat::test_that("sort_indep_by = '.count_per_indep_group' with descend_indep = FALSE", {
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "cat_table_html",
    sort_indep_by = ".count_per_indep_group",
    descend_indep = FALSE
  )
  testthat::expect_true(is.data.frame(result))
  testthat::expect_gt(nrow(result), 0)
})
