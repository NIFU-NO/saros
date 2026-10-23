# Regression guard for #621: n_range2() took its N from
# `.count_per_indep_group`, the size of a single bar group, so a figure with
# an independent variable was captioned with the range of the group sizes --
# `N = [149-151]` for 300 respondents -- rather than the size of the sample.
# `.count_per_dep` is the number of respondents behind each dependent variable,
# and is what the data-based n_range() has always reported.

testthat::test_that("n_range2 reports the sample, not the group sizes, given an indep", {
  with_indep <- makeme(data = saros::ex_survey, dep = b_1:b_3, indep = x1_sex)
  without <- makeme(data = saros::ex_survey, dep = b_1:b_3)

  testthat::expect_equal(n_range2(with_indep), "300")
  testthat::expect_equal(n_range2(with_indep), n_range2(without))
  testthat::expect_equal(
    n_range2(with_indep),
    n_range(saros::ex_survey, dep = b_1:b_3, indep = x1_sex)
  )
  # ... while the group sizes are indeed not 300
  testthat::expect_equal(
    sort(unique(with_indep$data$.count_per_indep_group)),
    c(149, 151)
  )
})

testthat::test_that("n_range2 ranges over the dependent variables", {
  data <- saros::ex_survey
  data$b_1[1:20] <- NA
  data$x1_sex[21:30] <- NA
  p <- makeme(data = data, dep = b_1:b_3, indep = x1_sex, showNA = "never")

  # b_1: 300 - 20 - 10; b_2 and b_3: 300 - 10
  testthat::expect_equal(n_range2(p), "[270-290]")
  testthat::expect_equal(
    n_range2(p),
    n_range(data, dep = b_1:b_3, indep = x1_sex)
  )
})

testthat::test_that("n_range2 reads the same N from an mschart", {
  m <- makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )
  testthat::expect_equal(n_range2(m), "300")
})

testthat::test_that("the figure caption carries the sample N", {
  p <- makeme(data = saros::ex_survey, dep = b_1:b_3, indep = x1_sex)
  testthat::expect_equal(
    as.character(get_fig_title_suffix_from_ggplot(p)),
    "N = 300"
  )
})
