# Regression guard for #625: with `totals = TRUE`, crosstable_add_totals()
# copies every respondent into the total group, and `.count_per_dep` summed
# the counts of every group -- the copies included. 300 respondents became
# "(N = 600)" in the add_n_to_dep_label suffix and, since #621 made n_range2()
# read `.count_per_dep`, "N = 600" in the figure caption. With several
# independent variables the data are copied once per variable, so the count
# was multiplied accordingly.

tr <- list(by_total = "Everyone")

count_per_dep <- function(data, dep, indep, totals, showNA = "never") {
  out <- saros:::summarize_cat_cat_data(
    data,
    dep = dep,
    indep = indep,
    showNA = showNA,
    totals = totals,
    translations = tr
  )
  unique(out$.count_per_dep)
}

testthat::test_that("totals add a bar, not respondents", {
  d <- saros::ex_survey

  testthat::expect_equal(count_per_dep(d, "b_1", "x1_sex", totals = TRUE), 300)
  testthat::expect_equal(
    count_per_dep(d, "b_1", "x1_sex", totals = TRUE),
    count_per_dep(d, "b_1", "x1_sex", totals = FALSE)
  )
})

testthat::test_that("the per-group counts, the total group included, are unchanged", {
  out <- saros:::summarize_cat_cat_data(
    saros::ex_survey,
    dep = "b_1",
    indep = "x1_sex",
    showNA = "never",
    totals = TRUE,
    translations = tr
  )
  per_group <- unique(out[, c("x1_sex", ".count_per_indep_group")])
  per_group <- stats::setNames(
    per_group$.count_per_indep_group,
    as.character(per_group$x1_sex)
  )
  testthat::expect_equal(
    per_group[c("Males", "Females", "Everyone")],
    c(Males = 151, Females = 149, Everyone = 300)
  )
})

testthat::test_that("the dep label suffix and the caption carry the sample N", {
  p <- makeme(
    data = saros::ex_survey,
    dep = b_1,
    indep = x1_sex,
    showNA = "never",
    totals = TRUE,
    add_n_to_dep_label = TRUE,
    hide_axis_text_if_single_variable = FALSE
  )
  testthat::expect_equal(
    unique(as.character(p$data$.variable_label)),
    "Bejing (N = 300)"
  )
  testthat::expect_equal(as.character(n_range2(p)), "300")
  testthat::expect_equal(
    as.character(get_fig_title_suffix_from_ggplot(p)),
    "N = 300"
  )
})

testthat::test_that("missing indep values count as they do without totals", {
  d <- saros::ex_survey
  d$x1_sex[1:10] <- NA

  # showNA = "never" leaves the ten out of the groups; the N agrees with
  # n_range(), which counts complete cases of dep and indep.
  testthat::expect_equal(
    count_per_dep(d, "b_1", "x1_sex", totals = TRUE),
    count_per_dep(d, "b_1", "x1_sex", totals = FALSE)
  )
  testthat::expect_equal(
    as.character(count_per_dep(d, "b_1", "x1_sex", totals = TRUE)),
    as.character(n_range(d, dep = b_1, indep = x1_sex))
  )
  # With showNA = "ifany" they form a group of their own and are counted
  testthat::expect_equal(
    count_per_dep(d, "b_1", "x1_sex", totals = TRUE, showNA = "ifany"),
    300
  )
})

testthat::test_that("several independent variables are not counted once per variable", {
  d <- saros::ex_survey
  out <- saros:::crosstable(
    d,
    dep = "b_1",
    indep = c("x1_sex", "x2_human"),
    showNA = "never",
    totals = TRUE,
    translations = tr
  )
  testthat::expect_equal(
    unique(out$.count_per_dep),
    sum(stats::complete.cases(d[, c("b_1", "x1_sex", "x2_human")]))
  )
})

testthat::test_that("the marker column does not leak into the output", {
  out <- saros:::summarize_cat_cat_data(
    saros::ex_survey,
    dep = "b_1",
    indep = "x1_sex",
    totals = TRUE,
    translations = tr
  )
  testthat::expect_false(".saros_total_copy" %in% names(out))
  testthat::expect_equal(saros:::get_indep_col_names(out), "x1_sex")
})
