# Regression guard for #613: `summarize_int_cat_data()` passed the whole `indep`
# vector to `bind_rows(.id = )`, which takes a single column name, and the list
# was unnamed so even a valid `.id` would only have recorded "1", "2", ...
#
# The branch is reached by sigtest_table_html with a *numeric* dependent
# variable and several independent variables. The same call with a categorical
# dep already worked -- sigtest_table_html and chr_table_html are deliberately
# exempt from the single-indep rule (.saros.env$types_skip_multiple_indep_validation)
# and the ?makeme example uses one -- so this was a parity gap rather than a
# package-wide limitation. cat_* and int_* reject several indeps by design and
# are unaffected.

testthat::test_that("a numeric dep with several indeps produces one row per pair", {
  data("ex_survey", package = "saros")

  numeric_dep <- suppressWarnings(makeme(
    data = ex_survey,
    dep = c_1,
    indep = c(x1_sex, x2_human),
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never"
  ))
  categorical_dep <- suppressWarnings(makeme(
    data = ex_survey,
    dep = b_1,
    indep = c(x1_sex, x2_human),
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never"
  ))

  # The structure must match what a categorical dep has always produced.
  testthat::expect_equal(nrow(numeric_dep), 2L)
  testthat::expect_equal(numeric_dep[["Var 2"]], c("x1_sex", "x2_human"))
  testthat::expect_equal(
    numeric_dep[["Var 2"]],
    categorical_dep[["Var 2"]]
  )
})

testthat::test_that("sort_indep_by applies within each indep block", {
  data("ex_survey", package = "saros")

  columns <- function(...) {
    out <- suppressWarnings(makeme(
      data = ex_survey,
      dep = c_1,
      indep = c(x1_sex, x2_human),
      label_separator = NULL,
      type = "sigtest_table_html",
      showNA = "never",
      ...
    ))
    sub("^n_valid_", "", grep("^n_valid_", names(out), value = TRUE))
  }

  testthat::expect_equal(
    columns(sort_indep_by = ".factor_order"),
    c(levels(ex_survey$x1_sex), levels(ex_survey$x2_human))
  )

  # Ascending group size, resolved separately within each block rather than
  # pooled across both.
  testthat::expect_equal(
    columns(sort_indep_by = ".count"),
    c(
      names(sort(table(ex_survey$x1_sex))),
      names(sort(table(ex_survey$x2_human)))
    )
  )
})

testthat::test_that("get_indep_level_order reads the block it is asked about", {
  data("ex_survey", package = "saros")

  summary <- saros:::summarize_int_cat_data(
    ex_survey,
    dep = "c_1",
    indep = c("x1_sex", "x2_human")
  )

  # `.indep_order` restarts per block, so without narrowing by `.indep_name`
  # the blocks would interleave.
  testthat::expect_equal(
    saros:::get_indep_level_order(summary, ex_survey, "x1_sex"),
    levels(ex_survey$x1_sex)
  )
  testthat::expect_equal(
    saros:::get_indep_level_order(summary, ex_survey, "x2_human"),
    levels(ex_survey$x2_human)
  )
})

testthat::test_that("a single indep is unchanged by the stacking fix", {
  data("ex_survey", package = "saros")

  summary <- saros:::summarize_int_cat_data(
    ex_survey,
    dep = "c_1",
    indep = "x1_sex"
  )

  # No `.indep_name` column when there is nothing to disambiguate.
  testthat::expect_false(".indep_name" %in% names(summary))
  testthat::expect_equal(nrow(summary), 2L)
  testthat::expect_equal(summary$.indep_order, c(1L, 2L))
})

testthat::test_that("cat_* and int_* still reject several indeps by design", {
  data("ex_survey", package = "saros")

  for (type in c("cat_table_html", "int_table_html")) {
    dep <- if (type == "cat_table_html") rlang::expr(b_1) else rlang::expr(c_1)
    testthat::expect_error(
      suppressWarnings(rlang::eval_tidy(rlang::expr(makeme(
        data = ex_survey,
        dep = !!dep,
        indep = c(x1_sex, x2_human),
        type = !!type
      )))),
      regexp = "Only 1 indep-column",
      info = type
    )
  }
})
