# Regression guard for #609: calling sigtest_table_html without an `indep`
# emitted two spurious warnings per dependent variable:
#
#   Unknown or uninitialised column: `x`.
#
# make_content.sigtest_table_html() builds its work list with
# tidyr::expand_grid(y = dots$dep, x = dots$indep). With no indep that tibble
# has no `x` column at all, and `$` warns once per access for a column a
# tibble does not have, where `[[` returns NULL silently. Both call sites
# already handled NULL correctly, so only the accessor was wrong -- the output
# was never affected, but the noise scaled with the number of dependent
# variables (a 65-variable chapter produced 130 warnings).

make_no_indep_data <- function(n_dep = 3L) {
  withr::with_seed(609L, {
    out <- data.frame(
      row.names = seq_len(100L)
    )
    for (i in seq_len(n_dep)) {
      out[[paste0("dep_", i)]] <- factor(
        sample(c("Low", "High"), 100L, replace = TRUE)
      )
    }
    out$g <- factor(sample(c("a", "b"), 100L, replace = TRUE))
    out
  })
}

# Collects only the uninitialised-column warnings this file is about. Any
# other warning is deliberately left to propagate, so an unexpected warning
# stays visible in the test output instead of being silently swallowed --
# the pattern established in test-issue-457-ggiraph-na-warning.R.
uninitialised_warnings <- function(expr) {
  warnings_seen <- character(0)
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (
        grepl(
          "Unknown or uninitialised column",
          conditionMessage(w),
          fixed = TRUE
        )
      ) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    }
  )
  warnings_seen
}

testthat::test_that("sigtest_table_html does not warn about `x` when there is no indep", {
  # The count scaled with the number of dependent variables, so more than one
  # is needed to catch a partial fix.
  for (n_dep in 1:3) {
    data <- make_no_indep_data(n_dep)

    testthat::expect_equal(
      uninitialised_warnings(
        makeme(
          data = data,
          dep = tidyselect::all_of(paste0("dep_", seq_len(n_dep))),
          label_separator = NULL,
          type = "sigtest_table_html",
          showNA = "never"
        )
      ),
      character(0)
    )
  }
})

testthat::test_that("the no-indep table is still produced correctly", {
  data <- make_no_indep_data(3L)

  out <- makeme(
    data = data,
    dep = tidyselect::all_of(paste0("dep_", 1:3)),
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_equal(out[["Var 1"]], paste0("dep_", 1:3))
  # No indep means no second variable and no per-category columns.
  testthat::expect_false("Var 2" %in% names(out))
  testthat::expect_true(all(c("n_valid", "n") %in% names(out)))
})

testthat::test_that("passing an indep is unaffected", {
  data <- make_no_indep_data(2L)

  testthat::expect_equal(
    uninitialised_warnings(
      makeme(
        data = data,
        dep = tidyselect::all_of(paste0("dep_", 1:2)),
        indep = g,
        label_separator = NULL,
        type = "sigtest_table_html",
        showNA = "never"
      )
    ),
    character(0)
  )
})

testthat::test_that("a variable selected as both dep and indep is still dropped", {
  data <- make_no_indep_data(2L)

  out <- makeme(
    data = data,
    dep = c(dep_1, g),
    indep = g,
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never"
  )

  # The `y_var != x_var` guard is part of the condition this fix rewrote.
  testthat::expect_equal(out[["Var 1"]], "dep_1")
})
