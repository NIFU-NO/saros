# Regression guard for #627: validate_makeme_options() keyed its rule on
# `hide_for_all_if_hidden_for_crowd`, which is not an argument of makeme(),
# so `hide_for_all_crowds_if_hidden_for_crowd` was never checked. A typo such
# as "targte" was accepted and silently turned the rule off. The misspelled
# name itself, which the ?makeme example used until #623, went into `...`
# and was ignored.

mesos_call <- function(...) {
  makeme(
    data = saros::ex_survey,
    dep = b_1,
    type = "cat_table_html",
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    simplify_output = FALSE,
    ...
  )
}

testthat::test_that("an invalid value warns and falls back to NULL", {
  testthat::expect_warning(
    mesos_call(hide_for_all_crowds_if_hidden_for_crowd = 42),
    regexp = "hide_for_all_crowds_if_hidden_for_crowd.*invalid"
  )
  testthat::expect_warning(
    mesos_call(hide_for_all_crowds_if_hidden_for_crowd = "targte"),
    regexp = "hide_for_all_crowds_if_hidden_for_crowd.*invalid"
  )
  testthat::expect_warning(
    mesos_call(hide_for_all_crowds_if_hidden_for_crowd = character(0)),
    regexp = "hide_for_all_crowds_if_hidden_for_crowd.*invalid"
  )
})

testthat::test_that("valid values pass without a warning", {
  for (value in list(NULL, "target", "others", "all", c("target", "all"))) {
    testthat::expect_no_warning(
      mesos_call(hide_for_all_crowds_if_hidden_for_crowd = value),
      message = "invalid"
    )
  }
})

testthat::test_that("the fallback leaves the rule off rather than half-applied", {
  # With a bad value replaced by NULL, the result is the same as not setting it.
  testthat::expect_identical(
    suppressWarnings(mesos_call(hide_for_all_crowds_if_hidden_for_crowd = "targte")),
    mesos_call()
  )
})

testthat::test_that("the misspelled argument name warns and points to the right one", {
  testthat::expect_warning(
    makeme(
      data = saros::ex_survey,
      dep = b_1,
      hide_for_all_if_hidden_for_crowd = "target"
    ),
    regexp = "hide_for_all_crowds_if_hidden_for_crowd"
  )
})

testthat::test_that("every rule in validate_makeme_options() names an argument of makeme()", {
  # Walk the function body for the `arg_params <- list(...)` assignment and
  # read the names of its rules, so the next misspelling is caught here.
  rule_names <- NULL
  walk <- function(expr) {
    if (!is.call(expr)) {
      return(invisible())
    }
    if (
      identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("arg_params"))
    ) {
      rule_names <<- setdiff(names(expr[[3]]), "")
    }
    parts <- as.list(expr)
    for (i in seq_along(parts)[-1]) {
      if (is.call(parts[[i]])) walk(parts[[i]])
    }
  }
  walk(body(saros:::validate_makeme_options))

  testthat::expect_true(length(rule_names) > 10)
  testthat::expect_true("hide_for_all_crowds_if_hidden_for_crowd" %in% rule_names)
  testthat::expect_equal(
    setdiff(rule_names, names(formals(makeme))),
    character(0)
  )
})
