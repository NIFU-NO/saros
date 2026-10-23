# #626: `hide_for_crowd_if_all_na` is deprecated. `FALSE` never took effect --
# keep_cols() dropped an all-NA variable through its category rule whatever
# the argument said, and process_crowd_data() drops every all-NA dependent
# variable again -- so the argument now only warns.

all_na_data <- function() {
  d <- saros::ex_survey
  d$all_na_var <- factor(NA, levels = c("No", "Yes"))
  attr(d$all_na_var, "label") <- "All NA Test Variable"
  d
}

table_of <- function(...) {
  makeme(
    data = all_na_data(),
    dep = c(a_1, all_na_var),
    type = "cat_table_html",
    hide_for_crowd_if_category_k_below = 0,
    ...
  )
}

testthat::test_that("supplying the argument warns that it is deprecated", {
  # The warning is shown "regularly"; make it fire on every call here.
  withr::local_options(rlib_warning_verbosity = "verbose")

  testthat::expect_warning(
    table_of(hide_for_crowd_if_all_na = FALSE),
    regexp = "deprecated and has no effect"
  )
  testthat::expect_warning(
    table_of(hide_for_crowd_if_all_na = TRUE),
    regexp = "deprecated and has no effect"
  )
})

testthat::test_that("leaving it out does not warn", {
  withr::local_options(rlib_warning_verbosity = "verbose")

  testthat::expect_no_warning(
    table_of(),
    message = "hide_for_crowd_if_all_na"
  )
})

testthat::test_that("a global FALSE warns, a global TRUE does not", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  withr::defer(global_settings_reset(fn_name = "makeme", quiet = TRUE))

  global_settings_set(
    fn_name = "makeme",
    new = list(hide_for_crowd_if_all_na = FALSE),
    quiet = TRUE
  )
  testthat::expect_warning(table_of(), regexp = "deprecated and has no effect")

  global_settings_set(
    fn_name = "makeme",
    new = list(hide_for_crowd_if_all_na = TRUE),
    quiet = TRUE
  )
  testthat::expect_no_warning(table_of(), message = "hide_for_crowd_if_all_na")
})

testthat::test_that("output is the same whatever the argument says", {
  default <- table_of()
  testthat::expect_equal(nrow(default), 1) # the all-NA variable is hidden
  testthat::expect_identical(
    suppressWarnings(table_of(hide_for_crowd_if_all_na = FALSE)),
    default
  )
})

testthat::test_that("keep_cols() no longer takes the argument", {
  testthat::expect_false(
    "hide_for_crowd_if_all_na" %in% names(formals(saros:::keep_cols))
  )
})
