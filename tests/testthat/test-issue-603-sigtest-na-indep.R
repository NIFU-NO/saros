# Regression guard for #603: `makeme(type = "sigtest_table_html")` aborted with
#
#   Error in `tidyr::pivot_wider()`:
#   i In argument: `tidyselect::all_of(x_var)`.
#   x Element `ny_sektor` doesn't exist.
#
# whenever the independent variable contained NA.
#
# Root cause was in simple_descriptives(), which dropped the grouping variable
# when it had more than `max_k` (5) distinct values. `unique()` counts NA as a
# value, so an indep with exactly five observed categories plus any NA tripped
# that hidden limit. The drop happened inside the per-y_var loop, so it was
# invisible to the caller-level `x_var` still used by the wide pivot -- hence
# the "doesn't exist" abort. The NA rows were only removed *after* the check,
# which is why every `showNA` setting failed identically.
#
# NA was therefore necessary-but-not-sufficient: an indep with *more* than five
# categories broke the same way with no NA at all, in sigtest_table_html,
# int_plot_html (at draw time) and int_table_html.

make_sigtest_data <- function(n_categories, n_na = 0L, seed = 603L) {
  withr::with_seed(seed, {
    n <- 200L
    out <- data.frame(
      sektor = factor(
        sample(LETTERS[seq_len(n_categories)], n, replace = TRUE),
        levels = LETTERS[seq_len(n_categories)]
      ),
      dep_1 = factor(sample(c("Low", "High"), n, replace = TRUE)),
      dep_2 = factor(sample(c("Low", "High"), n, replace = TRUE))
    )
  })
  if (n_na > 0L) {
    out$sektor[seq_len(n_na)] <- NA
  }
  out
}

sigtest <- function(data, ...) {
  makeme(
    data = data,
    dep = c(dep_1, dep_2),
    indep = sektor,
    label_separator = NULL,
    type = "sigtest_table_html",
    ...
  )
}

testthat::test_that("sigtest_table_html keeps the indep grouping when it contains NA", {
  reference <- sigtest(make_sigtest_data(5L))
  with_na <- sigtest(make_sigtest_data(5L, n_na = 3L), showNA = "never")

  # One row per dep, and the same set of columns as without the NAs. Only the
  # *order* of the per-category columns may differ, since the wide pivot
  # follows order of appearance in the data rather than factor level order.
  testthat::expect_equal(nrow(with_na), 2L)
  testthat::expect_setequal(names(with_na), names(reference))
  testthat::expect_true(all(
    paste0("n_valid_", LETTERS[1:5]) %in% names(with_na)
  ))
})

testthat::test_that("sigtest_table_html tolerates NA in indep for every showNA setting", {
  data <- make_sigtest_data(5L, n_na = 3L)

  for (show_na in c("never", "ifany", "always")) {
    out <- testthat::expect_no_error(sigtest(data, showNA = show_na))
    testthat::expect_equal(nrow(out), 2L)
    # "never" drops the NA group, the other two keep it as its own column.
    testthat::expect_equal(
      "n_valid_NA" %in% names(out),
      show_na %in% c("ifany", "always")
    )
  }
})

testthat::test_that("sigtest_table_html groups by an indep with more categories than the old max_k", {
  out <- sigtest(make_sigtest_data(8L))

  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(
    paste0("n_valid_", LETTERS[1:8]) %in% names(out)
  ))
})

testthat::test_that("sigtest_table_html falls back to long format above n_categories_limit", {
  data <- make_sigtest_data(20L)

  wide <- sigtest(data, n_categories_limit = 25)
  testthat::expect_equal(nrow(wide), 2L)
  testthat::expect_true("n_valid_A" %in% names(wide))

  # Default n_categories_limit is 12, so 20 categories go long: one row per
  # dep and category, with the categories in a column named after `indep`.
  long <- sigtest(data)
  testthat::expect_equal(nrow(long), 2L * 20L)
  testthat::expect_true("sektor" %in% names(long))
  testthat::expect_false("n_valid_A" %in% names(long))
  testthat::expect_setequal(as.character(long$sektor), LETTERS[1:20])
})

testthat::test_that("sigtest_table_html does not collide with variables named x or y", {
  data <- make_sigtest_data(20L)
  names(data) <- c("x", "y", "dep_2")

  out <- makeme(
    data = data,
    dep = c(y, dep_2),
    indep = x,
    label_separator = NULL,
    type = "sigtest_table_html"
  )

  # The long format keeps `indep`'s own name; the placeholder columns must
  # already carry their translated headers so nothing is name-repaired.
  testthat::expect_true(all(c("Var 1", "Var 2", "x") %in% names(out)))
  testthat::expect_false(any(grepl("...", names(out), fixed = TRUE)))
  testthat::expect_equal(unique(out[["Var 2"]]), "x")
})

testthat::test_that("int_table_html and int_plot_html handle NA and many-category indep", {
  data <- withr::with_seed(603L, {
    data.frame(
      sektor = factor(sample(LETTERS[1:6], 200L, replace = TRUE)),
      score = round(stats::rnorm(200L, mean = 10, sd = 3))
    )
  })
  data_na <- data
  data_na$sektor[1:5] <- NA

  for (dataset in list(data, data_na)) {
    tbl <- makeme(
      data = dataset,
      dep = score,
      indep = sektor,
      label_separator = NULL,
      type = "int_table_html"
    )
    testthat::expect_equal(nrow(tbl), 6L)

    # The descriptives are attached as a geom layer, so a dropped grouping
    # column only surfaces when the plot is actually drawn.
    plot <- makeme(
      data = dataset,
      dep = score,
      indep = sektor,
      label_separator = NULL,
      type = "int_plot_html"
    )
    png_file <- withr::local_tempfile(fileext = ".png")
    testthat::expect_no_error(
      ggplot2::ggsave(png_file, plot, width = 5, height = 4)
    )
  }
})
