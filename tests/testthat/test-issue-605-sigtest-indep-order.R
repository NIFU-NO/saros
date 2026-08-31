# Regression guard for #605: the per-category columns of a wide
# sigtest_table_html came out in order of appearance in the data rather than in
# the intended category order, and `sort_indep_by` had no effect on them at all
# -- not even its documented default ".factor_order".
#
# simple_descriptives() summarises with `.by =`, which returns groups in order
# of first appearance, and pivot_wider() then took the column order from those
# rows. Nothing re-sorted afterwards, so the same variable produced a different
# column order depending on how the input rows happened to be arranged.
#
# The order now comes from get_indep_level_order(), which reads `.indep_order`
# off data_summary -- where add_indep_order() has already resolved
# `sort_indep_by` and `descend_indep` -- so sigtest tables order their
# categories exactly like every other content type.

sektor_levels <- c("State", "County", "Municipal", "Private", "Other")

make_order_data <- function(seed = 605L) {
  withr::with_seed(seed, {
    n <- 300L
    data.frame(
      sektor = factor(
        sample(
          sektor_levels,
          n,
          replace = TRUE,
          prob = c(.35, .25, .2, .15, .05)
        ),
        levels = sektor_levels
      ),
      dep_1 = factor(sample(c("Low", "High"), n, replace = TRUE)),
      score = round(stats::rnorm(n, mean = 10, sd = 3))
    )
  })
}

# The per-category columns of the wide table, in the order they appear.
indep_columns <- function(x) {
  sub("^n_valid_", "", grep("^n_valid_", names(x), value = TRUE))
}

sigtest_order <- function(data, ...) {
  indep_columns(makeme(
    data = data,
    dep = dep_1,
    indep = sektor,
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never",
    ...
  ))
}

testthat::test_that("sigtest_table_html orders indep columns by factor level, not by appearance", {
  data <- make_order_data()

  testthat::expect_equal(sigtest_order(data), sektor_levels)

  # Neither shuffling the rows nor introducing NAs may move the columns.
  shuffled <- withr::with_seed(1L, data[sample(nrow(data)), , drop = FALSE])
  testthat::expect_equal(sigtest_order(shuffled), sektor_levels)

  with_na <- data
  with_na$sektor[1:6] <- NA
  testthat::expect_equal(sigtest_order(with_na), sektor_levels)
})

testthat::test_that("sigtest_table_html honours sort_indep_by and descend_indep", {
  data <- make_order_data()

  # Ascending by group size: Other is the smallest, State the largest.
  by_count <- names(sort(table(data$sektor)))
  testthat::expect_equal(
    sigtest_order(data, sort_indep_by = ".count"),
    by_count
  )
  testthat::expect_equal(
    sigtest_order(data, sort_indep_by = ".count", descend_indep = TRUE),
    rev(by_count)
  )
  testthat::expect_equal(
    sigtest_order(data, descend_indep = TRUE),
    rev(sektor_levels)
  )
})

testthat::test_that("sigtest_table_html orders its categories like cat_table_html", {
  data <- make_order_data()

  cat_table_order <- function(...) {
    out <- makeme(
      data = data,
      dep = dep_1,
      indep = sektor,
      label_separator = NULL,
      type = "cat_table_html",
      showNA = "never",
      ...
    )
    as.character(unique(as.data.frame(out)[["sektor"]]))
  }

  for (args in list(
    list(),
    list(sort_indep_by = ".count"),
    list(descend_indep = TRUE),
    list(sort_indep_by = ".count", descend_indep = TRUE),
    list(sort_indep_by = ".count_per_indep_group")
  )) {
    testthat::expect_equal(
      do.call(sigtest_order, c(list(data), args)),
      do.call(cat_table_order, args),
      info = paste(names(args), unlist(args), sep = "=", collapse = ", ")
    )
  }
})

testthat::test_that("an ordered indep keeps its level order regardless of sort_indep_by", {
  data <- make_order_data()
  data$sektor <- factor(data$sektor, levels = sektor_levels, ordered = TRUE)

  # add_indep_order() gives ordered factors precedence over sort_by.
  testthat::expect_equal(
    sigtest_order(data, sort_indep_by = ".count"),
    sektor_levels
  )
})

testthat::test_that("the NA column sorts last when showNA keeps it", {
  data <- make_order_data()
  data$sektor[1:6] <- NA

  out <- makeme(
    data = data,
    dep = dep_1,
    indep = sektor,
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "always"
  )
  testthat::expect_equal(indep_columns(out), c(sektor_levels, "NA"))
})

testthat::test_that("the long-format fallback is ordered too", {
  data <- withr::with_seed(605L, {
    data.frame(
      sektor = factor(
        sample(LETTERS[1:20], 300L, replace = TRUE),
        levels = LETTERS[1:20]
      ),
      dep_1 = factor(sample(c("Low", "High"), 300L, replace = TRUE))
    )
  })

  # 20 categories is above the default n_categories_limit of 12, so this goes
  # long -- the rows must be ordered for the same reason the columns are.
  out <- makeme(
    data = data,
    dep = dep_1,
    indep = sektor,
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never"
  )
  testthat::expect_equal(as.character(out$sektor), LETTERS[1:20])
})

testthat::test_that("a numeric dep falls back to the factor's own levels", {
  data <- make_order_data()

  # summarize_int_cat_data() does no sorting, so there is no .indep_order to
  # read; the factor levels are the fallback, matching arrange_table_data().
  out <- makeme(
    data = data,
    dep = score,
    indep = sektor,
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never"
  )
  testthat::expect_equal(indep_columns(out), sektor_levels)
})
