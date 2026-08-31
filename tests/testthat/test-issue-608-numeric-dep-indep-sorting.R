# Regression guard for #608: `sort_indep_by` and `descend_indep` were accepted,
# validated, and then silently discarded for a *numeric* dependent variable.
# generate_data_summary() passed them to summarize_cat_cat_data() but not to
# summarize_int_cat_data(), which never called the sorting machinery, so the
# summary carried no `.indep_order` and every consumer fell back to the
# factor's own levels. int_table_html, int_plot_html and sigtest_table_html
# were all affected -- even `descend_indep`, which is not a sort key at all.
#
# The keys that need a `.category`/`.proportion` column have no meaning for a
# numeric dependent variable. Rather than being ignored they now abort, in the
# spirit of #600.

sektor_levels <- c("State", "County", "Municipal", "Private", "Other")

make_int_data <- function(seed = 608L) {
  data <- withr::with_seed(seed, {
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
  # Give the groups clearly distinct means, so a mean-based order is
  # unambiguous and cannot coincide with the factor or count order.
  offsets <- c(
    State = 0,
    County = 5,
    Municipal = -5,
    Private = 10,
    Other = -10
  )
  data$score <- data$score + offsets[as.character(data$sektor)]
  data
}

indep_columns <- function(x) {
  sub("^n_valid_", "", grep("^n_valid_", names(x), value = TRUE))
}

sigtest_order <- function(data, dep, ...) {
  indep_columns(makeme(
    data = data,
    dep = {{ dep }},
    indep = sektor,
    label_separator = NULL,
    type = "sigtest_table_html",
    showNA = "never",
    ...
  ))
}

testthat::test_that("a numeric dep honours sort_indep_by", {
  data <- make_int_data()
  by_size <- names(sort(table(data$sektor)))
  # Derived separately, so that a `.median` that silently used the mean (or
  # vice versa) would still be caught.
  by_mean <- names(sort(tapply(data$score, data$sektor, mean)))
  by_median <- names(sort(tapply(data$score, data$sektor, stats::median)))

  testthat::expect_equal(
    sigtest_order(data, score, sort_indep_by = ".factor_order"),
    sektor_levels
  )
  testthat::expect_equal(
    sigtest_order(data, score, sort_indep_by = ".count"),
    by_size
  )
  testthat::expect_equal(
    sigtest_order(data, score, sort_indep_by = ".count_per_indep_group"),
    by_size
  )
  testthat::expect_equal(
    sigtest_order(data, score, sort_indep_by = ".mean"),
    by_mean
  )
  testthat::expect_equal(
    sigtest_order(data, score, sort_indep_by = ".median"),
    by_median
  )
})

testthat::test_that("a numeric dep honours descend_indep", {
  data <- make_int_data()
  by_mean <- names(sort(tapply(data$score, data$sektor, mean)))

  testthat::expect_equal(
    sigtest_order(data, score, descend_indep = TRUE),
    rev(sektor_levels)
  )
  testthat::expect_equal(
    sigtest_order(data, score, sort_indep_by = ".mean", descend_indep = TRUE),
    rev(by_mean)
  )
})

testthat::test_that("numeric and categorical deps agree on the dep-independent keys", {
  data <- make_int_data()

  # .mean and .median are deliberately excluded: for a categorical dep they
  # summarise ordinal category codes, so the two are not comparable.
  for (key in c(
    ".factor_order",
    ".variable_label",
    ".count",
    ".count_per_indep_group"
  )) {
    for (descend in c(FALSE, TRUE)) {
      testthat::expect_equal(
        sigtest_order(
          data,
          score,
          sort_indep_by = key,
          descend_indep = descend
        ),
        sigtest_order(
          data,
          dep_1,
          sort_indep_by = key,
          descend_indep = descend
        ),
        info = paste0(key, ", descend_indep = ", descend)
      )
    }
  }
})

testthat::test_that("keys a numeric dep cannot honour abort rather than being ignored", {
  data <- make_int_data()

  for (key in c(
    ".top",
    ".bottom",
    ".upper",
    ".lower",
    ".mid_upper",
    ".sum_value"
  )) {
    testthat::expect_error(
      sigtest_order(data, score, sort_indep_by = key),
      regexp = "numeric dependent variable",
      info = key
    )
  }

  # Category labels have nothing to refer to either.
  testthat::expect_error(
    sigtest_order(data, score, sort_indep_by = "Low"),
    regexp = "numeric dependent variable"
  )
  testthat::expect_error(
    sigtest_order(data, score, sort_indep_by = c("Low", "High")),
    regexp = "numeric dependent variable"
  )
})

testthat::test_that("a vector of otherwise-supported keys is rejected by name", {
  data <- make_int_data()

  # setdiff() against the whitelist is empty here, so the message has to fall
  # back to reporting what was actually passed rather than nothing.
  testthat::expect_error(
    sigtest_order(data, score, sort_indep_by = c(".mean", ".median")),
    regexp = ".mean",
    fixed = TRUE
  )
  testthat::expect_error(
    sigtest_order(data, score, sort_indep_by = c(".mean", ".median")),
    regexp = "single key",
    fixed = TRUE
  )
})

testthat::test_that("the error names the offending key and what is supported", {
  data <- make_int_data()

  testthat::expect_error(
    sigtest_order(data, score, sort_indep_by = ".top"),
    regexp = ".top", fixed = TRUE
  )
  testthat::expect_error(
    sigtest_order(data, score, sort_indep_by = ".top"),
    regexp = ".factor_order", fixed = TRUE
  )
})

testthat::test_that("int_table_html orders its rows by sort_indep_by", {
  data <- make_int_data()
  by_size <- names(sort(table(data$sektor)))

  int_table_order <- function(...) {
    out <- makeme(
      data = data,
      dep = score,
      indep = sektor,
      label_separator = NULL,
      type = "int_table_html",
      ...
    )
    as.character(as.data.frame(out)[["sektor"]])
  }

  testthat::expect_equal(int_table_order(), sektor_levels)
  testthat::expect_equal(int_table_order(sort_indep_by = ".count"), by_size)
  testthat::expect_equal(
    int_table_order(descend_indep = TRUE),
    rev(sektor_levels)
  )
  testthat::expect_error(
    int_table_order(sort_indep_by = ".top"),
    regexp = "numeric dependent variable"
  )
})

testthat::test_that("int_plot_html orders its axis by sort_indep_by", {
  data <- make_int_data()
  by_size <- names(sort(table(data$sektor)))

  plot_levels <- function(...) {
    p <- makeme(
      data = data,
      dep = score,
      indep = sektor,
      label_separator = NULL,
      type = "int_plot_html",
      ...
    )
    levels(p$data[["sektor"]])
  }

  testthat::expect_equal(plot_levels(), sektor_levels)
  testthat::expect_equal(plot_levels(sort_indep_by = ".count"), by_size)
  testthat::expect_error(
    plot_levels(sort_indep_by = ".top"),
    regexp = "numeric dependent variable"
  )
})

testthat::test_that("reordering the indep keeps its variable label", {
  # factor() returns a value carrying only `levels` and `class`, so reordering
  # levels the naive way drops the variable label the axis titles read -- the
  # same class of bug as #603.
  data("ex_survey", package = "saros")

  p <- makeme(
    data = ex_survey,
    dep = c_1,
    indep = x1_sex,
    type = "int_plot_html",
    sort_indep_by = ".count"
  )
  testthat::expect_equal(attr(p$data[["x1_sex"]], "label"), "Gender")

  out <- saros:::simple_descriptives(
    ex_survey,
    y_var = "c_1",
    x_var = "x1_sex",
    x_levels = c("Females", "Males")
  )
  testthat::expect_equal(attr(out[["x1_sex"]], "label"), "Gender")
})

testthat::test_that("an ordered indep keeps its level order for a numeric dep", {
  data <- make_int_data()
  data$sektor <- factor(data$sektor, levels = sektor_levels, ordered = TRUE)

  testthat::expect_equal(
    sigtest_order(data, score, sort_indep_by = ".count"),
    sektor_levels
  )
})
