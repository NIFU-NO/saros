# Regression guard for #617: `sort_indep_by = ".variable_label"` ordered the
# categories of the independent variable by factor level, because `rank()` on
# a factor ranks by level code. That made the key a silent duplicate of
# `".factor_order"`, while `?makeme`, `vignette("sorting")` and the dep-side
# `sort_dep_by = ".variable_label"` all mean alphabetical.

grp_levels <- c("Zebra", "Apple", "Mango")
alphabetical <- c("Apple", "Mango", "Zebra")

make_grp_data <- function(seed = 617L) {
  data <- withr::with_seed(seed, {
    n <- 240L
    data.frame(
      grp = factor(sample(grp_levels, n, replace = TRUE), levels = grp_levels),
      dep_1 = factor(sample(c("Low", "High"), n, replace = TRUE)),
      score = stats::rnorm(n)
    )
  })
  attr(data$dep_1, "label") <- "Dependent 1"
  attr(data$score, "label") <- "Score"
  data
}

cat_order <- function(data, showNA = "never", ...) {
  out <- makeme(
    data = data,
    dep = dep_1,
    indep = grp,
    label_separator = NULL,
    type = "cat_table_html",
    showNA = showNA,
    ...
  )
  as.character(unique(as.data.frame(out)[["grp"]]))
}

int_order <- function(data, ...) {
  out <- makeme(
    data = data,
    dep = score,
    indep = grp,
    label_separator = NULL,
    type = "int_table_html",
    showNA = "never",
    ...
  )
  as.character(as.data.frame(out)[["grp"]])
}

testthat::test_that(".variable_label orders the indep alphabetically, not by level", {
  data <- make_grp_data()

  testthat::expect_equal(
    cat_order(data, sort_indep_by = ".variable_label"),
    alphabetical
  )
  # The default is untouched, and still gives what `.variable_label` used to.
  testthat::expect_equal(
    cat_order(data, sort_indep_by = ".factor_order"),
    grp_levels
  )
  testthat::expect_equal(cat_order(data), grp_levels)
})

testthat::test_that("descend_indep reverses the alphabetical order", {
  data <- make_grp_data()

  testthat::expect_equal(
    cat_order(data, sort_indep_by = ".variable_label", descend_indep = TRUE),
    rev(alphabetical)
  )
})

testthat::test_that("a numeric dep orders its indep the same way", {
  data <- make_grp_data()

  testthat::expect_equal(
    int_order(data, sort_indep_by = ".variable_label"),
    alphabetical
  )
  testthat::expect_equal(
    int_order(data, sort_indep_by = ".variable_label", descend_indep = TRUE),
    rev(alphabetical)
  )
})

testthat::test_that("an NA category stays last under .variable_label", {
  data <- make_grp_data()
  data$grp[seq_len(10)] <- NA

  testthat::expect_equal(
    cat_order(data, showNA = "ifany", sort_indep_by = ".variable_label"),
    c(alphabetical, "NA")
  )
  testthat::expect_equal(
    cat_order(
      data,
      showNA = "ifany",
      sort_indep_by = ".variable_label",
      descend_indep = TRUE
    ),
    c("NA", rev(alphabetical))
  )
})

testthat::test_that("rank_labels_alphabetically() ranks text, ties within a category, NA last", {
  x <- factor(c("b", "a", "NA", "a", NA), levels = c("b", "a", "NA"))
  testthat::expect_equal(
    saros:::rank_labels_alphabetically(x),
    c(2L, 1L, 3L, 1L, NA)
  )
  # Plain character input is accepted as well
  testthat::expect_equal(
    saros:::rank_labels_alphabetically(c("Zebra", "Apple")),
    c(2L, 1L)
  )
})
