# Regression guard for #614: `add_indep_order()` built the `.variable_label`
# order column with `order(-rank(x))`, which mixes rank and permutation
# semantics.
#
# `.indep_order` is consumed as a per-row rank, but `order()` returns a
# permutation. The two only coincide when every category contributes the same
# number of rows to the summary:
#
#   values      : a a a b b b c c c
#   rank(x)     : 2 2 2 5 5 5 8 8 8
#   order(-rank): 7 8 9 4 5 6 1 2 3   <- what was stored
#   correct desc: 7 7 7 4 4 4 1 1 1
#
# With equal blocks the minimum within each block still orders the categories
# correctly, so the mistake cancelled out and went unnoticed. With unequal
# blocks it did not, and `descend_indep = TRUE` returned a scrambled order
# rather than a reversal.
#
# Uneven blocks are ordinary: a group where nobody chose a given response
# contributes fewer rows, and `showNA = "ifany"` adds a row only to the groups
# that have missing values.
#
# Note this covers the *reversal* only. That ascending order follows factor
# levels rather than the documented alphabetical order is #617.

group_levels <- c("Alpha", "Beta", "Gamma", "Delta")

make_label_data <- function(seed = 614L) {
  withr::with_seed(seed, {
    n <- 400L
    data.frame(
      grp = factor(
        sample(group_levels, n, replace = TRUE),
        levels = group_levels
      ),
      dep_1 = factor(sample(c("Low", "High"), n, replace = TRUE))
    )
  })
}

indep_order <- function(data, showNA = "never", ...) {
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

testthat::test_that("descend_indep reverses .variable_label with even blocks", {
  data <- make_label_data()

  testthat::expect_equal(
    indep_order(data, sort_indep_by = ".variable_label"),
    group_levels
  )
  testthat::expect_equal(
    indep_order(data, sort_indep_by = ".variable_label", descend_indep = TRUE),
    rev(group_levels)
  )
})

testthat::test_that("descend_indep reverses .variable_label when a group is missing a category", {
  data <- make_label_data()
  # Alpha and Beta answered only "Low", so they contribute one summary row
  # each where Gamma and Delta contribute two.
  data$dep_1[data$grp %in% c("Alpha", "Beta")] <- "Low"

  testthat::expect_equal(
    indep_order(data, sort_indep_by = ".variable_label"),
    group_levels
  )
  # This returned Delta | Gamma | Alpha | Beta before the fix.
  testthat::expect_equal(
    indep_order(data, sort_indep_by = ".variable_label", descend_indep = TRUE),
    rev(group_levels)
  )
})

testthat::test_that("descend_indep reverses .variable_label when NA is confined to some groups", {
  data <- make_label_data()
  # Only Gamma has missing values, so only Gamma gains an NA row.
  gamma <- which(data$grp == "Gamma")
  data$dep_1[gamma[seq_len(5)]] <- NA

  testthat::expect_equal(
    indep_order(data, sort_indep_by = ".variable_label", showNA = "ifany"),
    group_levels
  )
  testthat::expect_equal(
    indep_order(
      data,
      sort_indep_by = ".variable_label",
      showNA = "ifany",
      descend_indep = TRUE
    ),
    rev(group_levels)
  )
})

testthat::test_that("the order column is a rank, with ties within a category", {
  data <- make_label_data()

  summary <- saros:::summarize_cat_cat_data(
    data = data,
    dep = "dep_1",
    indep = "grp",
    sort_indep_by = ".variable_label",
    descend_indep = TRUE
  )

  # Every row of one category must carry the same order value; `order()`
  # returned a distinct value per row, which is what made it not a rank.
  per_category <- tapply(
    summary$.indep_order,
    as.character(summary$grp),
    function(x) length(unique(x))
  )
  testthat::expect_true(all(per_category == 1L))
})
