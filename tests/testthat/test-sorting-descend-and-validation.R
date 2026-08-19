# Regression tests for #599 (descend inverted on the multi-category dep sort)
# and #600 (.count_total_indep dead key / silent acceptance of invalid
# sort_indep_by keys).

dep_order <- function(...) {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    label_separator = " - ",
    showNA = "never",
    ...
  )
  as.character(out[[1]])
}

# ---------------------------------------------------------------------------
# #599: descend must mean the same thing for one label and for several
# ---------------------------------------------------------------------------

testthat::test_that("multi-category sort_dep_by honours descend (#599)", {
  # Summed values for these two categories, largest first:
  #   Bejing (171) > Budapest (161) > Brussels (156)
  testthat::expect_equal(
    dep_order(sort_dep_by = c("A bit", "A lot"), descend = TRUE),
    c("Bejing", "Budapest", "Brussels")
  )
  testthat::expect_equal(
    dep_order(sort_dep_by = c("A bit", "A lot"), descend = FALSE),
    c("Brussels", "Budapest", "Bejing")
  )
})

testthat::test_that("single- and multi-category sort_dep_by agree on descend (#599)", {
  # Both forms must put the largest first under descend = TRUE. The single-label
  # form was already correct; the multi-label form used to invert it.
  single_desc <- dep_order(sort_dep_by = "A lot", descend = TRUE)
  single_asc <- dep_order(sort_dep_by = "A lot", descend = FALSE)
  testthat::expect_equal(single_desc, rev(single_asc))

  multi_desc <- dep_order(sort_dep_by = c("A bit", "A lot"), descend = TRUE)
  multi_asc <- dep_order(sort_dep_by = c("A bit", "A lot"), descend = FALSE)
  testthat::expect_equal(multi_desc, rev(multi_asc))
})

# ---------------------------------------------------------------------------
# #600: sort_indep_by must reject what it cannot honour
# ---------------------------------------------------------------------------

indep_sorted <- function(sort_indep_by) {
  saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "cat_table_html",
    label_separator = " - ",
    showNA = "never",
    sort_indep_by = sort_indep_by
  )
}

testthat::test_that("supported sort_indep_by keys still work (#600)", {
  for (key in c(
    ".factor_order",
    ".variable_label",
    ".count",
    ".count_per_indep_group",
    ".top",
    ".bottom"
  )) {
    testthat::expect_no_error(indep_sorted(key))
  }
  testthat::expect_no_error(indep_sorted(NULL))
})

testthat::test_that("an unrecognised sort_indep_by key errors instead of being ignored (#600)", {
  # Previously these returned silently unsorted output, so a typo produced
  # wrong order with no indication anything was wrong.
  testthat::expect_error(indep_sorted(".totally_bogus"), "sort_indep_by")
  testthat::expect_error(indep_sorted(".coutn"), "sort_indep_by")
})

testthat::test_that("a .count-prefixed typo is not silently treated as .count (#600)", {
  testthat::expect_error(indep_sorted(".countBOGUS"), "sort_indep_by")
})

testthat::test_that(".count_total_indep is no longer offered as a valid key (#600)", {
  testthat::expect_false(
    ".count_total_indep" %in% saros:::.saros.env$allowed_indep_sort_columns
  )
  testthat::expect_error(indep_sorted(".count_total_indep"), "sort_indep_by")
})

testthat::test_that("a category label is still accepted for sort_indep_by (#600)", {
  testthat::expect_no_error(indep_sorted("A lot"))
  testthat::expect_error(indep_sorted("No such category"), "sort_indep_by")
})
