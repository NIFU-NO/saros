# Regression guard for #620: `add_n_to_indep_label = TRUE` dropped the
# independent variable out of the cat_plot layout. add_n_to_label() keeps the
# pre-suffix labels in a `<indep>_original` companion column, and the
# independent variable was identified by excluding the standard summary
# columns, so the companion passed for a second independent variable and the
# plot fell back to the layout for no indep -- the item label on the axis and
# again in the strip, with no split by group. fig_height_h_barchart2() aborted
# on the same miscount.

lv <- c("Not at all", "To a small degree", "To some degree", "To a large degree")

make_cycle_data <- function(seed = 620L) {
  data <- withr::with_seed(seed, {
    n <- 400L
    data.frame(
      cycle = as.character(sample(2023:2026, n, replace = TRUE)),
      item1 = factor(sample(lv, n, replace = TRUE), levels = lv, ordered = TRUE),
      item2 = factor(sample(lv, n, replace = TRUE), levels = lv, ordered = TRUE)
    )
  })
  attr(data$item1, "label") <- "Battery - Item 1"
  attr(data$item2, "label") <- "Battery - Item 2"
  data
}

y_axis_labels <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
}

strip_suffix <- function(x) sub(" \\(N = [0-9]+\\)$", "", x)

cycle_plot <- function(data, dep, ..., type = "cat_plot_html") {
  makeme(
    data = data,
    dep = {{ dep }},
    indep = cycle,
    label_separator = " - ",
    type = type,
    ...
  )
}

testthat::test_that("get_indep_col_names() leaves out the _original companions", {
  data <- make_cycle_data()
  summary <- saros:::summarize_cat_cat_data(
    data,
    dep = "item1",
    indep = "cycle",
    add_n_to_indep_label = TRUE,
    add_n_to_dep_label = TRUE
  )
  testthat::expect_true(
    all(c("cycle_original", ".variable_label_original") %in% names(summary))
  )
  testthat::expect_equal(saros:::get_indep_col_names(summary), "cycle")
  testthat::expect_equal(saros:::get_indep_col_name(summary), "cycle")

  # No indep at all, and the int_plot_html data column
  testthat::expect_equal(
    saros:::get_indep_col_names(
      saros:::summarize_cat_cat_data(data, dep = "item1")
    ),
    character(0)
  )
  testthat::expect_equal(
    saros:::get_indep_col_names(
      data.frame(.variable_name = "a", .variable_label = "A", .value = 1, grp = "x")
    ),
    "grp"
  )
})

testthat::test_that("add_n_to_indep_label keeps the indep on the axis of cat_plot_html", {
  data <- make_cycle_data()
  without <- cycle_plot(data, item1, add_n_to_indep_label = FALSE)
  with_n <- cycle_plot(data, item1, add_n_to_indep_label = TRUE)

  testthat::expect_equal(
    y_axis_labels(without),
    c("2026", "2025", "2024", "2023")
  )
  labels <- y_axis_labels(with_n)
  testthat::expect_equal(strip_suffix(labels), y_axis_labels(without))
  # The suffix is the number of respondents in each cycle
  n_per_cycle <- as.integer(table(data$cycle)[strip_suffix(labels)])
  testthat::expect_equal(
    labels,
    paste0(strip_suffix(labels), " (N = ", n_per_cycle, ")")
  )
})

testthat::test_that("the same holds with several dependent variables", {
  data <- make_cycle_data()
  with_n <- cycle_plot(data, c(item1, item2), add_n_to_indep_label = TRUE)

  testthat::expect_equal(
    strip_suffix(y_axis_labels(with_n)),
    c("2026", "2025", "2024", "2023")
  )
  # The items are the facets, so an item label appears once, in the strip
  testthat::expect_setequal(
    as.character(unique(with_n$data[[".variable_label"]])),
    c("Item 1", "Item 2")
  )
})

testthat::test_that("fig_height_h_barchart2() no longer takes the companion for a second indep", {
  data <- make_cycle_data()
  with_n <- cycle_plot(data, c(item1, item2), add_n_to_indep_label = TRUE)

  testthat::expect_no_error(height <- fig_height_h_barchart2(with_n))
  testthat::expect_true(is.numeric(height) && length(height) == 1)
})

testthat::test_that("cat_plot_docx keeps the indep on the axis as well", {
  data <- make_cycle_data()
  m <- cycle_plot(
    data,
    item1,
    add_n_to_indep_label = TRUE,
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )
  testthat::expect_equal(m$x, "cycle")
  testthat::expect_no_error(fig_height_h_barchart2(m))
})
