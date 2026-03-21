testthat::test_that("sort_dep_by = '.range' orders by proportion range descending", {
  # Create synthetic data with known distributions:
  # var_wide: proportions spread 0.1 to 0.9 -> range 0.8
  # var_narrow: proportions spread 0.4 to 0.6 -> range 0.2
  test_data <- data.frame(
    var_wide = factor(c(rep("A", 9), rep("B", 1)), levels = c("A", "B")),
    var_narrow = factor(c(rep("A", 6), rep("B", 4)), levels = c("A", "B"))
  )
  labelled::var_label(test_data$var_wide) <- "Q1 - Wide spread"
  labelled::var_label(test_data$var_narrow) <- "Q1 - Narrow spread"

  result <- saros::makeme(
    data = test_data,
    dep = var_wide:var_narrow,
    type = "cat_table_html",
    sort_dep_by = ".range",
    descend = TRUE,
    label_separator = " - "
  )
  labels <- as.character(result[[1]])
  # With descend = TRUE, widest range first
  testthat::expect_equal(labels, c("Wide spread", "Narrow spread"))
})

testthat::test_that("sort_dep_by = '.range' with descend = FALSE reverses order", {
  test_data <- data.frame(
    var_wide = factor(c(rep("A", 9), rep("B", 1)), levels = c("A", "B")),
    var_narrow = factor(c(rep("A", 6), rep("B", 4)), levels = c("A", "B"))
  )
  labelled::var_label(test_data$var_wide) <- "Q1 - Wide spread"
  labelled::var_label(test_data$var_narrow) <- "Q1 - Narrow spread"

  result_desc <- saros::makeme(
    data = test_data, dep = var_wide:var_narrow,
    type = "cat_table_html", sort_dep_by = ".range",
    descend = TRUE, label_separator = " - "
  )
  result_asc <- saros::makeme(
    data = test_data, dep = var_wide:var_narrow,
    type = "cat_table_html", sort_dep_by = ".range",
    descend = FALSE, label_separator = " - "
  )
  labels_desc <- as.character(result_desc[[1]])
  labels_asc <- as.character(result_asc[[1]])
  testthat::expect_equal(labels_desc, rev(labels_asc))
})

testthat::test_that("sort_dep_by = '.range' errors when .proportion is missing", {
  testthat::expect_error(
    saros:::add_dep_order(
      data = data.frame(.variable_name = "a", .category = "A", .count = 1),
      sort_by = ".range",
      descend = TRUE
    ),
    regexp = "requires.*\\.proportion"
  )
})

testthat::test_that("sort_dep_by = '.range' handles all-NA proportions gracefully", {
  test_data <- data.frame(
    .variable_name = c("a", "a", "b", "b"),
    .category = factor(c("A", "B", "A", "B")),
    .proportion = c(NA, NA, 0.3, 0.7),
    .variable_position = c(1L, 1L, 2L, 2L)
  )
  result <- saros:::add_dep_order(test_data, sort_by = ".range", descend = TRUE)
  testthat::expect_true(".dep_order" %in% names(result))
  # Variable "b" has range 0.4, "a" has range 0 (all-NA treated as 0)
  # With descend = TRUE, "b" should be first (order 1)
  b_order <- unique(result$.dep_order[result$.variable_name == "b"])
  a_order <- unique(result$.dep_order[result$.variable_name == "a"])
  testthat::expect_true(b_order < a_order)
})
