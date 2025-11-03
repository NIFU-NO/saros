test_that("txt_from_cat_mesos_plots returns text for meaningful differences", {
  # Create sample data with meaningful difference
  plot_data_1 <- data.frame(
    .variable_label = "Job satisfaction",
    .category = factor(
      c("Low", "Medium", "High"),
      levels = c("Low", "Medium", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.2, 0.3, 0.5)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Job satisfaction",
    .category = factor(
      c("Low", "Medium", "High"),
      levels = c("Low", "Medium", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.3, 0.4, 0.3)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result <- saros::txt_from_cat_mesos_plots(plots, min_prop_diff = 0.10)

  expect_type(result, "character")
  expect_true(length(result) >= 1)
  expect_true(any(grepl("Job satisfaction", result)))
})

test_that("txt_from_cat_mesos_plots returns empty for small differences", {
  # Create sample data with small difference (below threshold)
  plot_data_1 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.5, 0.5)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.52, 0.48)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result <- saros::txt_from_cat_mesos_plots(plots, min_prop_diff = 0.10)

  expect_type(result, "character")
  expect_equal(length(result), 0)
})

test_that("txt_from_cat_mesos_plots handles multiple variables", {
  # Create data with two variables
  plot_data_1 <- data.frame(
    .variable_label = rep(c("Var1", "Var2"), each = 2),
    .category = factor(rep(c("Low", "High"), 2), levels = c("Low", "High")),
    .category_order = rep(1:2, 2),
    .proportion = c(0.3, 0.7, 0.2, 0.8)
  )

  plot_data_2 <- data.frame(
    .variable_label = rep(c("Var1", "Var2"), each = 2),
    .category = factor(rep(c("Low", "High"), 2), levels = c("Low", "High")),
    .category_order = rep(1:2, 2),
    .proportion = c(0.5, 0.5, 0.6, 0.4)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result <- saros::txt_from_cat_mesos_plots(plots, min_prop_diff = 0.15)

  expect_type(result, "character")
  expect_true(length(result) >= 1)
  # Should have summaries for variables with meaningful differences
  expect_true(any(grepl("Var1|Var2", result)))
})

test_that("txt_from_cat_mesos_plots handles flip_to_lowest_categories", {
  # Create data where lowest category differs
  plot_data_1 <- data.frame(
    .variable_label = "Question",
    .category = factor(
      c("Low", "Medium", "High"),
      levels = c("Low", "Medium", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.5, 0.3, 0.2)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Question",
    .category = factor(
      c("Low", "Medium", "High"),
      levels = c("Low", "Medium", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.3, 0.4, 0.3)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result <- saros::txt_from_cat_mesos_plots(
    plots,
    flip_to_lowest_categories = TRUE,
    min_prop_diff = 0.10
  )

  expect_type(result, "character")
  expect_true(length(result) >= 1)
})

test_that("txt_from_cat_mesos_plots returns empty for NULL plots", {
  plots <- list(NULL, NULL)

  result <- testthat::expect_warning(
    saros::txt_from_cat_mesos_plots(plots),
    regexp = "elements 1 and 2 do not contain plot data"
  )

  expect_type(result, "character")
  expect_equal(length(result), 0)
})

test_that("txt_from_cat_mesos_plots respects n_highest_categories", {
  # Create data with 3 categories
  plot_data_1 <- data.frame(
    .variable_label = "Question",
    .category = factor(
      c("Low", "Med", "High"),
      levels = c("Low", "Med", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.2, 0.3, 0.5)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Question",
    .category = factor(
      c("Low", "Med", "High"),
      levels = c("Low", "Med", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.3, 0.3, 0.4)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  # Test with n_highest_categories = 2 (should combine Med + High)
  result <- saros::txt_from_cat_mesos_plots(
    plots,
    n_highest_categories = 2,
    min_prop_diff = 0.05
  )

  expect_type(result, "character")
  # Should mention multiple categories if combining them
  # The text will include both categories in selected_categories
})

test_that("txt_from_cat_mesos_plots uses custom templates", {
  plot_data_1 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.3, 0.7)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.4, 0.6)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  custom_template <- "CUSTOM: {var} shows {group_1} vs {group_2} for {selected_categories}"

  result <- saros::txt_from_cat_mesos_plots(
    plots,
    min_prop_diff = 0.05,
    glue_str_pos = custom_template,
    glue_str_neg = custom_template
  )

  expect_type(result, "character")
  if (length(result) > 0) {
    expect_true(any(grepl("CUSTOM:", result)))
  }
})

test_that("txt_from_cat_mesos_plots handles digits parameter", {
  plot_data_1 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.333, 0.667)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.444, 0.556)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result_2_digits <- saros::txt_from_cat_mesos_plots(
    plots,
    min_prop_diff = 0.05,
    digits = 2
  )
  result_1_digit <- saros::txt_from_cat_mesos_plots(
    plots,
    min_prop_diff = 0.05,
    digits = 1
  )

  expect_type(result_2_digits, "character")
  expect_type(result_1_digit, "character")
})

test_that("txt_from_cat_mesos_plots handles selected_categories_last_split", {
  plot_data_1 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    .category_order = 1:3,
    .proportion = c(0.2, 0.3, 0.5)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    .category_order = 1:3,
    .proportion = c(0.3, 0.3, 0.4)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result_or <- saros::txt_from_cat_mesos_plots(
    plots,
    n_highest_categories = 2,
    min_prop_diff = 0.05,
    selected_categories_last_split = " or "
  )

  result_and <- saros::txt_from_cat_mesos_plots(
    plots,
    n_highest_categories = 2,
    min_prop_diff = 0.05,
    selected_categories_last_split = " and "
  )

  expect_type(result_or, "character")
  expect_type(result_and, "character")
})
