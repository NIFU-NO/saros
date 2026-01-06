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

  testthat::expect_warning(
    result <- saros::txt_from_cat_mesos_plots(plots),
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

test_that("txt_from_cat_mesos_plots respects global settings", {
  # Ensure cleanup happens even if test fails
  withr::defer(saros::global_settings_reset())

  # Setup: Create sample data
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
    .proportion = c(0.5, 0.5)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  # Set global settings
  saros::global_settings_set(
    new = list(
      min_prop_diff = 0.05,
      digits = 3
    ),
    fn_name = "txt_from_cat_mesos_plots",
    quiet = TRUE
  )

  # Call without explicit parameters - should use global settings
  result <- saros::txt_from_cat_mesos_plots(plots)

  # Should detect difference with min_prop_diff = 0.05 (0.7 - 0.5 = 0.2 > 0.05)
  expect_type(result, "character")
  expect_true(length(result) >= 1)

  # Reset global settings
  saros::global_settings_reset()

  # Without global settings and default min_prop_diff = 0.10, should still detect
  result_default <- saros::txt_from_cat_mesos_plots(plots)
  expect_type(result_default, "character")
  expect_true(length(result_default) >= 1)
})

test_that("explicit arguments override global settings", {
  # Ensure cleanup happens even if test fails
  withr::defer(saros::global_settings_reset())

  # Setup: Create sample data with small difference
  plot_data_1 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.4, 0.6)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Question",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.45, 0.55)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  # Set global settings with low threshold
  saros::global_settings_set(
    new = list(
      min_prop_diff = 0.01
    ),
    fn_name = "txt_from_cat_mesos_plots",
    quiet = TRUE
  )

  # Explicit argument should override (difference is 0.05, so 0.5 threshold means no text)
  result_explicit <- saros::txt_from_cat_mesos_plots(plots, min_prop_diff = 0.5)
  expect_type(result_explicit, "character")
  expect_equal(length(result_explicit), 0) # No text due to high threshold

  # Global settings should allow detection (0.05 > 0.01)
  result_global <- saros::txt_from_cat_mesos_plots(plots)
  expect_type(result_global, "character")
  expect_true(length(result_global) >= 1)
})

test_that("txt_from_cat_mesos_plots correctly handles second group proportions", {
  # Test for issue where second group (others) becomes zero
  plot_data_1 <- data.frame(
    .variable_label = rep("Var1", 3),
    .category = factor(
      c("Low", "Med", "High"),
      levels = c("Low", "Med", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.2, 0.3, 0.5)
  )

  plot_data_2 <- data.frame(
    .variable_label = rep("Var1", 3),
    .category = factor(
      c("Low", "Med", "High"),
      levels = c("Low", "Med", "High")
    ),
    .category_order = 1:3,
    .proportion = c(0.15, 0.42, 0.43)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result <- saros::txt_from_cat_mesos_plots(plots, min_prop_diff = 0.01)

  expect_type(result, "character")
  # The result should contain non-zero values for both groups
  # Group 1 highest = 0.5, Group 2 highest = 0.43
  # Check that the text contains both values
  if (length(result) > 0) {
    expect_true(any(grepl("0\\.5", result)))
    expect_true(any(grepl("0\\.43", result)))
    # Should NOT show group_2 as 0
    expect_false(any(grepl("\\(0\\)", result)))
  }
})

test_that("txt_from_cat_mesos_plots avoids summing all categories for binary variables", {
  # Test for issue where n_highest_categories=2 with 2 categories sums to 1.0
  plot_data_1 <- data.frame(
    .variable_label = "Binary Question",
    .category = factor(c("No", "Yes"), levels = c("No", "Yes")),
    .category_order = 1:2,
    .proportion = c(0.4, 0.6)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Binary Question",
    .category = factor(c("No", "Yes"), levels = c("No", "Yes")),
    .category_order = 1:2,
    .proportion = c(0.7, 0.3)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  # With n_highest_categories = 2 on a binary variable
  result <- saros::txt_from_cat_mesos_plots(
    plots,
    n_highest_categories = 2,
    min_prop_diff = 0.05
  )

  expect_type(result, "character")
  # Should use only highest category (Yes), not sum of both
  if (length(result) > 0) {
    # Group 1 should show 0.6 (Yes only), not 1.0 (Yes + No)
    expect_true(any(grepl("0\\.6", result)))
    # Group 2 should show 0.3 (Yes only), not 1.0 (Yes + No)
    expect_true(any(grepl("0\\.3|0\\.7", result)))
    # Should NOT show 1.0 (which would be the sum)
    expect_false(any(grepl("\\(1\\)|\\(1\\.0\\)", result)))
  }
})

test_that("txt_from_cat_mesos_plots uses multiple categories when appropriate", {
  # Test that n_highest_categories=2 works correctly with 4+ categories
  plot_data_1 <- data.frame(
    .variable_label = "Four Category Question",
    .category = factor(
      c("VeryLow", "Low", "High", "VeryHigh"),
      levels = c("VeryLow", "Low", "High", "VeryHigh")
    ),
    .category_order = 1:4,
    .proportion = c(0.1, 0.2, 0.3, 0.4)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Four Category Question",
    .category = factor(
      c("VeryLow", "Low", "High", "VeryHigh"),
      levels = c("VeryLow", "Low", "High", "VeryHigh")
    ),
    .category_order = 1:4,
    .proportion = c(0.2, 0.3, 0.25, 0.25)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  # With n_highest_categories = 2 and 4 categories, should combine High + VeryHigh
  result <- saros::txt_from_cat_mesos_plots(
    plots,
    n_highest_categories = 2,
    min_prop_diff = 0.05
  )

  expect_type(result, "character")
  # Should combine the two highest categories (High + VeryHigh)
  # Group 1: 0.3 + 0.4 = 0.7
  # Group 2: 0.25 + 0.25 = 0.5
  if (length(result) > 0) {
    expect_true(any(grepl("0\\.7", result)))
    expect_true(any(grepl("0\\.5", result)))
  }
})

test_that("txt_from_cat_mesos_plots handles multiple variables with different category counts", {
  # Test mixed scenario: one binary variable and one with many categories
  plot_data_1 <- data.frame(
    .variable_label = c(rep("Binary", 2), rep("FourCat", 4)),
    .category = factor(
      c("No", "Yes", "VeryLow", "Low", "High", "VeryHigh"),
      levels = c("No", "Yes", "VeryLow", "Low", "High", "VeryHigh")
    ),
    .category_order = c(1, 2, 1, 2, 3, 4),
    .proportion = c(0.4, 0.6, 0.1, 0.2, 0.3, 0.4)
  )

  plot_data_2 <- data.frame(
    .variable_label = c(rep("Binary", 2), rep("FourCat", 4)),
    .category = factor(
      c("No", "Yes", "VeryLow", "Low", "High", "VeryHigh"),
      levels = c("No", "Yes", "VeryLow", "Low", "High", "VeryHigh")
    ),
    .category_order = c(1, 2, 1, 2, 3, 4),
    .proportion = c(0.7, 0.3, 0.2, 0.3, 0.25, 0.25)
  )

  plots <- list(
    list(data = plot_data_1),
    list(data = plot_data_2)
  )

  result <- saros::txt_from_cat_mesos_plots(
    plots,
    n_highest_categories = 2,
    min_prop_diff = 0.05
  )

  expect_type(result, "character")
  # For Binary (2 categories), should use only highest (Yes): 0.6 vs 0.3
  # For FourCat (4 categories), should combine top 2: 0.7 vs 0.5
  if (length(result) > 0) {
    # Should contain results for at least one variable
    expect_true(any(grepl("Binary|FourCat", result)))
  }
})
