testthat::test_that("txt_from_cat_mesos_plots validates plots argument is a list", {
  # Create a single ggplot object (not a list)
  single_plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3)) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y))

  # Should return fallback_string with warning
  testthat::expect_warning(
    result <- saros::txt_from_cat_mesos_plots(single_plot),
    "must be a list"
  )
  testthat::expect_equal(result, character())

  # Test with custom fallback_string
  testthat::expect_warning(
    result2 <- saros::txt_from_cat_mesos_plots(
      single_plot,
      fallback_string = "Invalid input"
    ),
    "must be a list"
  )
  testthat::expect_equal(result2, "Invalid input")
})

testthat::test_that("txt_from_cat_mesos_plots validates plots has at least 2 elements", {
  # Create plot data
  plot_data <- data.frame(
    .variable_label = "Test",
    .category = factor(c("A", "B")),
    .category_order = 1:2,
    .proportion = c(0.5, 0.5)
  )

  # List with only 1 element
  single_element_list <- list(list(data = plot_data))

  testthat::expect_warning(
    result <- saros::txt_from_cat_mesos_plots(single_element_list),
    "must contain at least 2 elements"
  )
  testthat::expect_equal(result, character())
})

testthat::test_that("txt_from_cat_mesos_plots validates plot elements have data", {
  # Create a list with elements that don't have data
  invalid_plots <- list(
    list(x = 1),
    list(y = 2)
  )

  testthat::expect_warning(
    result <- saros::txt_from_cat_mesos_plots(invalid_plots),
    "do not contain plot data"
  )
  testthat::expect_equal(result, character())

  # Mixed case: one valid, one invalid
  plot_data <- data.frame(
    .variable_label = "Test",
    .category = factor(c("A", "B")),
    .category_order = 1:2,
    .proportion = c(0.5, 0.5)
  )

  mixed_plots <- list(
    list(data = plot_data),
    list(no_data = "invalid")
  )

  testthat::expect_warning(
    result2 <- saros::txt_from_cat_mesos_plots(mixed_plots),
    "do not contain plot data"
  )
  testthat::expect_equal(result2, character())
})

testthat::test_that("txt_from_cat_mesos_plots works with valid data frames directly", {
  # Create plot data as data frames
  plot_data_1 <- data.frame(
    .variable_label = "Test",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.3, 0.7)
  )

  plot_data_2 <- data.frame(
    .variable_label = "Test",
    .category = factor(c("Low", "High"), levels = c("Low", "High")),
    .category_order = 1:2,
    .proportion = c(0.6, 0.4)
  )

  # Pass data frames directly
  plots <- list(plot_data_1, plot_data_2)

  # Should work without warnings
  testthat::expect_no_warning(
    result <- saros::txt_from_cat_mesos_plots(plots, min_prop_diff = 0.2)
  )
  testthat::expect_type(result, "character")
})
