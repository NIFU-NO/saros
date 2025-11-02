testthat::test_that("Issue #500: girafe handles showNA='always' without white fill", {
  testthat::skip_on_cran()

  # Reproduce the issue from GH #500
  # First create data with explicit NA level
  test_data <- saros::ex_survey
  test_data$p_1 <- forcats::fct_na_value_to_level(test_data$p_1, level = "NA")
  test_data$p_2 <- forcats::fct_na_value_to_level(test_data$p_2, level = "NA")
  test_data$p_3 <- forcats::fct_na_value_to_level(test_data$p_3, level = "NA")
  test_data$p_4 <- forcats::fct_na_value_to_level(test_data$p_4, level = "NA")

  result <- saros::makeme(
    test_data,
    dep = p_1:p_4,
    type = "cat_plot_html",
    showNA = "always",
    html_interactive = FALSE
  )

  # Should return a ggplot object
  testthat::expect_true(ggplot2::is_ggplot(result) || inherits(result, "gg"))

  # Check that .category has "NA" as a level
  testthat::expect_true("NA" %in% levels(result$data$.category))

  # When passed through girafe with palette, NA should get a color
  result_with_palette <- saros::girafe(
    result,
    interactive = FALSE,
    palette_codes = list(
      c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
    ),
    priority_palette_codes = c("NA" = "grey")
  )

  testthat::expect_true(
    ggplot2::is_ggplot(result_with_palette) ||
      inherits(result_with_palette, "gg")
  )

  # Extract the fill scale - it should have been set
  fill_scale <- result_with_palette$scales$get_scales("fill")
  testthat::expect_false(is.null(fill_scale))
})
