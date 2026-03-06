testthat::test_that("fig_height_h_barchart works with typical input", {
  result <- saros::fig_height_h_barchart(
    n_y = 5,
    n_cats_y = 3,
    max_chars_cats_y = 10,
    n_x = 1,
    n_cats_x = 4,
    max_chars_cats_x = 12,
    freq = FALSE,
    x_axis_label_width = 20,
    strip_angle = 0,
    main_font_size = 8,
    legend_location = "panel",
    legend_key_chars_equivalence = 5,
    multiplier_per_horizontal_line = 1,
    multiplier_per_vertical_letter = .15,
    multiplier_per_facet = .95,
    multiplier_per_legend_line = 1.5,
    fixed_constant = 0,
    figure_width_in_cm = 16,
    margin_in_cm = 0,
    max = 8,
    min = 1
  )
  testthat::expect_equal(result, 6.19)
})

testthat::test_that("fig_height_h_barchart handles frequency plot", {
  result <- saros::fig_height_h_barchart(
    n_y = 4,
    n_cats_y = 4,
    max_chars_cats_y = 20,
    max_chars_labels_y = 30,
    n_x = 1,
    n_cats_x = 3,
    max_chars_cats_x = 12,
    freq = TRUE,
    x_axis_label_width = 20,
    strip_angle = 0,
    fixed_constant = 0,
    figure_width_in_cm = 16,
    margin_in_cm = 0,
    max = 16,
    min = 1
  )
  testthat::expect_equal(result, 8.36)
})

testthat::test_that("fig_height_h_barchart handles missing n_x and n_cats_x", {
  result <- saros::fig_height_h_barchart(
    n_y = 6,
    n_cats_y = 3,
    max_chars_cats_y = 35,
    max_chars_labels_y = 25,
    x_axis_label_width = 20,
    strip_angle = 0,
    main_font_size = 7,
    legend_location = "panel",
    fixed_constant = 0,
    figure_width_in_cm = 16,
    margin_in_cm = 0,
    max = 8,
    min = 1
  )
  testthat::expect_equal(result, 3.69)
})

testthat::test_that("fig_height_h_barchart handles NULL multiplier_per_horizontal_line", {
  result <- saros::fig_height_h_barchart(
    n_y = 5,
    n_cats_y = 3,
    max_chars_cats_y = 10,
    n_x = 1,
    n_cats_x = 4,
    max_chars_cats_x = 12,
    freq = FALSE,
    x_axis_label_width = 20,
    strip_angle = 0,
    main_font_size = 8,
    legend_location = "panel",
    n_legend_lines = 2,
    legend_key_chars_equivalence = 5,
    multiplier_per_horizontal_line = 1,
    multiplier_per_vertical_letter = .15,
    multiplier_per_facet = .95,
    multiplier_per_legend_line = 1.5,
    fixed_constant = 0,
    figure_width_in_cm = 16,
    margin_in_cm = 0,
    max = 8,
    min = 1
  )
  testthat::expect_equal(result, 5.9)
})

testthat::test_that("fig_height_h_barchart respects min and max height", {
  result <- saros::fig_height_h_barchart(
    n_y = 5,
    n_cats_y = 3,
    max_chars_cats_y = 10,
    n_x = 1,
    n_cats_x = 4,
    max_chars_cats_x = 12,
    freq = FALSE,
    x_axis_label_width = 20,
    strip_angle = 0,
    main_font_size = 8,
    legend_location = "panel",
    n_legend_lines = 2,
    legend_key_chars_equivalence = 5,
    multiplier_per_horizontal_line = 1,
    multiplier_per_vertical_letter = .15,
    multiplier_per_facet = .95,
    multiplier_per_legend_line = 1.5,
    fixed_constant = 1,
    figure_width_in_cm = 16,
    margin_in_cm = 0,
    max = 6,
    min = 2
  )
  testthat::expect_equal(result, 6)
})

testthat::test_that("fig_height_h_barchart2 scales height for int_plot_html with indep", {
  plot_without <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_plot_html"
  )
  plot_with <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    indep = b_1,
    type = "int_plot_html"
  )
  result_without <- saros::fig_height_h_barchart2(plot_without)
  result_with <- saros::fig_height_h_barchart2(plot_with)
  testthat::expect_type(result_with, "double")
  testthat::expect_gte(result_with, 1)
  testthat::expect_lte(result_with, 12)
  # Height with indep should be >= height without (more groups = more space needed)
  testthat::expect_gte(result_with, result_without)
})

testthat::test_that("fig_height_h_barchart2 scales height for int_plot_html without indep", {
  plot <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_plot_html"
  )
  result <- saros::fig_height_h_barchart2(plot)
  testthat::expect_type(result, "double")
  testthat::expect_gte(result, 1)
  testthat::expect_lt(result, 12)
})

testthat::test_that("fig_height_h_barchart2 works for cat_plot_html", {
  plot <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_2,
    indep = x1_sex,
    type = "cat_plot_html"
  )
  result <- saros::fig_height_h_barchart2(plot)
  # Should return a calculated height, not default
  testthat::expect_type(result, "double")
  testthat::expect_true(result > 0)
})

testthat::test_that("fig_height_h_barchart2 respects hide_axis_text_if_single_variable", {
  # When hide_axis_text_if_single_variable = TRUE, height should be smaller
  plot_hidden <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1,
    label_separator = NULL,
    hide_axis_text_if_single_variable = TRUE,
    type = "cat_plot_html"
  )

  plot_shown <- saros::makeme(
    data = saros::ex_survey,
    dep = p_1,
    label_separator = NULL,
    hide_axis_text_if_single_variable = FALSE,
    type = "cat_plot_html"
  )

  height_hidden <- saros::fig_height_h_barchart2(plot_hidden)
  height_shown <- saros::fig_height_h_barchart2(plot_shown)

  # Height with hidden axis text should be smaller than shown axis text
  testthat::expect_true(
    height_hidden < height_shown,
    info = sprintf(
      "Hidden height (%s) should be < shown height (%s)",
      height_hidden,
      height_shown
    )
  )
})

testthat::test_that("fig_height_h_barchart2 uses theme font size", {
  old_theme <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(old_theme))

  plot <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "cat_plot_html"
  )

  ggplot2::theme_set(ggplot2::theme_gray(base_size = 7))
  height_small <- saros::fig_height_h_barchart2(
    plot + ggplot2::theme(text = ggplot2::element_text(size = 7)),
    max = 20,
    min = 1
  )

  ggplot2::theme_set(ggplot2::theme_gray(base_size = 16))
  height_large <- saros::fig_height_h_barchart2(
    plot + ggplot2::theme(text = ggplot2::element_text(size = 16)),
    max = 20,
    min = 1
  )

  # Larger font should yield a taller figure
  testthat::expect_gt(height_large, height_small)
})

testthat::test_that("fig_height_h_barchart2 skips legend height when legend is at sides", {
  old_theme <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(old_theme))

  plot <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "cat_plot_html"
  )

  ggplot2::theme_set(ggplot2::theme_gray() +
    ggplot2::theme(legend.position = "bottom"))
  height_bottom <- saros::fig_height_h_barchart2(plot +
    ggplot2::theme(legend.position = "bottom"))

  ggplot2::theme_set(ggplot2::theme_gray() +
    ggplot2::theme(legend.position = "right"))
  height_right <- saros::fig_height_h_barchart2(plot +
    ggplot2::theme(legend.position = "right"))

  # Legend at bottom adds vertical space; legend at right does not
  testthat::expect_gte(height_bottom, height_right)
})

testthat::test_that("extract_ggplot_theme_info returns correct structure", {
  plot <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_2,
    type = "cat_plot_html"
  )
  info <- saros:::extract_ggplot_theme_info(plot)
  testthat::expect_type(info, "list")
  testthat::expect_named(info, c("base_size", "legend_adds_height"))
  testthat::expect_type(info$base_size, "double")
  testthat::expect_type(info$legend_adds_height, "logical")
})

testthat::test_that("count_max_wrapped_lines counts correctly", {
  # Short labels — should all fit in one line at width 20
  testthat::expect_equal(
    saros:::count_max_wrapped_lines(c("Short", "Also short"), width = 20),
    1L
  )
  # Long label that wraps at width 10
  testthat::expect_equal(
    saros:::count_max_wrapped_lines("This is a fairly long label text", width = 10),
    4L
  )
  # Edge case: empty vector
  testthat::expect_equal(
    saros:::count_max_wrapped_lines(character(0), width = 20),
    1L
  )
})

testthat::test_that("fig_height_h_barchart handles negative strip_angle via abs()", {
  # strip_angle = 90 and strip_angle = -90 should give the same result
  result_pos <- saros::fig_height_h_barchart(
    n_y = 3,
    n_cats_y = 2,
    max_chars_labels_y = 15,
    max_chars_cats_y = 10,
    n_x = 1,
    n_cats_x = 2,
    max_chars_cats_x = 6,
    strip_angle = 90,
    x_axis_label_width = 20,
    strip_width = 20,
    main_font_size = 8,
    multiplier_per_vertical_letter = 0.15,
    figure_width_in_cm = 14,
    max = 20,
    min = 1
  )
  result_neg <- saros::fig_height_h_barchart(
    n_y = 3,
    n_cats_y = 2,
    max_chars_labels_y = 15,
    max_chars_cats_y = 10,
    n_x = 1,
    n_cats_x = 2,
    max_chars_cats_x = 6,
    strip_angle = -90,
    x_axis_label_width = 20,
    strip_width = 20,
    main_font_size = 8,
    multiplier_per_vertical_letter = 0.15,
    figure_width_in_cm = 14,
    max = 20,
    min = 1
  )
  testthat::expect_equal(result_pos, result_neg)
})
