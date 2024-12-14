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
