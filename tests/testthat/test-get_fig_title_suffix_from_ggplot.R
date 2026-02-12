test_that("get_fig_title_suffix_from_ggplot returns N range for valid plot", {
  # Create a test plot
  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  result <- saros::get_fig_title_suffix_from_ggplot(plot)

  # Should return AsIs object
  expect_s3_class(result, "AsIs")
  expect_type(result, "character")

  # Should contain "N = " prefix
  expect_true(grepl("^N = ", result))

  # Should contain a number
  expect_true(grepl("\\d+", result))
})

test_that("get_fig_title_suffix_from_ggplot handles custom n_equals_string", {
  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  result <- saros::get_fig_title_suffix_from_ggplot(
    plot,
    n_equals_string = "Sample size: "
  )

  expect_s3_class(result, "AsIs")
  expect_true(grepl("^Sample size: ", result))
})

test_that("get_fig_title_suffix_from_ggplot returns empty string for invalid plot", {
  # Non-ggplot object
  result1 <- saros::get_fig_title_suffix_from_ggplot("not a plot")
  expect_equal(result1, I(""))

  # NULL
  result2 <- saros::get_fig_title_suffix_from_ggplot(NULL)
  expect_equal(result2, I(""))
})

test_that("get_fig_title_suffix_from_ggplot returns empty for plot with no data", {
  # Create empty ggplot
  empty_plot <- ggplot2::ggplot()

  result <- saros::get_fig_title_suffix_from_ggplot(empty_plot)
  expect_equal(result, I(""))
})

test_that("get_fig_title_suffix_from_ggplot handles plot with zero rows", {
  # Create plot with zero-row data frame
  library(ggplot2)
  zero_row_plot <- ggplot(
    data.frame(x = numeric(0), y = numeric(0)),
    aes(x, y)
  ) +
    geom_point()

  result <- saros::get_fig_title_suffix_from_ggplot(zero_row_plot)
  expect_equal(result, I(""))
})

test_that("get_fig_title_suffix_from_ggplot handles plots with different N ranges", {
  # Create plot with independent variable (will have range)
  plot_with_indep <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex
  )

  result <- saros::get_fig_title_suffix_from_ggplot(plot_with_indep)

  expect_s3_class(result, "AsIs")
  expect_type(result, "character")
  expect_true(grepl("^N = ", result))
})

test_that("get_fig_title_suffix_from_ggplot with save = TRUE creates links", {
  skip_on_cran()
  skip_if_not_installed("withr")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  # Use temporary directory for file output
  withr::with_tempdir({
    result <- saros::get_fig_title_suffix_from_ggplot(plot, save = TRUE)

    # Should return AsIs object
    expect_s3_class(result, "AsIs")
    expect_type(result, "character")

    # Should contain N range
    expect_true(grepl("N = ", result))

    # Should contain download links (markdown format)
    expect_true(grepl("\\[", result))
    expect_true(grepl("\\]\\(", result))

    # Should contain separator
    expect_true(grepl(", ", result))

    # Should have PNG link
    expect_true(grepl("PNG", result))
  })
})

test_that("get_fig_title_suffix_from_ggplot handles custom separator", {
  skip_on_cran()
  skip_if_not_installed("withr")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  withr::with_tempdir({
    result <- saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      sep = " | "
    )

    expect_s3_class(result, "AsIs")
    expect_true(grepl(" \\| ", result))
  })
})

test_that("get_fig_title_suffix_from_ggplot handles custom link_prefix", {
  skip_on_cran()
  skip_if_not_installed("withr")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  withr::with_tempdir({
    result <- saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      file_suffixes = c(".csv", ".png"),
      link_prefixes = c("[CSV](", "[Download Image]("),
      save_fns = list(utils::write.csv, saros::ggsaver)
    )

    expect_s3_class(result, "AsIs")
    expect_true(grepl("Download Image", result))
  })
})

test_that("get_fig_title_suffix_from_ggplot handles custom file_suffix", {
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("svglite")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  withr::with_tempdir({
    result <- saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      file_suffixes = ".svg",
      link_prefixes = "[SVG](",
      save_fns = list(saros::ggsaver)
    )

    expect_s3_class(result, "AsIs")
    # The result should contain a link with .svg extension
    expect_true(grepl("\\.svg", result))
  })
})

test_that("get_fig_title_suffix_from_ggplot with save = FALSE doesn't create files", {
  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  result <- saros::get_fig_title_suffix_from_ggplot(plot, save = FALSE)

  # Should only contain N range, no links
  expect_s3_class(result, "AsIs")
  expect_false(grepl("\\[", result))
  expect_false(grepl("\\]\\(", result))
  expect_true(grepl("^N = \\d+", result))
})

test_that("get_fig_title_suffix_from_ggplot returns AsIs class to prevent escaping", {
  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  result <- saros::get_fig_title_suffix_from_ggplot(plot)

  # Check that result is wrapped in I() for AsIs class
  expect_s3_class(result, "AsIs")
  expect_identical(class(result), class(I("")))
})

test_that("get_fig_title_suffix_from_ggplot validates vector length matching", {
  skip_on_cran()
  skip_if_not_installed("withr")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  # Mismatched lengths should error
  expect_error(
    saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      file_suffixes = c(".csv", ".png", ".svg"),
      link_prefixes = c("[CSV](", "[PNG]("),
      save_fns = list(utils::write.csv, saros::ggsaver)
    ),
    "must have equal lengths"
  )

  expect_error(
    saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      file_suffixes = c(".csv", ".png"),
      link_prefixes = c("[CSV](", "[PNG]("),
      save_fns = list(utils::write.csv)
    ),
    "must have equal lengths"
  )
})

test_that("get_fig_title_suffix_from_ggplot respects folder argument", {
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("fs")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  withr::with_tempdir({
    test_folder <- file.path(getwd(), "custom_folder")
    dir.create(test_folder, showWarnings = FALSE)

    result <- saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      folder = test_folder
    )

    expect_s3_class(result, "AsIs")
    # Result should contain the custom folder path
    expect_true(grepl("custom_folder", result))
    # Files should exist in custom folder
    expect_true(length(list.files(test_folder)) > 0)
  })
})

test_that("get_fig_title_suffix_from_ggplot respects file_prefix argument", {
  skip_on_cran()
  skip_if_not_installed("withr")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  withr::with_tempdir({
    result <- saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      file_prefix = "test_prefix_"
    )

    expect_s3_class(result, "AsIs")
    # Result should contain file prefix
    expect_true(grepl("test_prefix_", result))
    # Files with prefix should exist
    files <- list.files(pattern = "^test_prefix_")
    expect_true(length(files) > 0)
  })
})

test_that("get_fig_title_suffix_from_ggplot inherits from global settings", {
  skip_on_cran()
  skip_if_not_installed("withr")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  withr::with_tempdir({
    test_folder <- file.path(getwd(), "global_test")
    dir.create(test_folder, showWarnings = FALSE)

    # Set global options
    old_settings <- saros::global_settings_set(
      fn_name = "get_fig_title_suffix_from_ggplot",
      new = list(folder = test_folder, file_prefix = "global_"),
      quiet = TRUE
    )

    # Call function without explicit folder/prefix
    result <- saros::get_fig_title_suffix_from_ggplot(plot, save = TRUE)

    expect_s3_class(result, "AsIs")
    expect_true(grepl("global_test", result))
    expect_true(grepl("global_", result))

    # Files should exist in global folder with global prefix
    files <- list.files(test_folder, pattern = "^global_")
    expect_true(length(files) > 0)

    # Reset global settings
    saros::global_settings_reset(
      fn_name = "get_fig_title_suffix_from_ggplot",
      quiet = TRUE
    )
  })
})

test_that("get_fig_title_suffix_from_ggplot direct args override global settings", {
  skip_on_cran()
  skip_if_not_installed("withr")

  plot <- saros::makeme(data = saros::ex_survey, dep = b_1:b_3)

  withr::with_tempdir({
    global_folder <- file.path(getwd(), "global_folder")
    direct_folder <- file.path(getwd(), "direct_folder")
    dir.create(global_folder, showWarnings = FALSE)
    dir.create(direct_folder, showWarnings = FALSE)

    # Set global options
    saros::global_settings_set(
      fn_name = "get_fig_title_suffix_from_ggplot",
      new = list(folder = global_folder, file_prefix = "global_"),
      quiet = TRUE
    )

    # Override with direct argument
    result <- saros::get_fig_title_suffix_from_ggplot(
      plot,
      save = TRUE,
      folder = direct_folder,
      file_prefix = "direct_"
    )

    expect_s3_class(result, "AsIs")
    # Should use direct args, not global
    expect_true(grepl("direct_folder", result))
    expect_true(grepl("direct_", result))
    expect_false(grepl("global_folder", result))
    expect_false(grepl("global_", result))

    # Files should be in direct folder, not global
    direct_files <- list.files(direct_folder, pattern = "^direct_")
    global_files <- list.files(global_folder, pattern = "^global_")
    expect_true(length(direct_files) > 0)
    expect_equal(length(global_files), 0)

    # Reset
    saros::global_settings_reset(
      fn_name = "get_fig_title_suffix_from_ggplot",
      quiet = TRUE
    )
  })
})
