test_that("ggsaver saves ggplot objects", {
  skip_if_not_installed("ggplot2")

  # Create a simple plot
  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()

  # Create temporary file
  temp_file <- tempfile(fileext = ".png")

  # Test ggsaver function
  expect_no_error(ggsaver(plot, temp_file, width = 5, height = 4))

  # Check file was created
  expect_true(file.exists(temp_file))

  # Clean up
  if (file.exists(temp_file)) unlink(temp_file)
})

test_that("ggsaver handles different file formats", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()

  # Test PNG
  temp_png <- tempfile(fileext = ".png")
  expect_no_error(ggsaver(plot, temp_png, width = 5, height = 4))
  expect_true(file.exists(temp_png))

  # Test PDF
  temp_pdf <- tempfile(fileext = ".pdf")
  expect_no_error(ggsaver(plot, temp_pdf, width = 5, height = 4))
  expect_true(file.exists(temp_pdf))

  # Test SVG only if svglite is available
  skip_if_not_installed("svglite")
  temp_svg <- tempfile(fileext = ".svg")
  expect_no_error(ggsaver(plot, temp_svg, width = 5, height = 4))
  expect_true(file.exists(temp_svg))

  # Clean up
  unlink(c(temp_png, temp_pdf, temp_svg))
})

test_that("ggsaver creates directories", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()

  # Create path with non-existent directory
  temp_dir <- file.path(tempdir(), "test_subdir")
  temp_file <- file.path(temp_dir, "test_plot.png")

  # Ensure directory doesn't exist initially
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  expect_false(dir.exists(temp_dir))

  # Test ggsaver creates directory
  expect_no_error(ggsaver(plot, temp_file, width = 5, height = 4))
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("ggsaver forwards additional arguments", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()

  temp_file <- tempfile(fileext = ".png")

  # Test with additional arguments (avoid dpi since ggsaver sets it)
  expect_no_error(ggsaver(
    plot,
    temp_file,
    width = 10,
    height = 8,
    units = "cm"
  ))

  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("ggsaver suppresses messages", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()

  temp_file <- tempfile(fileext = ".png")

  # Should not produce any messages
  expect_silent(ggsaver(plot, temp_file, width = 5, height = 4))

  # Clean up
  unlink(temp_file)
})

test_that("ggsaver applies palette_codes from arguments", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
    geom_boxplot()

  temp_file <- tempfile(fileext = ".png")

  # Test with custom palette
  custom_palette <- list(c("red", "blue", "green"))
  expect_no_error(ggsaver(
    plot,
    temp_file,
    palette_codes = custom_palette,
    width = 5,
    height = 4
  ))

  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("ggsaver inherits palette_codes from girafe global settings", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Save current global settings
  old_settings <- global_settings_get("girafe")

  # Set up cleanup to restore exact previous settings
  withr::defer({
    global_settings_set(fn_name = "girafe", new = old_settings, quiet = TRUE)
  })

  # Set global palette
  custom_palette <- list(c("purple", "orange", "yellow"))
  global_settings_set(
    fn_name = "girafe",
    new = list(palette_codes = custom_palette),
    quiet = TRUE
  )

  plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
    geom_boxplot()

  temp_file <- tempfile(fileext = ".png")
  withr::defer(unlink(temp_file))

  # Test that global settings are applied
  expect_no_error(ggsaver(plot, temp_file, width = 5, height = 4))
  expect_true(file.exists(temp_file))
})

test_that("ggsaver handles plots without fill aesthetic", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()

  temp_file <- tempfile(fileext = ".png")

  # Should work even with palette_codes set
  custom_palette <- list(c("red", "blue", "green"))
  expect_no_error(ggsaver(
    plot,
    temp_file,
    palette_codes = custom_palette,
    width = 5,
    height = 4
  ))

  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("ggsaver handles factor() fill mappings correctly", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Create plot with factor() in the fill aesthetic (not a bare column name)
  plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
    geom_boxplot()

  temp_file <- tempfile(fileext = ".png")
  withr::defer(unlink(temp_file))

  # Custom palette
  custom_palette <- list(c("red", "blue", "green"))

  # Should successfully apply palette to factor() fill mapping
  expect_no_error(ggsaver(
    plot,
    temp_file,
    palette_codes = custom_palette,
    width = 5,
    height = 4
  ))

  expect_true(file.exists(temp_file))

  # Verify get_fill_levels extracts levels correctly from factor() mapping
  fill_levels <- saros:::get_fill_levels(plot)
  expect_equal(length(fill_levels), 3)
  expect_equal(sort(fill_levels), c("4", "6", "8"))
})
