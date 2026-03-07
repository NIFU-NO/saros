testthat::test_that("crowd_plots_as_docx validates inputs", {
  library(saros)
  suppressMessages(library(dplyr))

  # Generate a valid plot for testing
  valid_plots <- makeme(
    data = ex_survey,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )

  # Test: path must be string
  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = 123),
    "must be a single character string"
  )

  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = c("a.docx", "b.docx")),
    "must be a single character string"
  )

  # Test: heading_style must be string
  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = "test.docx", heading_style = 123),
    "must be a single character string"
  )

  # Test: prefix_style must be string
  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = "test.docx", prefix_style = TRUE),
    "must be a single character string"
  )

  # Test: add_dep_label_prefix must be boolean
  testthat::expect_error(
    crowd_plots_as_docx(
      valid_plots,
      path = "test.docx",
      add_dep_label_prefix = "yes"
    ),
    "must be TRUE or FALSE"
  )

  # Test: extract_metadata must be boolean
  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = "test.docx", extract_metadata = 1),
    "must be TRUE or FALSE"
  )

  # Test: chart_width must be NULL or single numeric
  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = "test.docx", chart_width = "wide"),
    "must be NULL or a single numeric value"
  )

  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = "test.docx", chart_width = c(5, 6)),
    "must be NULL or a single numeric value"
  )

  # Test: chart_height must be NULL or single numeric
  testthat::expect_error(
    crowd_plots_as_docx(valid_plots, path = "test.docx", chart_height = "tall"),
    "must be NULL or a single numeric value"
  )

  testthat::expect_error(
    crowd_plots_as_docx(
      valid_plots,
      path = "test.docx",
      chart_height = c(4, 5)
    ),
    "must be NULL or a single numeric value"
  )
})

testthat::test_that("crowd_plots_as_docx works with plain list", {
  library(saros)
  suppressMessages(library(dplyr))

  # Generate plots
  plots <- makeme(
    data = ex_survey,
    dep = b_1:b_3,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )

  # Write to temp file
  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "test_output.docx")

  result <- crowd_plots_as_docx(plots, path = output_path)

  # Check file was created
  testthat::expect_true(file.exists(output_path))
  testthat::expect_identical(result, output_path)

  # Check file size (should be non-trivial)
  file_info <- file.info(output_path)
  testthat::expect_gt(file_info$size, 1000) # At least 1KB
})

testthat::test_that("crowd_plots_as_docx works with saros_officer_plots object", {
  library(saros)
  suppressMessages(library(dplyr))

  # Generate plots and convert to officer format
  plots <- makeme(
    data = ex_survey,
    dep = b_1:b_2,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )

  officer_plots <- crowd_plots_as_officer(plots)

  # Write to temp file
  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "test_officer_output.docx")

  result <- crowd_plots_as_docx(officer_plots, path = output_path)

  # Check file was created
  testthat::expect_true(file.exists(output_path))
  testthat::expect_identical(result, output_path)
})

testthat::test_that("crowd_plots_as_docx respects add_dep_label_prefix option", {
  library(saros)
  suppressMessages(library(dplyr))

  # Generate plots
  plots <- makeme(
    data = ex_survey,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )

  # Write with and without prefix
  temp_dir <- withr::local_tempdir()

  path_with_prefix <- file.path(temp_dir, "with_prefix.docx")
  path_without_prefix <- file.path(temp_dir, "without_prefix.docx")

  crowd_plots_as_docx(
    plots,
    path = path_with_prefix,
    add_dep_label_prefix = TRUE
  )
  crowd_plots_as_docx(
    plots,
    path = path_without_prefix,
    add_dep_label_prefix = FALSE
  )

  # Both files should exist
  testthat::expect_true(file.exists(path_with_prefix))
  testthat::expect_true(file.exists(path_without_prefix))

  # File with prefix should be slightly larger (has extra text)
  size_with <- file.info(path_with_prefix)$size
  size_without <- file.info(path_without_prefix)$size
  testthat::expect_gte(size_with, size_without)
})

testthat::test_that("crowd_plots_as_docx handles empty plot list", {
  library(saros)

  # Create empty list
  empty_list <- list()

  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "empty_output.docx")

  # Should warn but still create file
  testthat::expect_warning(
    result <- crowd_plots_as_docx(empty_list, path = output_path),
    "No valid plots"
  )

  # File should exist (empty document)
  testthat::expect_true(file.exists(output_path))
  testthat::expect_identical(result, output_path)
})

testthat::test_that("crowd_plots_as_docx respects custom chart dimensions", {
  library(saros)
  suppressMessages(library(dplyr))

  # Generate plot
  plots <- makeme(
    data = ex_survey,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )

  # Write with custom dimensions
  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "custom_dims.docx")

  result <- crowd_plots_as_docx(
    plots,
    path = output_path,
    chart_width = 5,
    chart_height = 3.5
  )

  # File should be created
  testthat::expect_true(file.exists(output_path))
  testthat::expect_identical(result, output_path)
})
