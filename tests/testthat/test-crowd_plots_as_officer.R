testthat::test_that("crowd_plots_as_officer validates inputs", {
  library(saros)

  # Test: non-list input
  testthat::expect_error(
    crowd_plots_as_officer("not a list"),
    "must be a list"
  )

  # Test: non-boolean extract_metadata
  testthat::expect_error(
    crowd_plots_as_officer(list(a = 1), extract_metadata = "yes"),
    "must be a single logical value"
  )

  # Test: unnamed list should error
  testthat::expect_error(
    crowd_plots_as_officer(list(1, 2, 3)),
    "must be a named list"
  )

  # Test: list with NA names should error
  list_with_na_name <- list("a" = 1, "b" = 2)
  names(list_with_na_name)[2] <- NA_character_
  testthat::expect_error(
    crowd_plots_as_officer(list_with_na_name),
    "missing or empty names"
  )

  # Test: list with empty string names should error
  list_with_empty_name <- list("a" = 1, "b" = 2)
  names(list_with_empty_name)[2] <- ""
  testthat::expect_error(
    crowd_plots_as_officer(list_with_empty_name),
    "missing or empty names"
  )

  # Test: empty list with extract_metadata = TRUE
  testthat::expect_warning(
    result <- crowd_plots_as_officer(list(), extract_metadata = TRUE),
    "is empty"
  )
  testthat::expect_s3_class(result, "saros_officer_plots")
  testthat::expect_equal(result$n_plots, 0L)
  testthat::expect_length(result$metadata, 0)

  # Test: empty list with extract_metadata = FALSE
  testthat::expect_warning(
    result <- crowd_plots_as_officer(list(), extract_metadata = FALSE),
    "is empty"
  )
  testthat::expect_s3_class(result, "saros_officer_plots")
  testthat::expect_equal(result$n_plots, 0L)
  testthat::expect_null(result$metadata)
})

testthat::test_that("crowd_plots_as_officer handles mschart objects", {
  library(saros)
  suppressMessages(library(dplyr))

  # Create valid mschart objects with docx_return_object = TRUE
  chart1 <- makeme(
    data = ex_survey,
    type = "cat_plot_docx",
    dep = b_1,
    docx_return_object = TRUE
  )

  chart2 <- makeme(
    data = ex_survey,
    type = "cat_plot_docx",
    dep = b_2,
    docx_return_object = TRUE
  )

  plot_list <- list(
    "Chart 1" = chart1,
    "Chart 2" = chart2
  )

  result <- crowd_plots_as_officer(plot_list)

  # Test structure
  testthat::expect_s3_class(result, "saros_officer_plots")
  testthat::expect_equal(result$n_plots, 2L)
  testthat::expect_length(result$plots, 2)
  testthat::expect_named(result$plots, c("Chart 1", "Chart 2"))

  # Test metadata extraction
  testthat::expect_length(result$metadata, 2)
  testthat::expect_equal(result$metadata[["Chart 1"]]$name, "Chart 1")
  testthat::expect_true(nzchar(result$metadata[["Chart 1"]]$dep_label_prefix))
})

testthat::test_that("crowd_plots_as_officer skips NULL and invalid objects", {
  library(saros)

  # Create one valid mschart
  chart1 <- makeme(
    data = ex_survey,
    type = "cat_plot_docx",
    dep = b_1,
    docx_return_object = TRUE
  )

  plot_list <- list(
    "Valid" = chart1,
    "NULL plot" = NULL,
    "Invalid plot" = ggplot2::ggplot(),
    "Another invalid" = data.frame(x = 1)
  )

  # Should warn about invalid objects
  testthat::expect_warning(
    result <- crowd_plots_as_officer(plot_list),
    "not a valid mschart object"
  )

  # Only valid plot should remain
  testthat::expect_equal(result$n_plots, 1L)
  testthat::expect_named(result$plots, "Valid")
})

testthat::test_that("crowd_plots_as_officer works without metadata extraction", {
  library(saros)

  chart1 <- makeme(
    data = ex_survey,
    type = "cat_plot_docx",
    dep = b_1,
    docx_return_object = TRUE
  )

  result <- crowd_plots_as_officer(
    list("Chart" = chart1),
    extract_metadata = FALSE
  )

  testthat::expect_s3_class(result, "saros_officer_plots")
  testthat::expect_null(result$metadata)
  testthat::expect_equal(result$n_plots, 1L)
})

testthat::test_that("crowd_plots_as_officer preserves dep_label_prefix", {
  library(saros)

  chart <- makeme(
    data = ex_survey,
    type = "cat_plot_docx",
    dep = b_1:b_3,
    docx_return_object = TRUE
  )

  result <- crowd_plots_as_officer(list("Test" = chart))

  # Check that dep_label_prefix is extracted
  prefix <- result$metadata[["Test"]]$dep_label_prefix
  testthat::expect_true(nzchar(prefix))

  # Verify it matches what get_dep_label_prefix returns
  testthat::expect_equal(
    prefix,
    get_dep_label_prefix(chart)
  )
})

testthat::test_that("crowd_plots_as_officer works with crowd parameter", {
  library(saros)
  suppressMessages(library(dplyr))

  # Create plots with crowd parameter
  plots <- makeme(
    data = ex_survey,
    dep = b_1:b_3,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx",
    docx_return_object = TRUE
  )

  result <- crowd_plots_as_officer(plots)

  testthat::expect_s3_class(result, "saros_officer_plots")
  testthat::expect_true(result$n_plots >= 1)
  testthat::expect_true(all(vapply(
    result$plots,
    inherits,
    logical(1),
    "ms_chart"
  )))
})

testthat::test_that("print.saros_officer_plots works", {
  library(saros)

  chart <- makeme(
    data = ex_survey,
    type = "cat_plot_docx",
    dep = b_1,
    docx_return_object = TRUE
  )

  result <- crowd_plots_as_officer(list("Test" = chart))

  # Test that print doesn't error and returns invisibly
  testthat::expect_invisible(print(result))
  testthat::expect_no_error(print(result))

  # Verify the structure contains expected data
  testthat::expect_equal(result$n_plots, 1L)
  testthat::expect_named(result$plots, "Test")
})

testthat::test_that("crowd_plots_as_officer handles all NULL plots", {
  library(saros)

  plot_list <- list(
    "A" = NULL,
    "B" = NULL,
    "C" = NULL
  )

  testthat::expect_warning(
    result <- crowd_plots_as_officer(plot_list),
    "No valid mschart objects"
  )

  testthat::expect_equal(result$n_plots, 0L)
  testthat::expect_length(result$plots, 0)
})
