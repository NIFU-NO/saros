testthat::test_that("is_rendering() detects knitr context correctly", {
  library(saros)

  # Outside knitr context (in test environment)
  testthat::expect_false(is_rendering())

  # Simulate knitr context
  withr::with_options(
    list(knitr.in.progress = TRUE),
    {
      testthat::expect_true(is_rendering())
    }
  )

  # Explicitly FALSE
  withr::with_options(
    list(knitr.in.progress = FALSE),
    {
      testthat::expect_false(is_rendering())
    }
  )

  # NULL option
  withr::with_options(
    list(knitr.in.progress = NULL),
    {
      testthat::expect_false(is_rendering())
    }
  )
})

testthat::test_that("crowd_output() routes to DOCX outside knitr", {
  library(saros)
  suppressMessages(library(dplyr))

  # Generate DOCX plots
  plots <- makeme(
    data = ex_survey,
    dep = b_1:b_2,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx"
  )

  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "test_output.docx")

  # Should create DOCX when not in knitr
  result <- crowd_output(plots, path = output_path)

  testthat::expect_true(file.exists(output_path))
  testthat::expect_identical(result, output_path)
})

testthat::test_that("crowd_output() with force_format='docx' creates file", {
  library(saros)
  suppressMessages(library(dplyr))

  plots <- makeme(
    data = ex_survey,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx"
  )

  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "forced_docx.docx")

  # Force DOCX even if we simulate knitr context
  withr::with_options(
    list(knitr.in.progress = TRUE),
    {
      result <- crowd_output(plots, path = output_path, force_format = "docx")
      testthat::expect_true(file.exists(output_path))
      testthat::expect_identical(result, output_path)
    }
  )
})

testthat::test_that("crowd_output() passes additional arguments through", {
  library(saros)
  suppressMessages(library(dplyr))

  plots <- makeme(
    data = ex_survey,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx"
  )

  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "custom_settings.docx")

  # Pass through custom chart dimensions
  result <- crowd_output(
    plots,
    path = output_path,
    chart_width = 5,
    chart_height = 3,
    add_dep_label_prefix = FALSE
  )

  testthat::expect_true(file.exists(output_path))
})

testthat::test_that("crowd_output() works with saros_officer_plots object", {
  library(saros)
  suppressMessages(library(dplyr))

  plots <- makeme(
    data = ex_survey,
    dep = b_1:b_2,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx"
  )

  officer_plots <- crowd_plots_as_officer(plots)

  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "officer_input.docx")

  result <- crowd_output(officer_plots, path = output_path)

  testthat::expect_true(file.exists(output_path))
  testthat::expect_identical(result, output_path)
})

testthat::test_that("crowd_output() handles empty plot list", {
  library(saros)

  empty_list <- list()

  temp_dir <- withr::local_tempdir()
  output_path <- file.path(temp_dir, "empty.docx")

  # Should warn but create file
  testthat::expect_warning(
    result <- crowd_output(empty_list, path = output_path),
    "No valid plots"
  )

  testthat::expect_true(file.exists(output_path))
})

testthat::test_that("crowd_output() default path works", {
  library(saros)
  suppressMessages(library(dplyr))

  plots <- makeme(
    data = ex_survey,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    type = "cat_plot_docx"
  )

  temp_dir <- withr::local_tempdir()

  # Change working directory to temp for default path test
  withr::with_dir(
    temp_dir,
    {
      result <- crowd_output(plots)
      testthat::expect_true(file.exists("crowd_output.docx"))
      testthat::expect_identical(result, "crowd_output.docx")
    }
  )
})
