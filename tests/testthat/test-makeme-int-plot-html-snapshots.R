test_that("int_plot_html basic functionality works", {
  data("ex_survey", package = "saros")

  # Test basic functionality without independent variable
  result <- saros::makeme(
    data = ex_survey,
    dep = c_1,
    type = "int_plot_html"
  )

  expect_s3_class(result, "ggplot")
  expect_true("GeomViolin" %in% class(result$layers[[1]]$geom))
  expect_true("GeomBoxplot" %in% class(result$layers[[2]]$geom))
})

test_that("int_plot_html with single dependent variable", {
  vdiffr::expect_doppelganger(title = "int_plot_html single dep variable", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1,
      type = "int_plot_html"
    )
  })
})

test_that("int_plot_html with multiple dependent variables", {
  vdiffr::expect_doppelganger(title = "int_plot_html multiple dep variables", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      type = "int_plot_html"
    )
  })
})

test_that("int_plot_html with independent variable", {
  vdiffr::expect_doppelganger(title = "int_plot_html with indep variable", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1,
      indep = x1_sex,
      type = "int_plot_html"
    )
  })
})

test_that("int_plot_html with multiple dep and indep variables", {
  vdiffr::expect_doppelganger(title = "int_plot_html multiple dep with indep", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      indep = x1_sex,
      type = "int_plot_html"
    )
  })
})

test_that("int_plot_html with inverse option", {
  vdiffr::expect_doppelganger(title = "int_plot_html with inverse=TRUE", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      indep = x1_sex,
      type = "int_plot_html",
      inverse = TRUE
    )
  })
})

test_that("int_plot_html with vertical option", {
  vdiffr::expect_doppelganger(title = "int_plot_html with vertical=TRUE", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      indep = x1_sex,
      type = "int_plot_html",
      vertical = TRUE
    )
  })
})

test_that("int_plot_html respects data with different distributions", {
  # Use only single variable to avoid "common categories" error with numeric data
  set.seed(123)
  test_data <- data.frame(
    var_normal = rnorm(300, mean = 20, sd = 5),
    group = rep(c("A", "B", "C"), each = 100)
  )

  vdiffr::expect_doppelganger(title = "int_plot_html diff_distributions", fig = {
    saros::makeme(
      data = test_data,
      dep = var_normal,
      indep = group,
      label_separator = NULL,
      type = "int_plot_html"
    )
  })
})

test_that("int_plot_html handles missing data correctly", {
  # Create test data with missing values
  test_data <- saros::ex_survey
  test_data$c_1[1:30] <- NA
  test_data$c_2[50:80] <- NA

  vdiffr::expect_doppelganger(title = "int_plot_html missing_data", fig = {
    saros::makeme(
      data = test_data,
      dep = c_1:c_2,
      indep = x1_sex,
      type = "int_plot_html"
    )
  })
})

test_that("int_plot_html with crowd functionality", {
  vdiffr::expect_doppelganger(title = "int_plot_html with crowd", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      indep = x1_sex,
      type = "int_plot_html",
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A"
    )
  })
})

test_that("int_plot_html with label customization", {
  vdiffr::expect_doppelganger(title = "int_plot_html with custom labels", fig = {
    saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      indep = x1_sex,
      type = "int_plot_html",
      x_axis_label_width = 15,
      strip_width = 20
    )
  })
})

test_that("int_plot_html error handling", {
  data("ex_survey", package = "saros")

  # Test with categorical variable (should error because it lacks mean/SD statistics)
  expect_error(
    saros::makeme(
      data = ex_survey,
      dep = a_1, # This is categorical - keep for error test
      type = "int_plot_html"
    ),
    regexp = "Column `mean` not found"
  )

  # Test with mixed variable types - should error before reaching the statistics issue
  expect_error(
    saros::makeme(
      data = ex_survey,
      dep = c(c_1, a_1), # c_1 is numeric, a_1 is factor
      type = "int_plot_html"
    )
    # Don't specify regexp as error could occur at different stages
  )
})
