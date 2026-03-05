testthat::test_that("int_plot_html hides axis label when hide_axis_text_if_single_variable=TRUE, single dep, no indep", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1,
    type = "int_plot_html",
    hide_axis_text_if_single_variable = TRUE,
    html_interactive = FALSE
  )

  testthat::expect_true(inherits(out, "ggplot"))
  testthat::expect_true(".variable_label" %in% colnames(out$data))
  testthat::expect_true(all(as.character(out$data[[".variable_label"]]) == ""))
  testthat::expect_true(".variable_label_original" %in% colnames(out$data))
  testthat::expect_true(all(nchar(as.character(out$data[[".variable_label_original"]])) > 0))
  # Summary geom_label layer data should also have the label hidden
  label_layer_data <- out$layers[[3]]$data
  testthat::expect_true(all(as.character(label_layer_data[[".variable_label"]]) == ""))
})

testthat::test_that("int_plot_html does NOT hide axis label when hide_axis_text_if_single_variable=FALSE, single dep, no indep", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1,
    type = "int_plot_html",
    hide_axis_text_if_single_variable = FALSE,
    html_interactive = FALSE
  )

  testthat::expect_true(inherits(out, "ggplot"))
  testthat::expect_true(".variable_label" %in% colnames(out$data))
  testthat::expect_true(all(nchar(as.character(out$data[[".variable_label"]])) > 0))
  testthat::expect_false(".variable_label_original" %in% colnames(out$data))
})

testthat::test_that("int_plot_html does NOT hide axis label when hide_axis_text_if_single_variable=TRUE but multiple dep vars", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_plot_html",
    hide_axis_text_if_single_variable = TRUE,
    html_interactive = FALSE
  )

  testthat::expect_true(inherits(out, "ggplot"))
  testthat::expect_true(".variable_label" %in% colnames(out$data))
  testthat::expect_true(all(nchar(as.character(out$data[[".variable_label"]])) > 0))
  testthat::expect_false(".variable_label_original" %in% colnames(out$data))
})

testthat::test_that("int_plot_html does NOT hide axis label when hide_axis_text_if_single_variable=TRUE but indep is present", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1,
    indep = x1_sex,
    type = "int_plot_html",
    hide_axis_text_if_single_variable = TRUE,
    html_interactive = FALSE
  )

  testthat::expect_true(inherits(out, "ggplot"))
  testthat::expect_false(".variable_label_original" %in% colnames(out$data))
})
