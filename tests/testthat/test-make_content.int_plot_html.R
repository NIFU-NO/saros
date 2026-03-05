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
  testthat::expect_true(all(
    nchar(as.character(out$data[[".variable_label_original"]])) > 0
  ))
  # Summary geom_label layer data should also have the label hidden.
  # Locate the layer by geom class rather than by index to be robust to layer reordering.
  label_layer <- Filter(
    function(l) inherits(l$geom, c("GeomLabelInteractive", "GeomLabel")),
    out$layers
  )
  testthat::expect_length(label_layer, 1L)
  label_layer_data <- label_layer[[1]]$data
  testthat::expect_true(
    ".variable_label" %in% colnames(label_layer_data),
    label = "geom_label layer data must contain a .variable_label column"
  )
  testthat::expect_true(all(
    as.character(label_layer_data[[".variable_label"]]) == ""
  ))
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
  testthat::expect_true(all(
    nchar(as.character(out$data[[".variable_label"]])) > 0
  ))
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
  testthat::expect_true(all(
    nchar(as.character(out$data[[".variable_label"]])) > 0
  ))
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

testthat::test_that("int_plot_html always hides fill legend (single dep, no indep)", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1,
    type = "int_plot_html",
    html_interactive = FALSE
  )

  testthat::expect_true(inherits(out, "ggplot"))
  testthat::expect_identical(out[["guides"]][["guides"]][["fill"]], "none")
  testthat::expect_identical(out$theme$legend.position, "none")
})

testthat::test_that("int_plot_html always hides fill legend (multiple dep vars)", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_plot_html",
    html_interactive = FALSE
  )

  testthat::expect_true(inherits(out, "ggplot"))
  testthat::expect_identical(out[["guides"]][["guides"]][["fill"]], "none")
  testthat::expect_identical(out$theme$legend.position, "none")
})

testthat::test_that("int_plot_html always hides fill legend (with indep)", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1,
    indep = x1_sex,
    type = "int_plot_html",
    html_interactive = FALSE
  )

  testthat::expect_true(inherits(out, "ggplot"))
  testthat::expect_identical(out[["guides"]][["guides"]][["fill"]], "none")
  testthat::expect_identical(out$theme$legend.position, "none")
})
