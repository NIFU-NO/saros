# Regression guard for #624: `.variable_label_original` had two meanings.
# add_n_to_label() stores the dependent variable's label there before adding
# its "(N = ...)" suffix, as the key txt_from_cat_mesos_plots() joins crowds
# on (#554). The make_content.*() methods also wrote it when they hid the
# label of a single-variable plot, and fig_height_h_barchart2() took its mere
# presence to mean "hidden". Two things went wrong:
#
# - With the default hide_axis_text_if_single_variable = TRUE, hiding copied
#   the N-suffixed label over the join key, so the crowds no longer matched
#   and the mesos text read the others' proportion as 0 -- and reported the
#   opposite of the truth.
# - With the label drawn, add_n_to_dep_label alone made the figure be sized
#   as though it were hidden.
#
# Whether the label is drawn is now recorded on the plot data by the
# make_content.*() method, and fig_height_h_barchart2() reads that instead.

mesos_plots <- function(...) {
  makeme(
    data = saros::ex_survey,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    add_n_to_dep_label = TRUE,
    simplify_output = FALSE,
    ...
  )
}

height <- function(...) {
  fig_height_h_barchart2(makeme(data = saros::ex_survey, ...))
}

hidden_attr <- function(obj) attr(obj$data, "dep_label_hidden", exact = TRUE)

testthat::test_that("hiding the label keeps the pre-suffix join key", {
  plots <- mesos_plots()

  testthat::expect_equal(
    unique(as.character(plots$Target$data$.variable_label_original)),
    "Bejing"
  )
  testthat::expect_equal(
    unique(as.character(plots$Others$data$.variable_label_original)),
    "Bejing"
  )
  # The label itself is still blanked
  testthat::expect_true(all(as.character(plots$Target$data$.variable_label) == ""))
})

testthat::test_that("the mesos text is the same whether or not the label is hidden", {
  hidden <- mesos_plots()
  shown <- mesos_plots(hide_axis_text_if_single_variable = FALSE)

  txt_hidden <- withr::with_seed(624, txt_from_cat_mesos_plots(hidden, min_prop_diff = 0))
  txt_shown <- withr::with_seed(624, txt_from_cat_mesos_plots(shown, min_prop_diff = 0))

  testthat::expect_identical(txt_hidden, txt_shown)
  # The others' proportion is found rather than read as 0
  others <- saros::ex_survey[saros::ex_survey$f_uni != "Uni of A", ]
  others_prop <- round(mean(others$b_1 == "A lot", na.rm = TRUE), 2)
  testthat::expect_match(txt_hidden, paste0("(", others_prop, ")"), fixed = TRUE)
})

testthat::test_that("cat_plot_docx keeps the pre-suffix join key too", {
  m <- makeme(
    data = saros::ex_survey,
    dep = b_1,
    type = "cat_plot_docx",
    docx_return_object = TRUE,
    add_n_to_dep_label = TRUE
  )
  testthat::expect_equal(unique(as.character(m$data$.variable_label_original)), "Bejing")
})

testthat::test_that("add_n_to_dep_label does not make a drawn label count as hidden", {
  hidden <- height(dep = b_1)
  shown <- height(dep = b_1, hide_axis_text_if_single_variable = FALSE)
  shown_n <- height(
    dep = b_1,
    hide_axis_text_if_single_variable = FALSE,
    add_n_to_dep_label = TRUE
  )
  testthat::expect_gt(shown, hidden)
  testthat::expect_gte(shown_n, shown)

  shown_indep <- height(
    dep = b_1, indep = f_uni,
    hide_axis_text_if_single_variable = FALSE
  )
  shown_indep_n <- height(
    dep = b_1, indep = f_uni,
    hide_axis_text_if_single_variable = FALSE,
    add_n_to_dep_label = TRUE
  )
  testthat::expect_gte(shown_indep_n, shown_indep)
})

testthat::test_that("each plot type records whether the label is drawn", {
  d <- saros::ex_survey
  html <- function(...) makeme(data = d, ...)
  docx <- function(...) {
    makeme(data = d, type = "cat_plot_docx", docx_return_object = TRUE, ...)
  }

  # cat_plot_html: blanked without an indep, strip left out with one
  testthat::expect_true(hidden_attr(html(dep = b_1)))
  testthat::expect_false(
    hidden_attr(html(dep = b_1, hide_axis_text_if_single_variable = FALSE))
  )
  testthat::expect_false(hidden_attr(html(
    dep = b_1,
    hide_axis_text_if_single_variable = FALSE,
    add_n_to_dep_label = TRUE
  )))
  testthat::expect_true(hidden_attr(html(dep = b_1, indep = f_uni)))
  testthat::expect_false(hidden_attr(html(dep = b_1, indep = f_uni, inverse = TRUE)))
  testthat::expect_false(hidden_attr(html(dep = b_1:b_3, indep = f_uni)))
  testthat::expect_false(hidden_attr(html(dep = b_1:b_3)))

  # int_plot_html: an indep puts the label in a facet strip
  testthat::expect_true(hidden_attr(html(dep = c_1, type = "int_plot_html")))
  testthat::expect_false(
    hidden_attr(html(dep = c_1, indep = x1_sex, type = "int_plot_html"))
  )

  # cat_plot_docx: no facets, so an indep on the axis leaves the label out
  testthat::expect_true(hidden_attr(docx(dep = b_1)))
  testthat::expect_true(hidden_attr(docx(dep = b_1, indep = f_uni)))
  testthat::expect_false(hidden_attr(docx(
    dep = b_1,
    hide_axis_text_if_single_variable = FALSE,
    add_n_to_dep_label = TRUE
  )))
})

testthat::test_that("the recorded state takes precedence over the global setting", {
  shown_default <- height(
    dep = b_1, indep = f_uni,
    hide_axis_text_if_single_variable = FALSE
  )
  int_default <- height(dep = c_1, indep = x1_sex, type = "int_plot_html")

  global_settings_set(
    fn_name = "fig_height_h_barchart",
    new = list(hide_axis_text_if_single_variable = TRUE),
    quiet = TRUE
  )
  withr::defer(global_settings_reset(fn_name = "fig_height_h_barchart", quiet = TRUE))

  testthat::expect_equal(
    height(dep = b_1, indep = f_uni, hide_axis_text_if_single_variable = FALSE),
    shown_default
  )
  testthat::expect_equal(
    height(dep = c_1, indep = x1_sex, type = "int_plot_html"),
    int_default
  )
})

testthat::test_that("dep_label_hidden() falls back on the data, then the default", {
  shown <- data.frame(.variable_name = "a", .variable_label = "A")
  blank <- data.frame(.variable_name = "a", .variable_label = "")

  testthat::expect_false(saros:::dep_label_hidden(shown))
  testthat::expect_true(saros:::dep_label_hidden(shown, default = TRUE))
  testthat::expect_true(saros:::dep_label_hidden(blank))

  # A recorded state wins over both
  attr(shown, "dep_label_hidden") <- TRUE
  testthat::expect_true(saros:::dep_label_hidden(shown))
  attr(blank, "dep_label_hidden") <- FALSE
  testthat::expect_false(saros:::dep_label_hidden(blank, default = TRUE))

  # The column alone no longer counts
  with_original <- data.frame(
    .variable_name = "a",
    .variable_label = "A (N = 3)",
    .variable_label_original = "A"
  )
  testthat::expect_false(saros:::dep_label_hidden(with_original))
})
