# Regression guard for #457: ggiraph emitted
#   "Failed setting attribute 'data-id'/'onclick', mismatched lengths of ids
#    and values (most often ... because of clipping or because of NAs in data)"
# when rendering cat_plot_html.
#
# Root cause was upstream in ggiraph and fixed in ggiraph 0.9.2 ("improve
# geom_line_interactive() and geom_path_interactive() with *mismatched lengths
# of ids* message when data have NA"). DESCRIPTION now requires ggiraph
# (>= 0.9.2).
#
# These tests render through ggiraph's own SVG device — the warning is emitted
# at draw time, not at plot-build time, so `print()` on a device is required to
# exercise it. They fail if saros reintroduces a data/attribute length mismatch
# (e.g. by clipping bars with scale limits while interactive aesthetics keep the
# full-length vectors).

# Collects only the ggiraph id-mismatch warnings this file is about. Any other
# warning is deliberately left to propagate, so an unexpected draw-time warning
# stays visible in the test output instead of being silently swallowed.
render_id_mismatch_warnings <- function(p) {
  mismatches <- character(0)
  withCallingHandlers(
    {
      svg_file <- withr::local_tempfile(fileext = ".svg")
      ggiraph::dsvg(file = svg_file)
      on.exit(grDevices::dev.off(), add = TRUE)
      print(p)
    },
    warning = function(w) {
      if (grepl("mismatched lengths", conditionMessage(w), fixed = TRUE)) {
        mismatches <<- c(mismatches, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    }
  )
  mismatches
}

expect_no_id_mismatch <- function(p) {
  testthat::expect_equal(render_id_mismatch_warnings(p), character(0))
}

testthat::test_that("cat_plot_html renders without ggiraph id-mismatch warnings (#457)", {
  testthat::skip_on_cran()

  expect_no_id_mismatch(
    saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_3,
      type = "cat_plot_html",
      label_separator = " - ",
      showNA = "never"
    )
  )
})

testthat::test_that("cat_plot_html with NA values renders without id-mismatch warnings (#457)", {
  testthat::skip_on_cran()

  # NAs in the data are the condition the ggiraph warning explicitly names.
  data_with_na <- saros::ex_survey
  data_with_na$b_1[1:50] <- NA

  expect_no_id_mismatch(
    saros::makeme(
      data = data_with_na,
      dep = b_1:b_3,
      type = "cat_plot_html",
      label_separator = " - ",
      showNA = "always"
    )
  )
})

testthat::test_that("cat_plot_html with a dep that is all-NA inside one indep group renders cleanly (#457)", {
  testthat::skip_on_cran()

  # b_1 has no responses at all for the first x1_sex group, so that group's bars
  # are absent from b_1's panel while the interactive attribute vectors are
  # still built for the full data.
  sparse_data <- saros::ex_survey
  sparse_data$b_1[sparse_data$x1_sex == levels(sparse_data$x1_sex)[1]] <- NA

  expect_no_id_mismatch(
    saros::makeme(
      data = sparse_data,
      dep = b_1:b_3,
      indep = x1_sex,
      type = "cat_plot_html",
      label_separator = " - ",
      showNA = "never"
    )
  )
})

testthat::test_that("cat_plot_html with hidden data labels renders without id-mismatch warnings (#457)", {
  testthat::skip_on_cran()

  # hide_label_if_prop_below blanks labels, so the text layer draws fewer
  # elements than the column layer — a length mismatch if handled naively.
  expect_no_id_mismatch(
    saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_3,
      type = "cat_plot_html",
      label_separator = " - ",
      showNA = "never",
      hide_label_if_prop_below = 0.5
    )
  )
})

testthat::test_that("cat_plot_html with an unused factor level renders without id-mismatch warnings (#457)", {
  testthat::skip_on_cran()

  # An empty category still gets a legend key (drop = FALSE), so the legend's
  # data_id vector is longer than the number of categories present in the data.
  data_unused_level <- saros::ex_survey
  original_label <- attr(data_unused_level$b_1, "label")
  data_unused_level$b_1 <- factor(
    as.character(data_unused_level$b_1),
    levels = c(levels(saros::ex_survey$b_1), "Never chosen")
  )
  attr(data_unused_level$b_1, "label") <- original_label

  expect_no_id_mismatch(
    saros::makeme(
      data = data_unused_level,
      dep = b_1:b_3,
      type = "cat_plot_html",
      label_separator = " - ",
      showNA = "never"
    )
  )
})
