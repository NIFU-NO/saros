# #458: hovering a colour (category) should highlight every bar of that
# category across all variables in cat_plot_html.
#
# ggiraph groups hover highlighting by `data_id`: elements sharing a data-id
# highlight together. cat_plot_html used to map data_id to a per-row sequence
# (`.id = seq_len(nrow(data))`), so each bar had a unique id and hovering
# highlighted only the bar under the cursor. The legend, meanwhile, already
# mapped data_id to the category via scale_fill_discrete_interactive(), so the
# two never matched and hovering a legend key highlighted nothing at all.

# data-id values are written into the SVG by girafe(), not by the dsvg device,
# and the SVG uses single-quoted attributes.
plot_data_ids <- function(p) {
  html <- ggiraph::girafe(ggobj = p)$x$html
  ids <- regmatches(html, gregexpr("data-id='[^']*'", html))[[1]]
  unique(sub("data-id='", "", sub("'$", "", ids)))
}

testthat::test_that("cat_plot_html groups hover highlighting by category, not by row (#458)", {
  testthat::skip_on_cran()

  p <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_plot_html",
    label_separator = " - ",
    showNA = "never"
  )

  observed <- plot_data_ids(p)
  categories <- levels(p$data$.category)

  # One id per category, shared across all three variables -- not one per bar.
  testthat::expect_setequal(observed, categories)
})

testthat::test_that("bar data_id matches the legend's data_id so legend hover highlights bars (#458)", {
  testthat::skip_on_cran()

  # scale_fill_discrete_interactive() sets the legend key's data_id to the
  # category label. The bars must use the same values or the legend cannot
  # highlight anything.
  p <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_plot_html",
    label_separator = " - ",
    showNA = "never"
  )

  testthat::expect_true(all(plot_data_ids(p) %in% levels(p$data$.category)))
})

testthat::test_that("category hover grouping also holds with an independent variable (#458)", {
  testthat::skip_on_cran()

  p <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "cat_plot_html",
    label_separator = " - ",
    showNA = "never"
  )

  observed <- plot_data_ids(p)

  # Faceted plots also give the strip labels their own data_id via
  # labeller_interactive(), so the categories are a subset here rather than the
  # whole set.
  testthat::expect_true(all(levels(p$data$.category) %in% observed))

  # No bare row indices: those would mean the bars are still grouped per row.
  testthat::expect_false(any(grepl("^[0-9]+$", observed)))
})

testthat::test_that("tooltips stay per-bar when hover grouping is per-category (#458)", {
  testthat::skip_on_cran()

  # Grouping the highlight by category must not collapse the tooltips, which
  # carry each bar's own numbers.
  p <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_plot_html",
    label_separator = " - ",
    showNA = "never"
  )
  html <- ggiraph::girafe(ggobj = p)$x$html
  # ggiraph writes tooltips as single-quoted title attributes, not <title> tags.
  tooltips <- regmatches(html, gregexpr("title='[^']*'", html))[[1]]

  # More distinct tooltips than categories: they are still per bar.
  testthat::expect_gt(
    length(unique(tooltips)),
    length(levels(p$data$.category))
  )
})
