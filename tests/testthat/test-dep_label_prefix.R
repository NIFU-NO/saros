# Tests for dep_label_prefix attachment and retrieval

# ---------------------------------------------------------------------------
# Helper: attach_dep_label_prefix / get_dep_label_prefix directly
# ---------------------------------------------------------------------------

test_that("get_dep_label_prefix returns empty string for plain objects with no attribute", {
  expect_equal(saros:::get_dep_label_prefix(data.frame(x = 1)), "")
  expect_equal(saros:::get_dep_label_prefix(list(a = 1)), "")
  expect_equal(saros:::get_dep_label_prefix(42L), "")
})

test_that("attach_dep_label_prefix stores on data.frame and is retrieved correctly", {
  df <- data.frame(x = 1:3)
  df2 <- saros:::attach_dep_label_prefix(df, "My heading")
  expect_equal(saros::get_dep_label_prefix(df2), "My heading")
})

test_that("attach_dep_label_prefix is a no-op for NULL / NA / empty main_question", {
  df <- data.frame(x = 1)
  expect_equal(
    saros::get_dep_label_prefix(saros:::attach_dep_label_prefix(df, NULL)),
    ""
  )
  expect_equal(
    saros::get_dep_label_prefix(saros:::attach_dep_label_prefix(
      df,
      NA_character_
    )),
    ""
  )
  expect_equal(
    saros::get_dep_label_prefix(saros:::attach_dep_label_prefix(df, "")),
    ""
  )
})

test_that("attach_dep_label_prefix stores on ggplot$data, not on the plot itself", {
  p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x))
  p2 <- saros:::attach_dep_label_prefix(p, "GG heading")
  # stored on $data
  expect_equal(attr(p2$data, "dep_label_prefix", exact = TRUE), "GG heading")
  # NOT on the plot object itself
  expect_null(attr(p2, "dep_label_prefix", exact = TRUE))
})

test_that("get_dep_label_prefix reads from ggplot$data", {
  p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x))
  attr(p$data, "dep_label_prefix") <- "Plot heading"
  expect_equal(saros::get_dep_label_prefix(p), "Plot heading")
})

test_that("dep_label_prefix on ggplot$data survives further + operations", {
  p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x))
  attr(p$data, "dep_label_prefix") <- "Survives +"
  p2 <- p + ggplot2::theme_minimal() + ggplot2::labs(title = "t")
  expect_equal(saros::get_dep_label_prefix(p2), "Survives +")
})

# ---------------------------------------------------------------------------
# Integration: makeme() attaches the attribute on all output types
# ---------------------------------------------------------------------------

test_that("makeme cat_plot_html attaches dep_label_prefix on $data", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_plot_html",
    label_separator = " - ",
    html_interactive = FALSE,
    showNA = "never"
  )
  expect_s3_class(out, "gg")
  prefix <- saros::get_dep_label_prefix(out)
  expect_true(nzchar(prefix))
})

test_that("makeme cat_plot_html still carries dep_label_prefix after coord_flip", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_plot_html",
    label_separator = " - ",
    html_interactive = FALSE,
    showNA = "never",
    vertical = FALSE # triggers coord_flip
  )
  expect_true(nzchar(saros::get_dep_label_prefix(out)))
})

test_that("attach_dep_label_prefix on empty ggplot() falls back to attr(obj, ...) since $data is waiver", {
  # ggplot2::ggplot() with no data has $data = waiver(), not a data.frame.
  # attach_dep_label_prefix must not error and must fall back to attr(obj, ...).
  empty_p <- ggplot2::ggplot()
  expect_false(is.data.frame(empty_p$data)) # confirm it's a waiver
  annotated <- saros:::attach_dep_label_prefix(empty_p, "Waiver heading")
  # stored on the plot object itself, not on $data
  expect_equal(
    attr(annotated, "dep_label_prefix", exact = TRUE),
    "Waiver heading"
  )
  # get_dep_label_prefix reaches it via the fallback path
  expect_equal(saros::get_dep_label_prefix(annotated), "Waiver heading")
})

test_that("makeme int_plot_html attaches dep_label_prefix on $data", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_plot_html",
    html_interactive = FALSE
  )
  expect_s3_class(out, "gg")
  expect_true(nzchar(saros::get_dep_label_prefix(out)))
})

test_that("makeme int_plot_html dep_label_prefix survives coord_flip", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_plot_html",
    html_interactive = FALSE,
    vertical = FALSE # triggers coord_flip
  )
  expect_true(nzchar(saros::get_dep_label_prefix(out)))
})

test_that("makeme cat_table_html attaches dep_label_prefix", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_table_html",
    label_separator = " - ",
    showNA = "never"
  )
  expect_true(is.data.frame(out))
  expect_true(nzchar(saros::get_dep_label_prefix(out)))
})

test_that("makeme chr_table_html attaches dep_label_prefix", {
  df <- data.frame(
    q_open = c("Answer A", "Answer B", "Answer C"),
    stringsAsFactors = FALSE
  )
  attr(df$q_open, "label") <- "Open question text"
  out <- saros::makeme(
    data = df,
    dep = q_open,
    type = "chr_table_html",
    label_separator = NULL
  )
  expect_true(is.data.frame(out))
  # For chr_table_html the main_question derives from the label attribute
  prefix <- saros::get_dep_label_prefix(out)
  expect_type(prefix, "character")
})

test_that("makeme int_table_html attaches dep_label_prefix", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_table_html",
    label_separator = " - "
  )
  expect_true(is.data.frame(out))
  expect_true(nzchar(saros::get_dep_label_prefix(out)))
})

test_that("makeme sigtest_table_html attaches dep_label_prefix", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    indep = x1_sex,
    type = "sigtest_table_html"
  )
  expect_true(is.data.frame(out))
  prefix <- saros::get_dep_label_prefix(out)
  expect_type(prefix, "character")
})

test_that("makeme cat_plot_docx docx_return_object=TRUE attaches dep_label_prefix on $data", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_plot_docx",
    label_separator = " - ",
    showNA = "never",
    docx_return_object = TRUE
  )
  expect_true(inherits(out, "ms_barchart"))
  expect_true(nzchar(saros::get_dep_label_prefix(out)))
  # stored on $data, not on the chart object itself
  expect_equal(
    attr(out$data, "dep_label_prefix", exact = TRUE),
    saros::get_dep_label_prefix(out)
  )
  expect_null(attr(out, "dep_label_prefix", exact = TRUE))
})

test_that("makeme cat_plot_docx rdocx path does NOT expose dep_label_prefix", {
  # When returning a full rdocx, no attribute should be set (can't carry one)
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_3,
    type = "cat_plot_docx",
    label_separator = " - ",
    showNA = "never",
    docx_return_object = FALSE
  )
  expect_s3_class(out, "rdocx")
  expect_equal(saros::get_dep_label_prefix(out), "")
})
