testthat::test_that("summarize_cat_cat_data", {
  #Takes on average 5 sec to run all.
  library(saros)

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = ex_survey,
      dep = paste0("a_", 1:9),

      data_label = "percentage_bare",
      showNA = "never",
      totals = FALSE,
      descend = TRUE,
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      digits = 0
    ) |>
      dplyr::filter(
        .variable_label == "Do you consent to the following? - Agreement #1",
        .category == "No"
      ) |>
      dplyr::pull(.data_label),
    expected = "60"
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = saros::ex_survey,
      dep = paste0("a_", 1:9),

      showNA = "never",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      sort_by = ".count",
      descend = TRUE
    ) |>
      dplyr::slice(1) |>
      dplyr::pull(.count),
    expected = 166
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = saros::ex_survey,
      dep = paste0("b_", 1:3),

      showNA = "never",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      sort_by = "A bit",
      descend = FALSE
    ) |>
      dplyr::slice(1) |>
      dplyr::pull(.count),
    expected = 126
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = ex_survey,
      dep = paste0("b_", 1:3),

      showNA = "never",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      sort_by = c("A bit", "A lot"),
      descend = FALSE
    ) |>
      dplyr::filter(.variable_name == "b_3", .category == "A lot") |>
      dplyr::pull(.sum_value),
    expected = .53666667
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = ex_survey,
      dep = paste0("b_", 1:3),

      showNA = "never",
      totals = FALSE,
      data_label = "percentage",
      digits = 1,
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = "."
    ) |>
      dplyr::filter(.variable_name == "b_1", .category == "A lot") |>
      dplyr::pull(.data_label),
    expected = "9.3%"
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = ex_survey,
      dep = paste0("a_", 1:9),

      showNA = "never",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      digits = 1,
      data_label_decimal_symbol = "."
    ) |>
      dplyr::filter(.variable_name == "a_2", .category == "Yes") |>
      dplyr::pull(.data_label),
    "51.9%"
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = ex_survey,
      dep = paste0("a_", 1:9),

      showNA = "never",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      sort_by = ".count",
      descend = TRUE
    ) |>
      dplyr::filter(.variable_name == "a_9", .category == "No") |>
      dplyr::pull(.count),
    expected = 146
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = ex_survey,
      dep = paste0("a_", 1:9),
      indep = "x1_sex",

      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      showNA = "never"
    ) |>
      dplyr::filter(
        .variable_name == "a_2" & .category == "No" & x1_sex == "Females"
      ) |>
      dplyr::pull(.count),
    expected = 64
  )

  testthat::expect_equal(
    saros:::summarize_cat_cat_data(
      data = ex_survey,
      dep = paste0("a_", 1:9),
      indep = c("x1_sex", "x2_human", "f_uni"),

      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      showNA = "never"
    ) |>
      dplyr::filter(
        .variable_name == "a_1",
        x1_sex == "Males",
        x2_human == "Robot?",
        f_uni == "Uni of A",
        .category == "Yes"
      ) |>
      dplyr::pull(.proportion),
    expected = 0.5
  )
})


testthat::test_that("crosstable srvyr gives same output as regular tbl with 0 indep-col", {
  suppressMessages(library(dplyr))
  suppressMessages(library(srvyr))
  x <-
    saros::ex_survey |>
    saros:::summarize_cat_cat_data(
      dep = paste0("b_", 1:3),

      sort_by = c("A lot", "A bit"),
      digits = 2,
      label_separator = " - ",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      descend = FALSE,
      showNA = "never"
    ) |>
    arrange(dplyr::pick(tidyselect::everything()))
  x_srv <-
    saros::ex_survey |>
    srvyr::as_survey(strata = f_uni) |>
    saros:::summarize_cat_cat_data(
      dep = paste0("b_", 1:3),

      sort_by = c("A lot", "A bit"),
      digits = 2,
      label_separator = " - ",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      descend = FALSE,
      showNA = "never"
    ) |>
    arrange(dplyr::pick(tidyselect::everything()))

  testthat::expect_equal(
    object = x |>
      dplyr::slice(1) |>
      dplyr::pull(.data$.proportion),
    expected = .43
  )

  testthat::expect_equal(object = names(x_srv), expected = names(x))

  testthat::expect_equal(object = nrow(x_srv), expected = nrow(x))

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".variable_name"]]),
    expected = dplyr::pull(x, .data[[".variable_name"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".category"]]),
    expected = dplyr::pull(x, .data[[".category"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".count"]]),
    expected = dplyr::pull(x, .data[[".count"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".variable_label"]]),
    expected = dplyr::pull(x, .data[[".variable_label"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".data_label"]]),
    expected = dplyr::pull(x, .data[[".data_label"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".proportion"]]),
    expected = dplyr::pull(x, .data[[".proportion"]]),
    tolerance = .000000001
  )

  testthat::expect_equal(
    object = dplyr::pull(x, .data[[".proportion_se"]]),
    expected = rep(NA_real_, nrow(x))
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".comb_categories"]]),
    expected = dplyr::pull(x, .data[[".comb_categories"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".sum_value"]]),
    expected = dplyr::pull(x, .data[[".sum_value"]])
  )
})

testthat::test_that("crosstable srvyr gives same output as regular tbl with 1 indep-col", {
  suppressMessages(library(dplyr))
  suppressMessages(library(srvyr))
  x <-
    ex_survey |>
    saros:::summarize_cat_cat_data(
      dep = paste0("b_", 1:3),
      indep = "x1_sex",

      sort_by = c("A lot", "A bit"),
      label_separator = " - ",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      descend = FALSE,
      showNA = "never"
    ) |>
    arrange(dplyr::pick(tidyselect::everything()))
  x_srv <-
    ex_survey |>
    srvyr::as_survey(strata = f_uni) |>
    saros:::summarize_cat_cat_data(
      dep = paste0("b_", 1:3),
      indep = "x1_sex",

      sort_by = c("A lot", "A bit"),
      label_separator = " - ",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      descend = FALSE,
      showNA = "never"
    ) |>
    arrange(dplyr::pick(tidyselect::everything()))

  testthat::expect_equal(object = names(x_srv), expected = names(x))

  testthat::expect_equal(object = nrow(x_srv), expected = nrow(x))

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".variable_name"]]),
    expected = dplyr::pull(x, .data[[".variable_name"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[["x1_sex"]]),
    expected = dplyr::pull(x, .data[["x1_sex"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".category"]]),
    expected = dplyr::pull(x, .data[[".category"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".count"]]),
    expected = dplyr::pull(x, .data[[".count"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".variable_label"]]),
    expected = dplyr::pull(x, .data[[".variable_label"]])
  )

  testthat::expect_equal(
    object = as.numeric(stringi::stri_replace_all_fixed(
      dplyr::pull(x_srv, .data[[".data_label"]]),
      "%",
      ""
    )),
    expected = as.numeric(stringi::stri_replace_all_fixed(
      dplyr::pull(x, .data[[".data_label"]]),
      "%",
      ""
    )),
    tolerance = .1
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".proportion"]]),
    expected = dplyr::pull(x, .data[[".proportion"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x, .data[[".proportion_se"]]),
    expected = rep(NA_real_, nrow(x))
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".comb_categories"]]),
    expected = dplyr::pull(x, .data[[".comb_categories"]])
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".sum_value"]]),
    expected = dplyr::pull(x, .data[[".sum_value"]])
  )
})


testthat::test_that("crosstable srvyr gives same output as regular tbl with 2 indep-col", {
  suppressMessages(library(dplyr))
  suppressMessages(library(srvyr))
  x <-
    ex_survey |>
    saros:::summarize_cat_cat_data(
      dep = paste0("b_", 1:3),
      indep = c("x1_sex", "x2_human"),

      sort_by = c("A lot", "A bit"),
      label_separator = " - ",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      descend = FALSE,
      showNA = "never"
    ) |>
    arrange(dplyr::pick(tidyselect::everything()))
  x_srv <-
    ex_survey |>
    srvyr::as_survey(strata = f_uni) |>
    saros:::summarize_cat_cat_data(
      dep = paste0("b_", 1:3),
      indep = c("x1_sex", "x2_human"),

      sort_by = c("A lot", "A bit"),
      label_separator = " - ",
      totals = FALSE,
      data_label = "percentage",
      hide_label_if_prop_below = 0,
      data_label_decimal_symbol = ".",
      descend = FALSE,
      showNA = "never"
    ) |>
    arrange(dplyr::pick(tidyselect::everything()))

  testthat::expect_equal(object = names(x_srv), expected = names(x))

  testthat::expect_equal(object = nrow(x_srv), expected = nrow(x))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_name"]]),
  #                        expected = dplyr::pull(x, .data[[".variable_name"]]))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[["x1_sex"]]),
  #                        expected = dplyr::pull(x, .data[["x1_sex"]]))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[["x2_human"]]),
  #                        expected = dplyr::pull(x, .data[["x2_human"]]))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".category"]]),
  #                        expected = dplyr::pull(x, .data[[".category"]]))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".count"]]),
  #                        expected = dplyr::pull(x, .data[[".count"]]))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".variable_label"]]),
  #                        expected = dplyr::pull(x, .data[[".variable_label"]]))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".data_label"]]),
  #                        expected = dplyr::pull(x, .data[[".data_label"]]))

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".proportion"]]),
  #                        expected = dplyr::pull(x, .data[[".proportion"]]))

  testthat::expect_equal(
    object = dplyr::pull(x, .data[[".proportion_se"]]),
    expected = rep(NA_real_, nrow(x))
  )

  testthat::expect_equal(
    object = dplyr::pull(x_srv, .data[[".comb_categories"]]),
    expected = dplyr::pull(x, .data[[".comb_categories"]])
  )

  # testthat::expect_equal(object = dplyr::pull(x_srv, .data[[".sum_value"]]),
  #                        expected = dplyr::pull(x, .data[[".sum_value"]]))
})

testthat::test_that("summarize_cat_cat_data preserves ordered factor levels", {
  # Create test data with ordered factors
  test_data <- data.frame(
    dep_ordered = factor(
      c("Low", "Medium", "High", "Low", "Medium", "High"),
      levels = c("Low", "Medium", "High"),
      ordered = TRUE
    ),
    indep_ordered = factor(
      c("First", "First", "First", "Second", "Second", "Second"),
      levels = c("First", "Second"),
      ordered = TRUE
    )
  )

  result <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = "dep_ordered",
    indep = "indep_ordered",
    sort_by = NULL,
    descend = FALSE
  )

  # Check that ordered factors maintain their levels
  testthat::expect_true(is.ordered(result$indep_ordered))
  testthat::expect_equal(levels(result$indep_ordered), c("First", "Second"))
  testthat::expect_equal(levels(result$.category), c("Low", "Medium", "High"))
})

testthat::test_that("summarize_cat_cat_data descend works with ordered factors", {
  # Create test data with ordered factors
  test_data <- data.frame(
    dep_ordered = factor(
      rep(c("Low", "Medium", "High"), each = 5),
      levels = c("Low", "Medium", "High"),
      ordered = TRUE
    ),
    indep_ordered = factor(
      rep(c("First", "Second"), length.out = 15),
      levels = c("First", "Second"),
      ordered = TRUE
    )
  )

  # Test with descend = TRUE
  result_desc <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = "dep_ordered",
    indep = "indep_ordered",
    sort_by = NULL,
    descend = TRUE
  )

  # Test with descend = FALSE
  result_asc <- saros:::summarize_cat_cat_data(
    data = test_data,
    dep = "dep_ordered",
    indep = "indep_ordered",
    sort_by = NULL,
    descend = FALSE
  )

  # Check that ordered factors are preserved in both cases
  testthat::expect_true(is.ordered(result_desc$indep_ordered))
  testthat::expect_true(is.ordered(result_asc$indep_ordered))

  # Check that the original level order is maintained (not reversed like unordered factors)
  testthat::expect_equal(
    levels(result_desc$indep_ordered),
    c("First", "Second")
  )
  testthat::expect_equal(levels(result_asc$indep_ordered), c("First", "Second"))
  testthat::expect_equal(
    levels(result_desc$.category),
    c("Low", "Medium", "High")
  )
  testthat::expect_equal(
    levels(result_asc$.category),
    c("Low", "Medium", "High")
  )
})
