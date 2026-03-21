testthat::test_that("process_crowd_data drops dep with all-NA in subset_data", {
  # Create minimal scenario: subset_data has b_2 all-NA,
  # but omitted_cols_list doesn't include b_2 (simulating bypass of keep_cols)
  test_data <- saros::ex_survey
  test_data$b_2 <- NA

  args <- list(
    data = test_data,
    dep = c("b_1", "b_2", "b_3"),
    indep = character(0),
    type = "cat_table_html",
    label_separator = " - ",
    showNA = "never",
    totals = FALSE,
    sort_dep_by = ".variable_position",
    sort_indep_by = ".factor_order",
    descend = TRUE,
    descend_indep = FALSE,
    data_label = "percentage",
    digits = 0,
    add_n_to_dep_label = FALSE,
    add_n_to_indep_label = FALSE,
    add_n_to_label = FALSE,
    add_n_to_category = FALSE,
    hide_label_if_prop_below = 0.01,
    data_label_decimal_symbol = ".",
    categories_treated_as_na = character(0),
    labels_always_at_bottom = character(0),
    labels_always_at_top = character(0),
    translations = list(table_heading_N = "Total (N)"),
    hide_for_all_crowds_if_hidden_for_crowd = NULL,
    hide_indep_cat_for_all_crowds_if_hidden_for_crowd = FALSE,
    n_categories_limit = 10,
    table_main_question_as_header = FALSE,
    hide_axis_text_if_single_variable = FALSE,
    error_on_duplicates = FALSE,
    mesos_var = NULL,
    mesos_group = NULL
  )

  testthat::expect_warning(
    result <- saros:::process_crowd_data(
      crwd = "all",
      args = args,
      omitted_cols_list = list(all = character(0)),
      kept_indep_cats_list = list(all = list()),
      data = test_data,
      mesos_var = NULL,
      mesos_group = NULL
    ),
    regexp = "no non-NA data"
  )
  # b_2 dropped, result from b_1 + b_3
  testthat::expect_true(is.data.frame(result))
  testthat::expect_gt(nrow(result), 0)
})

testthat::test_that("process_crowd_data returns NULL when all deps are all-NA", {
  test_data <- saros::ex_survey
  test_data$b_1 <- NA
  test_data$b_2 <- NA
  test_data$b_3 <- NA

  args <- list(
    data = test_data,
    dep = c("b_1", "b_2", "b_3"),
    indep = character(0),
    type = "cat_table_html",
    label_separator = " - ",
    showNA = "never",
    totals = FALSE,
    sort_dep_by = ".variable_position",
    sort_indep_by = ".factor_order",
    descend = TRUE,
    descend_indep = FALSE,
    data_label = "percentage",
    digits = 0,
    add_n_to_dep_label = FALSE,
    add_n_to_indep_label = FALSE,
    add_n_to_label = FALSE,
    add_n_to_category = FALSE,
    hide_label_if_prop_below = 0.01,
    data_label_decimal_symbol = ".",
    categories_treated_as_na = character(0),
    labels_always_at_bottom = character(0),
    labels_always_at_top = character(0),
    translations = list(table_heading_N = "Total (N)"),
    hide_for_all_crowds_if_hidden_for_crowd = NULL,
    hide_indep_cat_for_all_crowds_if_hidden_for_crowd = FALSE,
    n_categories_limit = 10,
    table_main_question_as_header = FALSE,
    hide_axis_text_if_single_variable = FALSE,
    error_on_duplicates = FALSE,
    mesos_var = NULL,
    mesos_group = NULL
  )

  testthat::expect_warning(
    result <- saros:::process_crowd_data(
      crwd = "all",
      args = args,
      omitted_cols_list = list(all = character(0)),
      kept_indep_cats_list = list(all = list()),
      data = test_data,
      mesos_var = NULL,
      mesos_group = NULL
    ),
    regexp = "no non-NA data"
  )
  testthat::expect_null(result)
})
