# test_that("embed_cat_text_html", {
#   #  Test 1: Check if the function returns a character
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = tidyselect::starts_with("e_"),
#
#                                   digits = 0,
#                                   data_label = "count",
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#
#     testthat::expect_true(is.list(result))
#     #  Test 1b: Check if the function returns a character
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   digits=0,
#                                   data_label = "count",
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true(is.list(result))
#
# #  Test 2: Check if the intro content is correct
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   contents = "intro",
#                                   digits = 0,
#                                   data_label = "count",
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true("intro" %in% names(result))
#
# #  Test 3: Check if the mode_max content is correct
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   contents = "mode_max",
#                                   digits = 0,
#                                   data_label = "count",
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true("mode_max" %in% names(result))
#
#  # Test 4: Check if the not_used_category content is correct
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   contents = "not_used_category",
#                                   ignore_if_below = 30,
#                                   digits=0,
#                                   data_label = "count",
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true("not_used_category" %in% names(result))
#
#   #Test 5: Check if the value_max content is correct
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   contents = "value_max",
#                                   digits = 0,
#                                   data_label = "count",
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   n_top_bottom = 1,
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true("value_max" %in% names(result))
#
#   #Test 6: Check if the value_min content is correct
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   contents = "value_min",
#                                   digits = 0,
#                                   data_label = "count",
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   n_top_bottom = 1,
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true("value_min" %in% names(result))
#
#   #Test 7: Check if the mean_max content is correct
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   contents = "mean_max",
#                                   digits = 0,
#                                   data_label = "count",
#                                   n_top_bottom = 1,
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true("mean_max" %in% names(result))
#
# # Test 8: Check if the mean_min content is correct
#     result <- embed_cat_text_html(ex_survey1,
#                                   dep = starts_with("e_"),
#
#                                   contents = "mean_min",
#                                   digits=0,
#                                   data_label = "count",
#                                   n_top_bottom = 1,
#                                   showNA = "never",
#                                   hide_label_if_prop_below = 0,
#                                   data_label_decimal_symbol = ".",
#                                   return_raw = FALSE,
#                                   totals = FALSE,
#                                   descend = FALSE,
#                                   require_common_categories = TRUE)
#     testthat::expect_true("mean_min" %in% names(result))
#
# # Test 9: Test when sort_by is NULL (default value)
#   result <- embed_cat_text_html(data = ex_survey1,
#                                 dep = matches("e_"),
#
#                                 digits=0,
#                                 data_label = "percentage",
#                                 contents = "value_max",
#                                 n_top_bottom = 1,
#                                 showNA = "never",
#                                 hide_label_if_prop_below = 0,
#                                 data_label_decimal_symbol = ".",
#                                 return_raw = FALSE,
#                                 totals = FALSE,
#                                 descend = FALSE,
#                                 require_common_categories = TRUE)
#   testthat::expect_true(!is.null(result))
#
# # Test 10: Test when sort_by is a character vector
#   result <- embed_cat_text_html(data = ex_survey1,
#                                 dep = matches("a_"),
#
#                                 data_label = "percentage",
#                                 digits = 0,
#                                 contents = "value_max",
#                                 sort_by = c("Yes", "No"),
#                                 n_top_bottom = 1,
#                                 showNA = "never",
#                                 hide_label_if_prop_below = 0,
#                                 data_label_decimal_symbol = ".",
#                                 return_raw = FALSE,
#                                 totals = FALSE,
#                                 descend = FALSE,
#                                 require_common_categories = TRUE)
#   testthat::expect_true(!is.null(result))
#
# # Test 11: Test when sort_by is a character vector in a different order
#   result <- embed_cat_text_html(data = ex_survey1,
#                                 dep = matches("a_"),
#
#                                 data_label = "percentage",
#                                 contents = "value_max",
#                                 sort_by = c("No", "Yes"),
#                                 digits = 0,
#                                 n_top_bottom = 1,
#                                 showNA = "never",
#                                 hide_label_if_prop_below = 0,
#                                 data_label_decimal_symbol = ".",
#                                 return_raw = FALSE,
#                                 totals = FALSE,
#                                 descend = FALSE,
#                                 require_common_categories = TRUE)
#   testthat::expect_true(!is.null(result))
#
# # Test 12: Test when sort_by is a character vector with invalid values
#   testthat::expect_error(embed_cat_text_html(data = ex_survey1,
#                                              dep = matches("e_"),
#
#                                              data_label = "percentage",
#                                              contents = "value_max",
#                                              digits = 0,
#                                              sort_by = c("Invalid1", "Invalid2"),
#                                              n_top_bottom = 1,
#                                              showNA = "never",
#                                              hide_label_if_prop_below = 0,
#                                              data_label_decimal_symbol = ".",
#                                              return_raw = FALSE,
#                                              totals = FALSE,
#                                              descend = FALSE,
#                                              require_common_categories = TRUE))
#
# # Test 13: Test when sort_by is an integer
#   testthat::expect_error(embed_cat_text_html(data = ex_survey1,
#                                              dep = matches("e_"),
#
#                                              data_label = "percentage",
#                                              contents = "value_max",
#                                              sort_by = 1,
#                                              digits = 0,
#                                              n_top_bottom = 1,
#                                              showNA = "never",
#                                              hide_label_if_prop_below = 0,
#                                              data_label_decimal_symbol = ".",
#                                              return_raw = FALSE,
#                                              totals = FALSE,
#                                              descend = FALSE,
#                                              require_common_categories = TRUE))
#
# # Test 14: Test when sort_by is a negative integer
#   testthat::expect_error(embed_cat_text_html(data = ex_survey1,
#                                              dep = matches("e_"),
#
#                                              data_label = "percentage",
#                                              contents = "value_max",
#                                              sort_by = -1,
#                                              digits = 0,
#                                              n_top_bottom = 1,
#                                              showNA = "never",
#                                              hide_label_if_prop_below = 0,
#                                              data_label_decimal_symbol = ".",
#                                              return_raw = FALSE,
#                                              totals = FALSE,
#                                              descend = FALSE,
#                                              require_common_categories = TRUE))
#
# # Test 15: Test when sort_by is an integer larger than the number of unique categories
#   testthat::expect_error(embed_cat_text_html(data = ex_survey1,
#                                              dep = matches("e_"),
#
#                                              data_label = "percentage",
#                                              contents = "value_max",
#                                              sort_by = 5,
#                                              digits = 0,
#                                              n_top_bottom = 1,
#                                              showNA = "never",
#                                              hide_label_if_prop_below = 0,
#                                              data_label_decimal_symbol = ".",
#                                              return_raw = FALSE,
#                                              totals = FALSE,
#                                              descend = FALSE,
#                                              require_common_categories = TRUE))
#
# # Test 16: Test when sort_by is a numeric value (not an integer)
#   testthat::expect_error(embed_cat_text_html(data = ex_survey1,
#                                              dep = matches("e_"),
#
#                                              data_label = "percentage",
#                                              contents = "value_max",
#                                              sort_by = 1.5,
#                                              digits=0,
#                                              n_top_bottom = 1,
#                                              showNA = "never",
#                                              hide_label_if_prop_below = 0,
#                                              data_label_decimal_symbol = ".",
#                                              return_raw = FALSE,
#                                              totals = FALSE,
#                                              descend = FALSE,
#                                              require_common_categories = TRUE))
# })
