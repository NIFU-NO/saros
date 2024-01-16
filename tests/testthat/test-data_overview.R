testthat::test_that("eval_cols", {
  x <-
    saros:::eval_cols(x = c("x1_sex, x2_human",
                            "matches('b_')"),
              data = saros::ex_survey)
  testthat::expect_equal(lengths(x), c(2, 3))
})

testthat::test_that("look_for_extended", {
  x <-
    saros:::look_for_extended(data = saros::ex_survey,
                              cols = colnames(saros::ex_survey),
                              label_separator = " - ",
                              name_separator = "_")
  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_equal(dim(x), c(32, 9))
  testthat::expect_contains(names(x), c(".variable_name", ".variable_name_prefix", ".variable_name_suffix",
                                        ".variable_label", ".variable_label_prefix", ".variable_label_suffix",
                                        ".variable_type", ".variable_group_id"))
  x <-
    saros:::look_for_extended(data = saros::ex_survey,
                              cols = colnames(saros::ex_survey),
                              name_separator = "_")
  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_equal(dim(x), c(32, 9))
  testthat::expect_contains(names(x), c(".variable_name", ".variable_name_prefix", ".variable_name_suffix",
                                        ".variable_label", ".variable_label_prefix", ".variable_label_suffix",
                                        ".variable_type", ".variable_group_id"))
  x <-
    saros:::look_for_extended(data = saros::ex_survey,
                              cols = colnames(saros::ex_survey),
                              label_separator = " - ")
  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_equal(dim(x), c(32, 9))
  testthat::expect_contains(names(x), c(".variable_name", ".variable_name_prefix", ".variable_name_suffix",
                                        ".variable_label", ".variable_label_prefix", ".variable_label_suffix",
                                        ".variable_type", ".variable_group_id"))
  x <-
    saros:::look_for_extended(data = saros::ex_survey,
                              cols = paste0("b_", 1:3))
  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_equal(dim(x), c(3, 9))
  testthat::expect_contains(names(x), c(".variable_name", ".variable_name_prefix", ".variable_name_suffix",
                                        ".variable_label", ".variable_label_prefix", ".variable_label_suffix",
                                        ".variable_type", ".variable_group_id"))

})

testthat::test_that("validate_labels", {
    saros:::look_for_extended(data = saros::ex_survey,
                              cols = paste0("b_", 1:3),
                              label_separator = " - ") |>
    dplyr::mutate(.variable_label_suffix = c("Bejing", NA, "Budapest")) |>
    saros:::validate_labels() |>
    dplyr::pull(.variable_label_suffix) |>
    testthat::expect_equal(c("Bejing", "b_2", "Budapest"))
})


testthat::test_that("add_element_names", {
  saros::ex_survey_ch_overview |>
    dplyr::mutate(.variable_name_dep = dep) |>
  saros:::add_element_names(element_names = c("uni_cat_prop_plot", "uni_cat_table")) |>
    dim() |>
    testthat::expect_equal(c(10, 7))
})


testthat::test_that("refine_chapter_overview", {
  x <-
    saros:::refine_chapter_overview(chapter_overview = saros::ex_survey_ch_overview,
                                    data = saros::ex_survey,
                                    label_separator = " - ",
                                    name_separator = "_",
                                    element_names = "uni_chr_table")
  testthat::expect_equal(dim(x), c(26, 29))
})
