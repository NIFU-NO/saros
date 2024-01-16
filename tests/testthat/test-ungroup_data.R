test_that("ungroup_data", {
  x <- saros::ex_survey %>%
    dplyr::group_by(.data$f_uni)
  testthat::expect_false(inherits(saros:::ungroup_data(x), "grouped_df"))
  x <- saros::ex_survey %>%
    srvyr::as_survey_design() %>%
    srvyr::group_by(.data$f_uni)
  testthat::expect_false(inherits(saros:::ungroup_data(x), "grouped_svy"))
})
