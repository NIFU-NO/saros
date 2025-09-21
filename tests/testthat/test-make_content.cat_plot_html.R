testthat::test_that("show.legend works with unused categories", {
  vdiffr::expect_doppelganger(title = "Unused categories in legend", fig = {
    saros::ex_survey |>
      dplyr::filter(dplyr::if_all(p_1:p_4, ~ .x != "Strongly agree")) |>
      saros::makeme(dep = p_1:p_4, indep = x1_sex)
  })
})
