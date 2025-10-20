testthat::test_that("make_content.cat_plot_html basic smoke test", {
  out <- saros::makeme(
    data = saros::ex_survey,
    dep = a_1:a_2,
    indep = x1_sex,
    type = "cat_plot_html",
    showNA = "never",
    html_interactive = FALSE, # keep light for CRAN
    descend = FALSE
  )

  # Basic structure checks without heavy rendering
  testthat::expect_true(inherits(out, c("ggplot", "gg")))
})
testthat::test_that("show.legend works with unused categories", {
  vdiffr::expect_doppelganger(title = "Unused categories in legend", fig = {
    saros::ex_survey |>
      dplyr::filter(dplyr::if_all(p_1:p_4, ~ .x != "Strongly agree")) |>
      saros::makeme(
        dep = p_1:p_4,
        indep = x1_sex,
        sort_dep_by = ".variable_position",
        descend = TRUE
      )
  })
})
