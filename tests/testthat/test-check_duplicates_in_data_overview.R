testthat::test_that("check_duplicates_in_chapter_overview", {
  unlist(saros:::check_duplicates_in_chapter_overview(
    data = saros::ex_survey,
    chapter_overview = saros::ex_survey_ch_overview)) |>
    testthat::expect_equal(NULL)
})
