testthat::test_that("make_content.cat_table_html works", {
  result <-
    saros::makeme(
      data = saros::ex_survey,
      dep = p_1:p_4, # indep = x2_human,
      type = "cat_table_html",
      showNA = "never",
      add_n_to_label = TRUE
    )
  testthat::expect_equal(as.character(result$.variable_label[[4]]), "Blue Party (N = 266)")
})

testthat::test_that("make_content.cat_table_html works with NA on both dep and indep", {
  expected_df <-
    tibble::tibble(
      Generation = factor(c("Z", NA), exclude = NULL),
      `F (%)` = c(NA, "50"),
      `M (%)` = c(NA, "50"),
      `NA (%)` = c("100", NA),
      `Total (N)` = c(1L, 2L)
    )
  attr(expected_df$Generation, "label") <- "Generation"
  data.frame(
    a = factor(c("M", "F", NA), exclude = NULL),
    b = factor(c(NA, NA, "Z"), exclude = NULL)
  ) |>
    labelled::set_variable_labels(a = "Gender", b = "Generation") |>
    saros::makeme(dep = a, indep = b, showNA = "never", type = "cat_table_html") |>
    testthat::expect_equal(expected = expected_df)
})


testthat::test_that("make_content.cat_table_html works with all missing variable labels", {
  testthat::expect_warning(
    saros::ex_survey |>
      dplyr::mutate(dplyr::across(a_1:a_3, ~ factor(.x, ordered = TRUE))) |>
      saros::makeme(dep = a_1:a_3, type = "cat_table_html"),
    regexp = "No variable labels found for "
  )

  out <-
    saros::ex_survey |>
    dplyr::mutate(dplyr::across(a_1:a_3, ~ factor(.x, ordered = TRUE))) |>
    saros::makeme(dep = a_1:a_3, type = "cat_table_html") |>
    suppressWarnings()

  out |>
    dim() |>
    testthat::expect_equal(expected = c(3, 5))
})
