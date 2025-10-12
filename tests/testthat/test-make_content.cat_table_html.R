testthat::test_that("make_content.cat_table_html works", {
  result <-
    saros::makeme(
      data = saros::ex_survey,
      dep = p_1:p_4, # indep = x2_human,
      type = "cat_table_html",
      showNA = "never",
      add_n_to_dep_label = TRUE,
      descend = FALSE
    )
  testthat::expect_equal(
    as.character(result$.variable_label[[4]]),
    "Blue Party (N = 266)"
  )
})

testthat::test_that("make_content.cat_table_html works with NA on both dep and indep", {
  expected_df <-
    data.frame()
  data.frame(
    a = factor(c("M", "F", NA), exclude = NULL),
    b = factor(c(NA, NA, "Z"), exclude = NULL)
  ) |>
    labelled::set_variable_labels(a = "Gender", b = "Generation") |>
    saros::makeme(
      dep = a,
      indep = b,
      showNA = "never",
      type = "cat_table_html",
      descend = FALSE
    ) |>
    testthat::expect_equal(expected = expected_df)
})


testthat::test_that("make_content.cat_table_html works with NA on both dep and indep", {
  expected_df <-
    tibble::tibble(
      Generation = factor(
        c("Z", "NA"),
        levels = c("Z", "NA"),
        exclude = NULL,
        ordered = TRUE
      ),
      `M (%)` = c("67", "33"),
      `F (%)` = c(NA, "33"),
      `NA (%)` = c("33", "33"),
      `Total (N)` = c(3L, 3L)
    ) |>
    labelled::set_variable_labels(Generation = "Generation")
  test_data <-
    data.frame(
      a = factor(
        c("M", "F", "M", "M", NA, NA),
        levels = c("M", "F", NA),
        exclude = NULL
      ),
      b = factor(
        c(NA, NA, "Z", "Z", "Z", NA),
        levels = c("Z", NA),
        exclude = NULL,
        ordered = TRUE
      )
    ) |>
    labelled::set_variable_labels(a = "Gender", b = "Generation")

  saros::makeme(
    data = test_data,
    dep = a,
    indep = b,
    showNA = "always",
    type = "cat_table_html",

    descend = FALSE
  ) |>
    testthat::expect_equal(expected = expected_df)
})

testthat::test_that("make_content.cat_table_html works with all missing variable labels", {
  testthat::expect_warning(
    testthat::expect_warning(
      saros::ex_survey |>
        dplyr::mutate(dplyr::across(a_1:a_3, ~ factor(.x, ordered = TRUE))) |>
        saros::makeme(dep = a_1:a_3, type = "cat_table_html", descend = FALSE),
      regexp = "No main question found\\."
    ),
    regexp = "No variable labels found for "
  )

  out <-
    saros::ex_survey |>
    dplyr::mutate(dplyr::across(a_1:a_3, ~ factor(.x, ordered = TRUE))) |>
    saros::makeme(dep = a_1:a_3, type = "cat_table_html", descend = FALSE) |>
    suppressWarnings()

  out |>
    dim() |>
    testthat::expect_equal(expected = c(3, 5))
})
