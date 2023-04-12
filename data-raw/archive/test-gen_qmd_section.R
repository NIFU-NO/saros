testthat::test_that("gen_qmd_section produces valid output", {
  # Prepare arguments
  data_overview <-
    ex_survey_ch_overview %>%
    saros::refine_data_overview(
      data = ex_survey1,
      label_separator = " - ",
      name_separator = "_",
      group_by = c("chapter", "col_group"))

  data_overview_section <-
    data_overview %>%
    dplyr::filter(.data$col_group == 3)

  index <-
    data_overview_section %>%
    dplyr::slice(1) %>%
    glue::glue_data("{chapter}_{col_group}")
  test_path <- tempdir()

  ##############
  # Test 4: Successful test with creating element as needed
  ch_folder <- if(quarto::quarto_version() < 1.3) "2_Ambivalence" else "2 Ambivalence"

  element_list <-
    list(uni_cat_plot_html =
           lst_saros_elements(data_overview = data_overview_section,
                              element_name = "uni_cat_plot_html",
                              data = ex_survey1))

  testthat::expect_equal(object =
                           saros:::gen_qmd_section(
                             data_overview_section = data_overview_section,
                             elements = element_list,
                             path = test_path),
                         expected = paste(sep="\n",
                                          "## 2 Ambivalence_3 - Do you consent to the following?",
                                          "",
                                          "::: {.content-visible when-format=\"html\"}",
                                          "```{r}",
                                          paste0("uni_cat_plot_html_2_Ambivalence_3 <- readRDS(\"", ch_folder, "/uni_cat_plot_html/2 Ambivalence_3.rds\")"),
                                          "plot(uni_cat_plot_html_2_Ambivalence_3)",
                                          "",
                                          "``` ",
                                          ":::",
                                          "")
  )

  # Test 5: Successful test with valid input

  testthat::expect_equal(object =
                           saros:::gen_qmd_section(
                             data_overview_section = data_overview_section,
                             elements = element_list,
                             path = test_path),
                         expected = paste(sep="\n",
                                          "## 2 Ambivalence_3 - Do you consent to the following?",
                                          "",
                                          "::: {.content-visible when-format=\"html\"}",
                                          "```{r}",
                                          paste0("uni_cat_plot_html_2_Ambivalence_3 <- readRDS(\"", ch_folder, "/uni_cat_plot_html/2 Ambivalence_3.rds\")"),
                                          "plot(uni_cat_plot_html_2_Ambivalence_3)",
                                          "",
                                          "``` ",
                                          ":::",
                                          "")
  )



  # Test 6: Successful test with no elements, should return only h2 heading
  testthat::expect_equal(
    saros:::gen_qmd_section(
      data_overview_section = data_overview_section,
      elements = NULL,
      path = test_path),
    expected = "## 2 Ambivalence_3 - Do you consent to the following?\n\n")


  # Test 9: Successful test with custom glue_index_string
  index <-
    data_overview_section %>%
    dplyr::slice(1) %>%
    glue::glue_data("{chapter}_{col_group}")
  testthat::expect_equal(
    saros:::gen_qmd_section(
      data_overview_section = data_overview_section,
      path = test_path,
      elements = element_list,
      glue_index_string = "{chapter}_{col_group}"),
    expected = paste(sep="\n",
                     "## 2 Ambivalence_3 - Do you consent to the following?",
                     "",
                     "::: {.content-visible when-format=\"html\"}",
                     "```{r}",
                     paste0("uni_cat_plot_html_2_Ambivalence_3 <- readRDS(\"", ch_folder, "/uni_cat_plot_html/2 Ambivalence_3.rds\")"),
                     "plot(uni_cat_plot_html_2_Ambivalence_3)",
                     "",
                     "``` ",
                     ":::",
                     "")
  )

})



testthat::test_that("gen_qmd_section produces INVALID output", {
  # Test 1: Failing test with multiple designated_role values

  data_overview <-
    ex_survey_ch_overview %>%
    saros::refine_data_overview(data = ex_survey1,
                                label_separator = " - ",
                                name_separator = "_",
                                group_by = c("chapter", "col_group"))
  data_overview_section <-
    data_overview %>%
    dplyr::filter(.data$col_group == 3)

  data_overview_section_2 <- data.frame(designated_role = c("indep", "dep"),
                                        designated_type = "cat", label_prefix = "How do you feel about...")
  index <-
    data_overview_section %>%
    dplyr::slice(1) %>%
    glue::glue_data("{designated_role}_{col_group}_{name_prefix}_{designated_type}")
  test_path <- tempdir()

  testthat::expect_error(
    saros:::gen_qmd_section(
      data_overview_section = data_overview_section_2,
      path = test_path),
    regexp = "Multiple designated_role values detected"
  )

  # Test 2: Failing test with multiple designated_type values
  data_overview_section_3 <- data.frame(designated_role = "indep",
                                        designated_type = c("cat", "int"),
                                        label_prefix = "How do you feel about...")
  testthat::expect_error(
    saros:::gen_qmd_section(
      data_overview_section = data_overview_section_3,
      path = test_path),
    "Multiple designated_type values detected"
  )

  # Test 3: Failing test with multiple label_prefix values
  data_overview_section_4 <-
    data.frame(designated_role = "indep",
               designated_type = "cat",
               label_prefix = c("How do you feel about...", "prefix2"))
  testthat::expect_error(
    saros:::gen_qmd_section(
      data_overview_section = data_overview_section_4,
      path = test_path),
    "Multiple label_prefix values detected"
  )


})
