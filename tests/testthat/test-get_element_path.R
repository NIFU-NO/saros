testthat::test_that("get_element_path", {
  library(dplyr)

  test_path <- tempdir()

  storage_path <- fs::path(test_path, "raw_elements", "opening_text")
  fs::dir_create(storage_path)

  data_overview_section1 <-
    ex_survey_ch_overview %>%
    saros::refine_data_overview(data = ex_survey1,
                                label_separator = " - ", name_separator = "_",
                                group_by = c("chapter", "col_group", "label_prefix")) %>%
    filter(chapter == "2 Ambivalence",
           col_group == 3)
  data_overview_section2 <-
    ex_survey_ch_overview %>%
    saros::refine_data_overview(data = ex_survey1,
                                label_separator = " - ", name_separator = "_",
                                group_by = c("chapter", "designated_type", "col_group")) %>%
    filter(chapter == "2 Ambivalence")



  element_null <- NULL
  element_null_list <- list(NULL)
  element_null_list2 <- list(opening_text = NULL)
  element_null_list3 <- list(opening_text = NULL,
                             uni_cat_plot_html = NULL)


  element_invalid_list1 <- data.frame()
  element_invalid_list2 <-
    list(abcdef = NULL)
  element_list1 <-
    list(opening_text =
           list(A = list("content_A"),
                B = list("content_B")))

  element_list_valid_paths <-
    list(opening_text =
           c(`x2_Ambivalence_3_Do_you_consent_to_the_following_` = fs::path(storage_path, "A.rds"),
             B = fs::path(storage_path, "B.rds")))

  element_list_invalid_paths <-
    list(opening_text =
           c(`x2_Ambivalence_3_Do_you_consent_to_the_following_` = fs::path(storage_path, "C.rds"),
             D = fs::path(storage_path, "D.rds")))

  saveRDS(object = "abc", file = element_list_valid_paths$opening_text["x2_Ambivalence_3_Do_you_consent_to_the_following_"])
  saveRDS(object = "def", file = element_list_valid_paths$opening_text["B"])

  glue_index_string1 <- NULL
  glue_index_string2 <- "{chapter}_{col_group}_{designated_type}"


  #############################################################

  # Test 1: element is NULL
  test_result <-
    saros:::get_element_path(
      elements = element_null,
      path = test_path
    )
  testthat::expect_null(test_result)

  # Test 2: element_contents is NULL
  test_result <-
    saros:::get_element_path(
      data_overview = data_overview_section1,
      elements = element_null_list,
      path = test_path
    )
  testthat::expect_null(test_result)

  # Test 3: element_contents is NULL
  test_result <-
    saros:::get_element_path(
      elements = element_null_list2,
      path = test_path
    )
  testthat::expect_null(test_result)

  # Test 4: element_contents is NULL
  testthat::expect_error(
  test_result <-
    saros:::get_element_path(
      elements = element_null_list3,
      path = test_path
    ),
  regexp = "If not NULL, `elements` must be a list of length 1, not a list")

##################################


  testthat::expect_error(
    object =
    saros:::get_element_path(
      elements = element_invalid_list1,
      path = test_path),
    regexp = "`elements` must be a list, not a data frame")



  testthat::expect_error(
    saros:::get_element_path(
      data_overview = data_overview_section1,
      elements = element_list_invalid_paths,
      path = test_path
    ), regexp = "Can't find file")



  testthat::expect_error(
    saros:::get_element_path(
      data_overview = data_overview_section2,
      elements = element_list_valid_paths,
      path = test_path
    ), regexp = "`data_overview` contains multiple grouping variables: `x2_Ambivalence_cat_1`,")



  # Test 7: Invalid element_contents type
  testthat::expect_error(
    saros:::get_element_path(
      data_overview = data_overview_section1,
      elements = list(opening_text = 42),
      path = test_path
    ),
    regexp = "`element_contents_names` must be a character vector, not NULL"
  )

  # Test 7: Invalid element_contents type
  testthat::expect_error(
    saros:::get_element_path(
      elements = list(opening_text = c(x2_Ambivalence_3_Do_you_consent_to_the_following_ = 42)),
      data_overview = data_overview_section1,
      path = test_path
    ),
    regexp = "`c\\(x2_Ambivalence_3_Do_you_consent_to_the_following_ = 42\\)` must be"
  )

  element_list_valid_paths_multiple <-
    list(opening_text =
           c(`x2_Ambivalence_3_Do_you_consent_to_the_following_` = fs::path(storage_path, "A.rds"),
             B = fs::path(storage_path, "B.rds")),
         uni_cat_plot_html =
           c(x2_Ambivalence_3_Do_you_consent_to_the_following_ = fs::path(storage_path, "A.rds"),
             B = fs::path(storage_path, "B.rds")))

  testthat::expect_error(
    saros:::get_element_path(
      data_overview = data_overview_section1,
      elements = element_list_valid_paths_multiple,
      path = test_path
    ),
    regexp = "If not NULL, `elements` must be a list of length 1, not a list")


  ##########################################################################


  # Test 4: element_contents is a named list

  testthat::expect_equal(
    saros:::get_element_path(
      data_overview = data_overview_section1,
      elements = element_list_valid_paths,
      path = test_path
    ),
    expected = paste(sep="\n",
      "```{r}",
      "opening_text_x2_Ambivalence_3_Do_you_consent_to_the_following_ <- ",
      "  readRDS(\"2 Ambivalence/opening_text/x2_Ambivalence_3_Do_you_consent_to_the_following_.rds\")",
      "cat(opening_text_x2_Ambivalence_3_Do_you_consent_to_the_following_)",
      "``` ",
      ""))


  testthat::expect_equal(
    saros:::get_element_path(
      data_overview = data_overview_section1,
      elements = list(
        uni_cat_plot_html =
                lst_saros_elements(
                    data = ex_survey1,
                    data_overview = data_overview_section1,
                    element_name = "uni_cat_plot_html")),
      path = test_path
    ),
    expected = paste(sep = "\n",
                     "::: {.content-visible when-format=\"html\"}",
                     "```{r}",
                     "uni_cat_plot_html_x2_Ambivalence_3_Do_you_consent_to_the_following_ <- ",
                     "  readRDS(\"2 Ambivalence/uni_cat_plot_html/x2_Ambivalence_3_Do_you_consent_to_the_following_.rds\")",
                     "(uni_cat_plot_html_x2_Ambivalence_3_Do_you_consent_to_the_following_)",
                     "``` ",
                     ":::",
                     ""))



  testthat::expect_equal(
    saros:::get_element_path(
      data_overview = data_overview_section1,
      elements = list(
        uni_cat_plot_html =
          lst_saros_elements(
            data = ex_survey1,
            data_overview = data_overview_section1 %>% group_by(chapter, col_group),
            element_name = "uni_cat_plot_html")),
      glue_index_string = "{chapter}_{col_group}",
      path = test_path
    ),
    expected = paste(sep = "\n",
                     "::: {.content-visible when-format=\"html\"}",
                     "```{r}",
                     "uni_cat_plot_html_x2_Ambivalence_3 <- ",
                     "  readRDS(\"2 Ambivalence/uni_cat_plot_html/x2_Ambivalence_3.rds\")",
                     "(uni_cat_plot_html_x2_Ambivalence_3)",
                     "``` ",
                     ":::",
                     ""))





  unlink(test_path)
})

