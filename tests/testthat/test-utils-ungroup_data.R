testthat::test_that("ungroup_data ungrouped regular data.frame", {
  data <- data.frame(a = 1:3, b = 4:6)
  grouped_data <- dplyr::group_by(data, a)
  
  result <- saros:::ungroup_data(grouped_data)
  
  testthat::expect_false(dplyr::is_grouped_df(result))
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("ungroup_data handles already ungrouped data.frame", {
  data <- data.frame(a = 1:3, b = 4:6)
  
  result <- saros:::ungroup_data(data)
  
  testthat::expect_false(dplyr::is_grouped_df(result))
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("ungroup_data handles tibble", {
  data <- dplyr::tibble(a = 1:3, b = 4:6)
  grouped_data <- dplyr::group_by(data, a)
  
  result <- saros:::ungroup_data(grouped_data)
  
  testthat::expect_false(dplyr::is_grouped_df(result))
  testthat::expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
})

testthat::test_that("ungroup_data preserves data content", {
  data <- data.frame(a = c(1, 1, 2), b = c(4, 5, 6))
  grouped_data <- dplyr::group_by(data, a)
  
  result <- saros:::ungroup_data(grouped_data)
  
  testthat::expect_equal(nrow(result), nrow(data))
  testthat::expect_equal(ncol(result), ncol(data))
  testthat::expect_equal(result$a, data$a)
  testthat::expect_equal(result$b, data$b)
})

testthat::test_that("ungroup_data handles multiple grouping variables", {
  data <- data.frame(a = c(1, 1, 2), b = c("x", "y", "x"), c = 1:3)
  grouped_data <- dplyr::group_by(data, a, b)
  
  result <- saros:::ungroup_data(grouped_data)
  
  testthat::expect_false(dplyr::is_grouped_df(result))
  testthat::expect_equal(nrow(result), 3)
})

testthat::test_that("ungroup_data handles survey objects", {
  # Skip this test if srvyr is not available
  testthat::skip_if_not_installed("srvyr")
  
  # Create a simple survey design for testing
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    strata = c("a", "a", "b", "b", "a"),
    weights = c(1, 1, 2, 2, 1)
  )
  
  survey_design <- survey::svydesign(
    ids = ~1,
    strata = ~strata,
    weights = ~weights,
    data = data
  )
  
  survey_srvyr <- srvyr::as_survey_design(survey_design)
  grouped_survey <- srvyr::group_by(survey_srvyr, strata)
  
  result <- saros:::ungroup_data(grouped_survey)
  
  testthat::expect_true(inherits(result, "tbl_svy"))
  # Should be ungrouped - srvyr survey objects have different grouping indicators
  testthat::expect_false(dplyr::is_grouped_df(result))
})

testthat::test_that("ungroup_data handles empty data.frame", {
  data <- data.frame()
  
  result <- saros:::ungroup_data(data)
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 0)
})