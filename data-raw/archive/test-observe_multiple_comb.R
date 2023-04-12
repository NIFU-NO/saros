testthat::test_that("test_multiple_comb t_test one-sample mtcars", {
  library(dplyr)
  x <-
    mtcars %>%
    test_multiple_comb(response = matches("mpg|disp"),
                       mu = 0,
                       alternative = "two-sided",
                       conf_level = .95,
                       test = "t"
    )
  testthat::expect_equal(dim(x), c(2, 10))
  testthat::expect_equal(as.numeric(x[2,2]), 10.5306904)
})

testthat::test_that("test_multiple_comb t_test two-sample mtcars", {
  library(dplyr)
  x <-
    mtcars %>%
    test_multiple_comb(response = matches("mpg|disp"),
                       explanatory = matches("vs"),
                       mu = 0,
                       order = c("1", "0"),
                       alternative = "two-sided",
                       conf_level = .95,
                       test = "t"
    )
  testthat::expect_equal(dim(x), c(2, 13))
})

# testthat::test_that("test_multiple_comb t_test two-sample cyl mtcars", {
#   library(dplyr)
#   x <-
#     mtcars %>%
#     test_multiple_comb(response = matches("mpg"),
#                        explanatory = matches("cyl"),
#                        mu = 0,
#                        alternative = "two-sided",
#                        conf_level = .95,
#                        test = "t"
#     )
#   testthat::expect_equal(dim(x), c(2, 13))
# })


testthat::test_that("test_multiple_comb t_test two-sample ex_survey1", {
  library(dplyr)
  x <-
    ex_survey1 %>%
    test_multiple_comb(response = matches("c_[1-2]"),
                       explanatory = matches("x1_sex"),
                       mu = 0,
                       order = c("Males", "Females"),
                       alternative = "two-sided",
                       conf_level = .95,
                       test = "t"
    )
  testthat::expect_equal(dim(x), c(2, 13))
})
################################################################################

testthat::test_that("test_multiple_comb chisq one-sample mtcars", {
  library(dplyr)
  x <-
    mtcars %>%
    mutate(across(matches("vs|am"), ~as.factor(.x))) %>%
    test_multiple_comb(response = matches("vs|am"),
                       alternative = "two-sided",
                       conf_level = .95,
                       test = "chisq",
                       p = c(.5, .5)
    )
  testthat::expect_equal(dim(x), c(2, 4))
})


testthat::test_that("test_multiple_comb two-sample chisq mtcars", {
  library(dplyr)
  x <-
    mtcars %>%
    mutate(across(matches("carb|am|cyl"), ~as.factor(.x))) %>%
    test_multiple_comb(response = matches("carb|am"),
                       explanatory = matches("cyl"),
                       mu = 0,
                       order = c("1", "0"),
                       alternative = "two-sided",
                       conf_level = .95,
                       test = "chisq"
    ) %>%
    suppressWarnings()
  testthat::expect_equal(dim(x), c(2, 5))
})


testthat::test_that("test_multiple_comb chisq_test ex_survey1", {
  library(dplyr)
  x <-
    ex_survey1 %>%
    test_multiple_comb(response = matches("a_[1-9]"),
                       explanatory = matches("x1_sex|x2_human"),
                       mu = 0,
                       order = c("Males", "Females"),
                       alternative = "two-sided",
                       conf_level = .95,
                       test = "chisq"
    )
  testthat::expect_equal(dim(x), c(2*9, 5))
})
