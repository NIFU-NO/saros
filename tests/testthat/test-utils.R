testthat::test_that("col_to_binaries", {
  suppressMessages(library(dplyr))
  test <-
    ex_survey1 %>%
    col_to_binaries(col = b_3, label_separator = "  -  ") %>%
    pull(`b_3___A bit`)
  testthat::expect_true(object = all(as.vector(test) %in% 0:1))
  testthat::expect_equal(attr(test, "label"),
                         expected = "How much do you like living in - Budapest  -  A bit")
  })
