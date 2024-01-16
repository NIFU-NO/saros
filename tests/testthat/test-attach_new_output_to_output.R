testthat::test_that("attach_new_output_to_output", {
  saros:::attach_new_output_to_output(new_out = "New output",
                                      output = "",
                                      level = 4,
                                      grouped_data = tibble::tibble(a=1, b=1, c=1, d=1),
                                      heading_line = "# New heading") |>
  testthat::expect_equal("# New heading\nNew output")

  saros:::attach_new_output_to_output(new_out = "New output",
                                      output = "",
                                      level = 4,
                                      grouped_data = tibble::tibble(a=1, b=1, c=1, .element_name=1),
                                      heading_line = "# New heading") |>
    testthat::expect_equal("New output")

  saros:::attach_new_output_to_output(new_out = "new",
                                      output = "",
                                      level = 4,
                                      grouped_data = tibble::tibble(a=1, b=1, c=1, d=1),
                                      heading_line = "# New heading") |>
    testthat::expect_equal("new")

  saros:::attach_new_output_to_output(new_out = "new",
                                      output = "existing",
                                      level = 4,
                                      grouped_data = tibble::tibble(a=1, b=1, c=1, d=1),
                                      heading_line = "# New heading") |>
    testthat::expect_equal("existing\nnew")

  saros:::attach_new_output_to_output(new_out = "New output",
                                      output = "existing",
                                      level = 2,
                                      grouped_data = tibble::tibble(a=1, b=1, c=1, d=1),
                                      heading_line = "# New heading") |>
    testthat::expect_equal("existing\nNew output")

})
