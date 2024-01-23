testthat::test_that("argument_validation_and_insertion function", {

  args <-
    formals(saros::draft_report)
  args <-
     args[!names(args) %in% saros:::.saros.env$ignore_args] |>
    lapply(FUN = eval) |>
    utils::modifyList(keep.null = TRUE,
                      val =
                        list(data = data.frame(a = 1),
                             chapter_overview = data.frame(chapter = "Chapter 1"),
                             path = "test"))

   args |>
    saros:::argument_validation_and_insertion() |>
     testthat::expect_silent()

    # Test Case 2: Invalid argument
   args |>
     utils::modifyList(keep.null = TRUE,
                       val =
                         list(invalid_arg = 2)) |>
     saros:::argument_validation_and_insertion() |>
     testthat::expect_error(regexp = "not recognized valid arguments.")

    # Test Case 3: Missing required argument
   args |>
     utils::modifyList(keep.null = TRUE,
                       val =
                         list(data = NULL)) |>
     saros:::argument_validation_and_insertion() |>
     testthat::expect_warning(regexp = "`data` is invalid")

    # Test Case 4: Invalid data type for 'data'
   args |>
     utils::modifyList(keep.null = TRUE,
                       val =
                         list(data = "invalid_data")) |>
     saros:::argument_validation_and_insertion() |>
     testthat::expect_warning(regexp = "`data` is invalid")

   # Test Case 5: 'element_names' not a character vector
   args |>
     utils::modifyList(keep.null = TRUE,
                       val = list(element_names = 123)) |>
     saros:::argument_validation_and_insertion() |>
     testthat::expect_warning(regexp = "`element_names` is invalid \\(it is a number, and specified as 123\\)")

   # Test Case 15: 'element_names' contains non-existent elements
   args |>
     utils::modifyList(keep.null = TRUE,
                       val = list(element_names = c("nonexistent_element"))) |>
     saros:::argument_validation_and_insertion() |>
     testthat::expect_warning(regexp = "`element_names` is invalid \\(it is a string, and specified as nonexistent_element\\).")

   # Test Case 16: 'showNA' not one of the allowed values
   args |>
     utils::modifyList(keep.null = TRUE,
                       val = list(showNA = "invalid_value")) |>
     saros:::argument_validation_and_insertion() |>
     testthat::expect_warning(regexp = "`showNA` is invalid \\(it is a string, and specified as invalid_value\\)\\. Using default: never, always, and ifany")

   args |>
     utils::modifyList(keep.null = TRUE,
                       val = list(descend = NA)) |>
     saros:::argument_validation_and_insertion() |>
     testthat::expect_warning("`descend` is invalid \\(it is `NA`, and specified as NA\\)\\. Using default: TRUE")
  })
