testthat::test_that("build_custom_palette handles unnamed elements as NA colors", {
  # Create a simple palette function using build_custom_palette
  palette_codes <- list(
    c("red", "blue", "green") # Last resort palette
  )

  # Create priority palette with unnamed element for NA
  priority_palette_codes <- c("Yes" = "green", "grey") # Unnamed grey for NA

  # Test with levels including "NA"
  lvls <- c("Yes", "No", "NA")

  custom_pal <- saros:::build_custom_palette(
    palette_codes = palette_codes,
    fct_levels = lvls,
    priority_palette_codes = priority_palette_codes
  )

  result <- custom_pal(n = 3, lvls = lvls)

  # Check that we have colors for all levels
  testthat::expect_equal(length(result), 3)
  testthat::expect_true(all(lvls %in% names(result)))

  # Check that "Yes" got the priority color
  testthat::expect_equal(unname(result["Yes"]), "green")

  # Check that "NA" got assigned a color (the unnamed element from priority)
  testthat::expect_true(!is.na(result["NA"]))
  testthat::expect_equal(unname(result["NA"]), "grey")
})

testthat::test_that("build_custom_palette works with showNA explicit levels", {
  # Simulate the scenario from issue #500
  palette_codes <- list(
    c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
  )

  priority_palette_codes <- c("NA" = "grey")

  lvls <- c(
    "Strongly disagree",
    "Disagree",
    "Neither agree nor disagree",
    "Agree",
    "Strongly agree",
    "NA"
  )

  custom_pal <- saros:::build_custom_palette(
    palette_codes = palette_codes,
    fct_levels = lvls,
    priority_palette_codes = priority_palette_codes
  )

  result <- custom_pal(n = length(lvls), lvls = lvls)

  # All levels should have colors
  testthat::expect_equal(length(result), length(lvls))
  testthat::expect_true(all(lvls %in% names(result)))

  # NA should have the priority color
  testthat::expect_equal(unname(result["NA"]), "grey")
})

testthat::test_that("build_custom_palette handles all named elements normally", {
  # When all elements are named, should work as before
  palette_codes <- list(
    c("red", "blue", "green")
  )

  priority_palette_codes <- c("Yes" = "green", "No" = "red")

  lvls <- c("Yes", "No")

  custom_pal <- saros:::build_custom_palette(
    palette_codes = palette_codes,
    fct_levels = lvls,
    priority_palette_codes = priority_palette_codes
  )

  result <- custom_pal(n = 2, lvls = lvls)

  testthat::expect_equal(length(result), 2)
  testthat::expect_equal(unname(result["Yes"]), "green")
  testthat::expect_equal(unname(result["No"]), "red")
})

testthat::test_that("girafe handles factor() fill mappings correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggiraph")

  library(ggplot2)

  # Create plot with factor() in the fill aesthetic (not a bare column name)
  plot <- ggplot(mtcars, aes(x = hp, y = mpg, fill = factor(cyl))) +
    geom_point()

  # Custom palette
  custom_palette <- list(c("red", "blue", "green"))

  # Should not error and should return a ggplot object when interactive=FALSE
  result <- girafe(
    plot,
    palette_codes = custom_palette,
    interactive = FALSE
  )

  expect_true(ggplot2::is_ggplot(result))

  # Verify get_fill_levels extracts levels correctly from factor() mapping
  fill_levels <- saros:::get_fill_levels(plot)
  expect_equal(length(fill_levels), 3)
  expect_equal(sort(fill_levels), c("4", "6", "8"))
})
