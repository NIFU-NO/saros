testthat::test_that("girafe reverses checkbox categories when colour_2nd_binary_cat is set", {
  testthat::skip_on_cran()

  # Create a checkbox plot with "Selected" and "Not selected"
  test_data <- data.frame(
    var1 = rep("X", 4),
    .category = factor(
      c("Selected", "Not selected", "Selected", "Not selected"),
      levels = c("Selected", "Not selected")
    )
  )

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = var1, fill = .category)) +
    ggplot2::geom_bar()

  # Apply girafe with checkbox and colour_2nd_binary_cat
  result <- saros::girafe(
    p,
    interactive = FALSE,
    checked = "Selected",
    not_checked = "Not selected",
    colour_2nd_binary_cat = "#ffffff",
    palette_codes = list(c("red", "blue"))
  )

  # Should have reversed to put not_checked second
  new_levels <- levels(result$data$.category)
  testthat::expect_equal(new_levels, c("Not selected", "Selected"))
})

testthat::test_that("girafe checkbox without colour_2nd_binary_cat maintains normal order", {
  testthat::skip_on_cran()

  # Create a checkbox plot
  test_data <- data.frame(
    var1 = rep("X", 4),
    .category = factor(
      c("Selected", "Not selected", "Selected", "Not selected"),
      levels = c("Selected", "Not selected")
    )
  )

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = var1, fill = .category)) +
    ggplot2::geom_bar()

  # Apply girafe with checkbox but NO colour_2nd_binary_cat
  result <- saros::girafe(
    p,
    interactive = FALSE,
    checked = "Selected",
    not_checked = "Not selected",
    palette_codes = list(c("red", "blue"))
  )

  # Should maintain normal order: checked, not_checked
  new_levels <- levels(result$data$.category)
  testthat::expect_equal(new_levels, c("Selected", "Not selected"))
})

testthat::test_that("girafe ignores colour_2nd_binary_cat without checkbox criteria", {
  testthat::skip_on_cran()

  # Create a plot with 2 categories that DON'T match checkbox criteria
  test_data <- data.frame(
    var1 = factor(rep(c("X", "Y"), each = 2)),
    .category = factor(rep(c("A", "B"), each = 2))
  )

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = var1, fill = .category)) +
    ggplot2::geom_bar()

  original_levels <- levels(p$data$.category)

  # Apply girafe with colour_2nd_binary_cat but no checkbox match
  result <- saros::girafe(
    p,
    interactive = FALSE,
    colour_2nd_binary_cat = "#ffffff",
    palette_codes = list(c("red", "blue"))
  )

  # Should NOT reverse - colour_2nd_binary_cat is ignored without checkbox
  new_levels <- levels(result$data$.category)
  testthat::expect_equal(new_levels, original_levels)
})
