# Test for color consistency across mesos groups (Bug fix)
# Issue: When one category is missing from target group, the colour assignment
# becomes inconsistent across target and others plots

test_that("category levels are consistent across crowd groups for cat_plot", {
  skip_on_cran()

  # Create test data with missing category in target group
  test_data <- data.frame(
    mesos = rep(c("Target", "Others"), times = c(30, 70)),
    q1 = factor(
      c(
        sample(c("A", "B"), 30, replace = TRUE), # Target: only A and B
        sample(c("A", "B", "C"), 70, replace = TRUE) # Others: A, B, and C
      ),
      levels = c("A", "B", "C")
    )
  )

  # Add label attribute
  attr(test_data$q1, "label") <- "Question 1"

  # Verify setup: Target missing C, Others has all three
  target_cats <- unique(as.character(test_data$q1[test_data$mesos == "Target"]))
  others_cats <- unique(as.character(test_data$q1[test_data$mesos == "Others"]))

  expect_false("C" %in% target_cats)
  expect_true("C" %in% others_cats)

  # Test the fix
  result <- makeme(
    data = test_data,
    dep = q1,
    type = "cat_plot_html",
    mesos_var = "mesos",
    mesos_group = "Target",
    crowd = c("target", "others"),
    simplify_output = FALSE
  )

  # Check structure
  expect_true(is.list(result))
  expect_equal(length(result), 2)

  # Extract plots
  plot1 <- result[[1]]
  plot2 <- result[[2]]

  expect_false(is.null(plot1))
  expect_false(is.null(plot2))

  # Extract factor levels from both plots
  levels1 <- levels(plot1$data$.category)
  levels2 <- levels(plot2$data$.category)

  # The critical test: both should have the same levels (A, B, C)
  # This ensures consistent color mapping
  expect_equal(levels1, levels2)
  expect_equal(levels1, c("A", "B", "C"))
  expect_equal(levels2, c("A", "B", "C"))
})

test_that("category levels work with multiple dep variables", {
  skip_on_cran()

  # Create test data with different missing patterns
  test_data <- data.frame(
    mesos = rep(c("Group1", "Group2"), times = c(40, 60)),
    q1 = factor(
      c(
        sample(c("Low", "Medium"), 40, replace = TRUE), # Group1: missing High
        sample(c("Low", "Medium", "High"), 60, replace = TRUE)
      ),
      levels = c("Low", "Medium", "High")
    ),
    q2 = factor(
      c(
        sample(c("Low", "Medium", "High"), 40, replace = TRUE),
        sample(c("Medium", "High"), 60, replace = TRUE) # Group2: missing Low
      ),
      levels = c("Low", "Medium", "High")
    )
  )

  # Add labels
  attr(test_data$q1, "label") <- "Question 1"
  attr(test_data$q2, "label") <- "Question 2"

  result <- makeme(
    data = test_data,
    dep = c(q1, q2),
    type = "cat_plot_html",
    mesos_var = "mesos",
    mesos_group = "Group1",
    crowd = c("target", "others"),
    simplify_output = FALSE
  )

  # Extract plots
  plot1 <- result[[1]]
  plot2 <- result[[2]]

  # Both plots should have consistent levels
  levels1 <- levels(plot1$data$.category)
  levels2 <- levels(plot2$data$.category)

  expect_equal(levels1, levels2)
  expect_equal(levels1, c("Low", "Medium", "High"))
})
