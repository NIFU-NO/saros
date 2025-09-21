# Sample data for testing
data_summary <- tibble::tibble(
  .variable_label = factor(c("A", "B", "C")),
  .count_per_dep = c(10, 20, 30),
  .count_per_indep_group = c(5, 15, 25),
  indep_var = factor(c("X", "Y", "Z"))
)

# Tests
testthat::test_that("No changes when all flags are FALSE", {
  result <- saros:::add_n_to_label(data_summary)
  testthat::expect_equal(result, data_summary)
})

testthat::test_that("Add N to dependent variable labels", {
  result <- saros:::add_n_to_label(data_summary, add_n_to_dep_label = TRUE)
  expected_labels <- c("A (N = 10)", "B (N = 20)", "C (N = 30)")
  testthat::expect_equal(levels(result$.variable_label), expected_labels)
})

testthat::test_that("Custom prefix and suffix for dep labels", {
  result <- saros:::add_n_to_label(
    data_summary,
    add_n_to_dep_label = TRUE,
    add_n_to_dep_label_prefix = " [Count=",
    add_n_to_dep_label_suffix = "]"
  )
  expected_labels <- c("A [Count=10]", "B [Count=20]", "C [Count=30]")
  testthat::expect_equal(levels(result$.variable_label), expected_labels)
})

testthat::test_that("Add N to independent variable labels", {
  result <- saros:::add_n_to_label(data_summary, add_n_to_indep_label = TRUE)
  expected_indep <- c("X (N = 5)", "Y (N = 15)", "Z (N = 25)")
  testthat::expect_equal(levels(result$indep_var), expected_indep)
})

testthat::test_that("Custom prefix and suffix for indep labels", {
  result <- saros:::add_n_to_label(
    data_summary,
    add_n_to_indep_label = TRUE,
    add_n_to_indep_label_prefix = " [Total=",
    add_n_to_indep_label_suffix = "]"
  )
  expected_indep <- c("X [Total=5]", "Y [Total=15]", "Z [Total=25]")
  testthat::expect_equal(levels(result$indep_var), expected_indep)
})

testthat::test_that("Add N to general labels with defaults", {
  result <- saros:::add_n_to_label(data_summary, add_n_to_dep_label = TRUE)
  expected_labels <- c("A (N = 10)", "B (N = 20)", "C (N = 30)")
  testthat::expect_equal(levels(result$.variable_label), expected_labels)
})

testthat::test_that("Combine N for dep and indep labels", {
  result <- saros:::add_n_to_label(
    data_summary,
    add_n_to_dep_label = TRUE,
    add_n_to_indep_label = TRUE
  )
  expected_labels <- c("A (N = 10)", "B (N = 20)", "C (N = 30)")
  expected_indep <- c("X (N = 5)", "Y (N = 15)", "Z (N = 25)")
  testthat::expect_equal(levels(result$.variable_label), expected_labels)
  testthat::expect_equal(levels(result$indep_var), expected_indep)
})

testthat::test_that("Works with empty prefix and suffix", {
  result <- saros:::add_n_to_label(
    data_summary,
    add_n_to_dep_label = TRUE,
    add_n_to_dep_label_prefix = "",
    add_n_to_dep_label_suffix = ""
  )
  expected_labels <- c("A10", "B20", "C30")
  testthat::expect_equal(levels(result$.variable_label), expected_labels)
})

testthat::test_that("Handles missing count columns gracefully", {
  incomplete_data <- data_summary |> dplyr::mutate(.count_per_dep = NA)
  result <- saros:::add_n_to_label(incomplete_data, add_n_to_dep_label = TRUE)
  expected_labels <- c("A (N = NA)", "B (N = NA)", "C (N = NA)")
  testthat::expect_equal(as.character(result$.variable_label), expected_labels)
})

testthat::test_that("Preserves other columns unchanged", {
  result <- saros:::add_n_to_label(data_summary, add_n_to_dep_label = TRUE)
  testthat::expect_equal(result$.count_per_dep, data_summary$.count_per_dep)
  testthat::expect_equal(result$indep_var, data_summary$indep_var)
})
