testthat::test_that("keep_cols returns only the target group", {
  data <- data.frame(dep1 = c(1, 2, 3), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target"
  )
  testthat::expect_equal(result$data$mesos_var, c("A", "A"))
})

testthat::test_that("keep_cols returns only the others group", {
  data <- data.frame(dep1 = c(1, 2, 3), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "others"
  )
  testthat::expect_equal(result$data$mesos_var, "B")
})

testthat::test_that("keep_cols returns all data for 'all' group", {
  data <- data.frame(dep1 = c(1, 2, 3), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "all"
  )
  testthat::expect_equal(result$data, data)
})

testthat::test_that("keep_cols retains columns if hide_for_crowd_if_all_na is FALSE", {
  data <- data.frame(dep1 = c(NA, NA, 1), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target",
    hide_for_crowd_if_all_na = FALSE
  )
  testthat::expect_equal(result$omitted_vars, character(0))
})


testthat::test_that("keep_cols omits all NA columns if hide_for_crowd_if_all_na is TRUE", {
  data <- data.frame(dep1 = c(NA, NA, NA), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target",
    hide_for_crowd_if_all_na = TRUE
  )
  testthat::expect_equal(result$omitted_vars, "dep1")
})


testthat::test_that("keep_cols omits columns if valid N is below threshold", {
  data <- data.frame(dep1 = c(1, 2, 3, NA), mesos_var = c("A", "B", "A", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target",
    hide_for_crowd_if_valid_n_below = 4
  )
  testthat::expect_equal(result$omitted_vars, "dep1")
})

testthat::test_that("keep_cols omits columns if category count is below threshold", {
  data <- data.frame(dep1 = c(1, 1, 2), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target",
    hide_for_crowd_if_category_k_below = 3
  )
  testthat::expect_equal(result$omitted_vars, "dep1")
})

testthat::test_that("keep_cols omits columns if category N is below threshold", {
  data <- data.frame(dep1 = c(1, 1, 2), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target",
    hide_for_crowd_if_category_n_below = 2
  )
  testthat::expect_equal(result$omitted_vars, "dep1")
})

testthat::test_that("keep_cols omits columns if cell N is below threshold", {
  data <- data.frame(
    dep1 = c(1, 1, 2),
    indep1 = c(1, 1, 2),
    mesos_var = c("A", "B", "A")
  )
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    indep = "indep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target",
    hide_for_crowd_if_cell_n_below = 2
  )
  testthat::expect_equal(result$omitted_vars, "dep1")
})


testthat::test_that("keep_cols omits columns if cell N is above threshold", {
  data <- data.frame(dep1 = c(1, 1, 2, 2), indep1 = c(1, 1, 2, 2))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    indep = "indep1",
    hide_for_crowd_if_cell_n_below = 2
  )
  testthat::expect_equal(result$omitted_vars, character())
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    indep = "indep1",
    hide_for_crowd_if_cell_n_below = 3
  )
  testthat::expect_equal(result$omitted_vars, "dep1")
})


testthat::test_that("keep_cols omits columns if hidden for crowd vars is specified", {
  data <- data.frame(dep1 = c(1, 2, 3), mesos_var = c("A", "B", "A"))
  result <- saros:::keep_cols(
    data,
    dep = "dep1",
    mesos_var = "mesos_var",
    mesos_group = "A",
    crowd = "target",
    hide_for_all_crowds_if_hidden_for_crowd_vars = "dep1"
  )
  testthat::expect_equal(result$omitted_vars, "dep1")
})
