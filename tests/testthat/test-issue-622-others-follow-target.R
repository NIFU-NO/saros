# Regression guard for #622: with `crowd = c("target", "others")` a variable
# hidden for the target -- too few respondents, a single category used, all
# NA -- was still shown for the others, unless
# `hide_for_all_crowds_if_hidden_for_crowd = "target"` had been set. The
# others are the target's complement; without the target's result there is
# nothing for them to be compared with, so they now follow the target by
# themselves. `hide_for_all_crowds_if_hidden_for_crowd` is still what extends
# the rule to the `all` crowd.

mesos_data <- function() {
  data <- saros::ex_survey
  data$f_uni <- as.character(data$f_uni)
  data
}

crowd_tables <- function(data, ..., mesos_group = "Uni of A") {
  makeme(
    data = data,
    type = "cat_table_html",
    mesos_var = "f_uni",
    mesos_group = mesos_group,
    simplify_output = FALSE,
    ...
  )
}

# b_1, b_2 and b_3 are labelled by city: Bejing, Brussels and Budapest
cities <- function(tbl) {
  sort(sub(".* - ", "", as.character(tbl[[".variable_label"]])))
}

testthat::test_that("a variable hidden for the target is hidden for the others", {
  data <- mesos_data()
  # b_2 is all NA for the target, which therefore cannot show it
  data$b_2[data$f_uni == "Uni of A"] <- NA

  out <- crowd_tables(data, dep = b_1:b_3, crowd = c("target", "others"))

  testthat::expect_named(out, c("Target", "Others"))
  testthat::expect_equal(cities(out$Target), c("Bejing", "Budapest"))
  testthat::expect_equal(cities(out$Others), c("Bejing", "Budapest"))
})

testthat::test_that("a target hidden altogether hides the others altogether", {
  data <- mesos_data()
  # A target of one respondent fails hide_for_crowd_if_category_k_below
  data$f_uni[1] <- "Tiny"

  out <- crowd_tables(
    data,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_group = "Tiny"
  )
  testthat::expect_true(is.data.frame(out) && nrow(out) == 0)

  plots <- makeme(
    data = data,
    dep = b_1,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Tiny"
  )
  testthat::expect_true(is.data.frame(plots) && nrow(plots) == 0)
})

testthat::test_that("the order of crowd does not matter", {
  data <- mesos_data()
  data$b_2[data$f_uni == "Uni of A"] <- NA

  out <- crowd_tables(data, dep = b_1:b_3, crowd = c("others", "target"))

  testthat::expect_named(out, c("Others", "Target"))
  testthat::expect_equal(cities(out$Others), c("Bejing", "Budapest"))
})

testthat::test_that("the all crowd follows the target only when asked to", {
  data <- mesos_data()
  data$b_2[data$f_uni == "Uni of A"] <- NA

  out <- crowd_tables(data, dep = b_1:b_3, crowd = c("target", "others", "all"))
  testthat::expect_equal(cities(out$Others), c("Bejing", "Budapest"))
  testthat::expect_equal(cities(out$All), c("Bejing", "Brussels", "Budapest"))

  out <- crowd_tables(
    data,
    dep = b_1:b_3,
    crowd = c("target", "others", "all"),
    hide_for_all_crowds_if_hidden_for_crowd = "target"
  )
  testthat::expect_equal(cities(out$All), c("Bejing", "Budapest"))
})

testthat::test_that("the others on their own are unaffected", {
  data <- mesos_data()
  data$b_2[data$f_uni == "Uni of A"] <- NA

  out <- crowd_tables(data, dep = b_1:b_3, crowd = "others")
  testthat::expect_equal(cities(out$Others), c("Bejing", "Brussels", "Budapest"))
})

testthat::test_that("the others are dropped when the target yields no output at all", {
  # Only reachable through paths downstream of the per-variable filtering, so
  # the target's processing is stood in for here.
  seen <- character()
  testthat::local_mocked_bindings(
    process_crowd_data = function(crwd, ...) {
      seen <<- c(seen, crwd)
      if (crwd == "target") NULL else crwd
    }
  )
  out <- saros:::process_all_crowds(
    args = list(crowd = c("others", "target", "all"), dep = "b_1", showNA = "never"),
    omitted_cols_list = list(),
    kept_indep_cats_list = list(),
    data = saros::ex_survey,
    mesos_var = "f_uni",
    mesos_group = "Uni of A"
  )
  testthat::expect_equal(seen, c("target", "all"))
  testthat::expect_null(out$others)
  testthat::expect_equal(out$all, "all")
})
