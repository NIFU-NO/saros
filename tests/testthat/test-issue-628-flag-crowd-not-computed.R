# Regression guard for #628: with
# hide_indep_cat_for_all_crowds_if_hidden_for_crowd = TRUE, the categories of
# the independent variable kept by the crowds named in
# hide_for_all_crowds_if_hidden_for_crowd decide what the other crowds may
# show. A named crowd that was never computed kept no categories, so every
# category was hidden and the output came back empty. The likeliest way to
# hit it: the flag set to "target" globally, and a chunk with the default
# crowd = "all".

cat_table <- function(...) {
  makeme(
    data = saros::ex_survey,
    dep = b_1,
    indep = x1_sex,
    type = "cat_table_html",
    ...
  )
}

testthat::test_that("a flag naming a crowd that was not computed is ignored", {
  baseline <- cat_table()
  testthat::expect_equal(nrow(baseline), 2)

  flagged <- cat_table(
    hide_for_all_crowds_if_hidden_for_crowd = "target",
    hide_indep_cat_for_all_crowds_if_hidden_for_crowd = TRUE
  )
  testthat::expect_identical(flagged, baseline)
})

testthat::test_that("the same holds when the flag comes from global settings", {
  withr::defer(global_settings_reset(fn_name = "makeme", quiet = TRUE))
  global_settings_set(
    fn_name = "makeme",
    new = list(
      hide_for_all_crowds_if_hidden_for_crowd = "target",
      hide_indep_cat_for_all_crowds_if_hidden_for_crowd = TRUE
    ),
    quiet = TRUE
  )
  testthat::expect_equal(nrow(cat_table()), 2)
})

testthat::test_that("crowds that were computed still decide", {
  kept <- list(
    target = list(g = "a"),
    others = list(g = c("a", "b"))
  )
  out <- saros:::process_global_indep_categories(kept, "target")
  testthat::expect_equal(out$target$g, "a")
  testthat::expect_equal(out$others$g, "a")

  # Only the computed part of the flag counts
  out <- saros:::process_global_indep_categories(kept, c("target", "all"))
  testthat::expect_equal(out$others$g, "a")
})

testthat::test_that("a flag naming no computed crowd leaves every list alone", {
  kept <- list(all = list(g = c("a", "b")))
  testthat::expect_identical(
    saros:::process_global_indep_categories(kept, "target"),
    kept
  )
  testthat::expect_identical(
    saros:::process_global_indep_categories(kept, NULL),
    kept
  )
})

testthat::test_that("end to end, a category the target lacks is still hidden from the others", {
  d <- saros::ex_survey
  # Every respondent at Uni of A is male, so the target never shows "Females"
  d$x1_sex[d$f_uni == "Uni of A"] <- "Males"

  plots <- makeme(
    data = d,
    dep = b_1,
    indep = x1_sex,
    crowd = c("target", "others"),
    mesos_var = "f_uni",
    mesos_group = "Uni of A",
    hide_for_all_crowds_if_hidden_for_crowd = "target",
    hide_indep_cat_for_all_crowds_if_hidden_for_crowd = TRUE,
    simplify_output = FALSE
  )
  others_groups <- unique(sub("___.*$", "", as.character(plots$Others$data$x1_sex)))
  testthat::expect_equal(others_groups, "Males")
})
