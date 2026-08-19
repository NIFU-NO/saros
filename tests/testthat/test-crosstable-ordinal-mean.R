# #577: saros deliberately computes .mean/.median over the *factor level codes*
# of ordinal survey scales, whose categories are text ("Not at all", "A bit",
# "A lot"). Taking the mean of an ordinal scale's positions is a normal and
# useful summary in survey reporting.
#
# The original issue proposed converting with as.character() first. That would
# parse the label text and return all-NA for every text-based scale. These tests
# pin the intended behaviour so the "fix" cannot be reapplied silently.

testthat::test_that("crosstable .mean uses ordinal factor level codes, not label text (#577)", {
  result <- saros:::crosstable.data.frame(
    saros::ex_survey,
    dep = "b_1",
    indep = "x1_sex",
    showNA = "never"
  )

  testthat::expect_true(".mean" %in% colnames(result))

  observed_mean <- unique(result$.mean[result$x1_sex == "Males"])
  observed_mean <- observed_mean[!is.na(observed_mean)]

  # Recompute independently: mean over the integer level codes of the factor.
  males <- saros::ex_survey$b_1[
    saros::ex_survey$x1_sex == "Males" & !is.na(saros::ex_survey$b_1)
  ]
  expected_mean <- mean(as.numeric(males))

  testthat::expect_equal(observed_mean[1], expected_mean, tolerance = 1e-6)

  # The scale has 3 text categories, so a level-code mean must land in [1, 3].
  # If as.character() were applied first, this would be NaN/NA instead.
  testthat::expect_gte(observed_mean[1], 1)
  testthat::expect_lte(observed_mean[1], 3)
  testthat::expect_false(is.na(observed_mean[1]))
})

testthat::test_that("crosstable .median uses ordinal factor level codes for text scales (#577)", {
  result <- saros:::crosstable.data.frame(
    saros::ex_survey,
    dep = "b_1",
    indep = "x1_sex",
    showNA = "never"
  )

  testthat::expect_true(".median" %in% colnames(result))

  observed_median <- unique(result$.median[result$x1_sex == "Males"])
  observed_median <- observed_median[!is.na(observed_median)]

  males <- saros::ex_survey$b_1[
    saros::ex_survey$x1_sex == "Males" & !is.na(saros::ex_survey$b_1)
  ]
  expected_median <- stats::median(as.numeric(males))

  testthat::expect_equal(observed_median[1], expected_median, tolerance = 1e-6)
  testthat::expect_false(is.na(observed_median[1]))
})

testthat::test_that("text-based category labels do not coerce to numeric (#577)", {
  # This is the premise of the two tests above: the labels are text, so any
  # implementation that parses the *labels* yields NA for the whole scale.
  category_labels <- levels(saros::ex_survey$b_1)

  testthat::expect_true(all(is.na(suppressWarnings(as.numeric(category_labels)))))
  testthat::expect_equal(as.numeric(factor(category_labels, levels = category_labels)), seq_along(category_labels))
})
