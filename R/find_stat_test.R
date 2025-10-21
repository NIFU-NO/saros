find_test2 <- function(y, x = NULL) {
  # Determine the type of the vectors
  y_type <- class(y)[1]
  x_type <- class(x)[1]

  # Initialize dataframe to store test results
  result <- data.frame(
    .bi_test = NA_character_,
    .p_value = NA_real_,
    y_type = y_type,
    x_type = if (is.null(x)) NA_character_ else x_type
  )

  if (is.null(x)) {
    if (y_type == "numeric") {
      # One-sample t-test against mean of 0
      result$.p_value <-
        tryCatch(
          expr = {
            stats::t.test(y, mu = 0)$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "One-sample t-test"
    } else if (y_type == "factor") {
      # Chi-squared goodness-of-fit test for uniform distribution
      result$.p_value <-
        tryCatch(
          expr = {
            stats::chisq.test(table(y, useNA = "no"))$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "Chi-squared Goodness-of-Fit Test"
    }
  } else {
    # Select the test based on the type of y and x
    if (y_type == "numeric" && x_type == "factor") {
      # ANOVA
      result$.p_value <-
        tryCatch(
          expr = {
            summary(stats::aov(y ~ x))[[1]]$`Pr(>F)`[1]
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "ANOVA"
    } else if (y_type == "factor" && x_type == "numeric") {
      # ANOVA (factor as response variable not typical, consider logistic regression)

      result$.p_value <-
        tryCatch(
          expr = {
            summary(stats::aov(x ~ y))[[1]]$`Pr(>F)`[1]
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "ANOVA"
    } else if (y_type == "factor" && x_type == "factor") {
      # Chi-squared test

      result$.p_value <-
        tryCatch(
          expr = {
            suppressWarnings(stats::chisq.test(table(
              y,
              x,
              useNA = "no"
            )))$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "Chi-squared Goodness-of-Fit Test"
    } else if (y_type == "numeric" && x_type == "numeric") {
      # Correlation test

      result$.p_value <-
        tryCatch(
          expr = {
            stats::cor.test(y, x, use = "complete.obs")$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "Pearson Correlation"
    } else if (
      (y_type == "ordered" && x_type == "factor") ||
        (y_type == "factor" && x_type == "ordered")
    ) {
      # Kruskal-Wallis chisq test

      result$.p_value <-
        tryCatch(
          expr = {
            stats::kruskal.test(y ~ x)$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "Kruskal-Wallis chisq"
    } else if (y_type == "ordered" && x_type == "numeric") {
      # Spearman's rank correlation test

      result$.p_value <-
        tryCatch(
          expr = {
            stats::cor.test(
              y,
              x,
              method = "spearman",
              use = "complete.obs"
            )$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "Spearman Rank Correlation"
    } else if (y_type == "numeric" && x_type == "ordered") {
      # Spearman's rank correlation test

      result$.p_value <-
        tryCatch(
          expr = {
            stats::cor.test(
              x,
              y,
              method = "spearman",
              use = "complete.obs"
            )$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "Spearman Rank Correlation"
    } else if (y_type == "ordered" && x_type == "ordered") {
      # Spearman's rank correlation test

      result$.p_value <-
        tryCatch(
          expr = {
            stats::cor.test(
              x,
              y,
              method = "spearman",
              use = "complete.obs"
            )$p.value
          },
          error = function(e) NA_real_
        )
      result$.bi_test <- "Spearman Rank Correlation"
    } else {
      cli::cli_warn(
        "Unable to find a suitable statistical test for y: {y_type} and x: {x_type}."
      )
    }
  }

  # Return the result dataframe
  result
}
