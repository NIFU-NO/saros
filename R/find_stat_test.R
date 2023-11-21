
#
# find_test <- function(y, x) {
#
#   chisq_test <- function(...) stats::chisq.test(x=y, y=x)$p.value
#   anova_test <- function(y, x) summary(stats::aov(y ~ x))[[1]][["Pr(>F)"]][1]
#   cor_test <- function(y, x) stats::cor.test(x, y, use = "complete.obs")$p.value
#   kruskal_test <- function(y, x) stats::kruskal.test(x=y, g=x)$p.value
#
#
#   if((inherits(y, what = "numeric") ||
#       inherits(y, what = "integer")) &&
#      (inherits(x, what = "numeric") ||
#       inherits(x, what = "integer") ||
#       inherits(x, what = "ordered"))) return(cor_test)
#
#   if(inherits(y, what = "factor") &&
#      (inherits(x, what = "factor") ||
#       inherits(x, what = "ordered"))) return(chisq_test)
#
#   if((inherits(y, what = "numeric") ||
#       inherits(y, what = "integer")) &&
#      inherits(x, what = "factor")) return(anova_test)
#
#   if(inherits(y, what = "ordered") &&
#      inherits(x, what = "factor")) return(kruskal_test)
#
#   if(!inherits(y, what = "character") &&
#      !inherits(x, what = "character")) {
#     cli::cli_warn("Unable to find a suitable statistical test for outcome {class(y)} and {class(x)}.")
#   }
#   return()
#
# }

find_test2 <- function(y, x=NULL) {
  # Determine the type of the vectors
  y_type <- class(y)[1]
  x_type <- class(x)[1]

  # Initialize variables to store test results
  test_name <- NULL
  p_value <- NULL

  if(is.null(x)) {
    if(y_type == "numeric") {
      # One-sample t-test against mean of 0
      test_name <- "One-sample t-test"
      p_value <-
      tryCatch(expr = {
        stats::t.test(y, mu = 0)$p.value
      }, error = function(e) return(NA))
    } else if(y_type == "factor") {
      # Chi-squared goodness-of-fit test for uniform distribution
      test_name <- "Chi-squared Goodness-of-Fit Test"
      p_value <-
        tryCatch(expr = {
          stats::chisq.test(table(y, useNA="no"))$p.value
        }, error = function(e) return(NA))
    }
  } else {
    # Select the test based on the type of y and x
    if(y_type == "numeric" && x_type == "factor") {

      # ANOVA
      test_name <- "ANOVA"
      p_value <-
        tryCatch(expr = {
          summary(stats::aov(y ~ x))[[1]]$`Pr(>F)`[1]
    }, error = function(e) return(NA))

    } else if(y_type == "factor" && x_type == "numeric") {

      # ANOVA (factor as response variable not typical, consider logistic regression)
      test_name <- "ANOVA"
      p_value <-
        tryCatch(expr = {
          summary(stats::aov(x ~ y))[[1]]$`Pr(>F)`[1]
        }, error = function(e) return(NA))

    } else if(y_type == "factor" && x_type == "factor") {

      # Chi-squared test
      test_name <- "Chi-sq"
      p_value <-
        tryCatch(expr = {
          suppressWarnings(stats::chisq.test(table(y, x, useNA="no")))$p.value
        }, error = function(e) return(NA))

    } else if(y_type == "numeric" && x_type == "numeric") {

      # Correlation test
      test_name <- "Pearson Cor"
      p_value <-
        tryCatch(expr = {
          stats::cor.test(y, x, use="complete.obs")$p.value
        }, error = function(e) return(NA))

    } else if((y_type == "ordered" && x_type == "factor") ||
              (y_type == "factor" && x_type == "ordered")) {

      # Kruskal-Wallis chisq test
      test_name <- "Kruskal-Wallis chisq"
      p_value <-
        tryCatch(expr = {
          stats::kruskal.test(y ~ x)$p.value
        }, error = function(e) return(NA))

    } else if(y_type == "ordered" && x_type == "numeric") {

      # Spearman's rank correlation test
      test_name <- "Spearman Rank Cor"
      p_value <-
        tryCatch(expr = {
          stats::cor.test(y, x, method = "spearman", use="complete.obs")$p.value
    }, error = function(e) return(NA))

    } else if(y_type == "numeric" && x_type == "ordered") {

      # Spearman's rank correlation test
      test_name <- "Spearman Rank Cor"
      p_value <-
        tryCatch(expr = {
          stats::cor.test(x, y, method = "spearman", use="complete.obs")$p.value
        }, error = function(e) return(NA))

    } else if(y_type == "ordered" && x_type == "ordered") {

      # Spearman's rank correlation test
      test_name <- "Spearman Rank Cor"
      p_value <-
        tryCatch(expr = {
          stats::cor.test(x, y, method = "spearman", use="complete.obs")$p.value
        }, error = function(e) return(NA))

    } else if(!(y_type == "character" || x_type == "character" ||
                y_type == "POSIXt" || x_type == "POSIXt")) {

      cli::cli_warn("Unable to find a suitable statistical test for outcome {y_type} and {x_type}.")
      return(data.frame(.bi_test = NA, .p_value = NA))
    }
  }

  # Return the name of the test and the p-value
  data.frame(.bi_test = test_name, .p_value = p_value)
}



# find_stat_config <-
#   function(dep_pos,
#            .variable_type,
#            dep_n,
#            dep_unique,
#            indep_type,
#            indep_pos,
#            indep_n,
#            indep_unique) {
#
#     categorical_types <- c("fct", "factor", "ord", "ordered")
#     continuous_types <- c("numeric", "dbl", "integer", "int")
#
#     stat_test <-
#       dplyr::case_when(
#         .variable_type %in% continuous_types &&
#           dep_n > 2 && length(indep_pos) == 0 ~ "mean",
#
#         dep_n == 2 && length(indep_pos) == 0 ~ "prop",
#
#         .variable_type %in% categorical_types &&
#           dep_n > 2 && length(indep_pos) == 0 ~ "chisq",
#
#         .variable_type %in% continuous_types &&
#           length(indep_pos) == 1 &&
#           indep_type %in% continuous_types &&
#           indep_n >= 5 ~ "correlation",
#
#         .variable_type %in% continuous_types &&
#           length(indep_pos) == 1 &&
#           indep_type %in% categorical_types &&
#           indep_n > 2 ~ "F",
#
#         .variable_type %in% continuous_types &&
#           length(indep_pos) == 1 &&
#           indep_type %in% categorical_types &&
#           indep_n == 2 ~ "t",
#
#         .variable_type %in% categorical_types &&
#           dep_n >= 2 &&
#           length(indep_pos) == 1 &&
#           indep_type %in% categorical_types &&
#           indep_n >= 2 ~ "chisq",
#
#         .default = "NA"
#       )
#     if(stat_test == "NA" && .variable_type != "chr") {
#       error_indep_str <- if(length(indep_pos) == 1) stringi::stri_c(ignore_null=TRUE, " and {.arg {indep_type}} ({.arg {indep_pos}})")
#       cli::cli_warn("Statistical test not found for {.arg { .variable_type}} ({.arg {dep_pos}}, n_unique={.var {dep_n}}){error_indep_str}.")
#     }
#
#     lvls <- dep_unique %>% as.character()
#     success <- if(stat_test %in% c("prop", "chisq") && dep_n == 2) lvls[length(lvls)]
#     p_lvls <- if(stat_test %in% c("prop", "chisq") && length(indep_pos) == 0) stats::setNames(rep(1/length(lvls), length(lvls)), nm=lvls)
#     m_lvls <- if(stat_test %in% c("mean")) 0
#     order_lvls <- if(stat_test %in% c("prop", "chisq", "t") &&
#                      # .variable_type %in% c("numeric", "integer", "int") &&
#                      length(indep_pos) == 1 &&
#                      indep_n == 2) as.character(indep_unique)
#     generate_type <- if(length(indep_pos) == 0 && stat_test %in% c("chisq", "prop")) "draw" else "bootstrap"
#     null_hypothesis_type <- if(length(indep_pos) == 0) "point" else "independence"
#
#     list(test = stat_test,
#          lvls = lvls,
#          success = success,
#          p_lvls = p_lvls,
#          m_lvls = m_lvls,
#          order_lvls = order_lvls,
#          null_hypothesis_type = null_hypothesis_type,
#          generate_type = generate_type)
#   }

