
#' Make Table with All Combinations of Univariate/Bivariate Significance Tests
#'  Based on Variable Types
#'
#'  Although there are hundreds of significance tests for associations between
#'  two variables, depending upon the distributions, variables types and
#'  assumptions, most fall into a smaller set of popular tests. This function
#'  runs for all combinations of dependent and independent variables in data,
#'  with a suitable test (but not the only possible) for the combination. Also
#'  supports univariate tests, where the assumptions are that of a mean of zero
#'  for continuous variables or all equal proportions for binary/categorical.
#'
#'  This function does not allow any adjustments - use the original underlying
#'  functions for that (chisq.test, t.test, etc.)
#'
#'
#' @param data Data frame (or tibble)
#' @param dep,indep Character vector of dependent/independent variable names.
#'
#' @return Data.frame
#' @export
#'
#' @examples sigtest_table(ex_survey, dep=paste0("b_", 1:3), indep="x1_sex")
sigtest_table <- function(data, dep, indep=NULL) {
  if(!is.character(dep)) cli::cli_abort("{.arg dep} must be character vector.")

  if(is.character(indep)) {

    out <- expand.grid(dep = dep, indep = indep, stringsAsFactors = FALSE)
    nr <- seq_len(nrow(out))
    out2 <- lapply(nr, function(i) {
      tryCatch({
      find_test2(y = data[[out[i, "dep", drop=TRUE]]],
                 x = data[[out[i, "indep", drop=TRUE]]])
      },
      error = function(e) data.frame(.bi_test = NA_character_, .p_value = NA_real_))
    })

  } else if(is.null(indep)) {

    out <- expand.grid(dep = dep, stringsAsFactors = FALSE)
    nr <- seq_len(nrow(out))
    out2 <- lapply(nr, function(i) {
      tryCatch({
        find_test2(y = data[[out[i, "dep", drop=TRUE]]],
                 x = NULL)
      },
      error = function(e) data.frame(.bi_test = NA_character_, .p_value = NA_real_))

    })
  }
  cbind(out, do.call(rbind, out2))

}
