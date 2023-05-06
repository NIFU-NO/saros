
#' Extension to the infer package for multiple testing
#'
#' @inheritParams infer::t_test
#' @inheritParams infer::chisq_test
#' @inheritParams infer::prop_test
#' @inheritParams summarize_data
#' @param test String indicating type of test done for all combinations.
#' @param parametric Logical, if false, applies a set of non-parametric tests equivalent to the t-, chisq-, and prop tests.
#'
#' @importFrom rlang := !!!
#' @return A data frame containing for each response-explanatory row combination
#' columns summarizing the test statistic.
#' @export
#'
#' @examples
#' library(dplyr)
#' x <- test_multiple_comb(data = mtcars,
#'                      response = matches("mpg|disp|drat|hp|qsec"),
#'                      explanatory = matches("vs|am"),
#'                      test = "t")
test_multiple_comb <-
  function(data,
           ...,
           formula = NULL,
           response = NULL,
           explanatory = NULL,
           order = NULL,
           alternative = "two-sided",
           mu = 0,
           correct = TRUE,
           p = NULL,
           conf_int = TRUE,
           conf_level = 0.95,
           test = c("t", "chisq", "prop"),
           success = NULL,
           z = FALSE,
           parametric = TRUE,
           call = rlang::caller_env()
  ) {

    dots <- rlang::list2(...)
    test <- rlang::arg_match(test, call = call)
    response_cols <- tidyselect::eval_select(rlang::enquo(response), data, error_call = call)

    explanatory_cols <- tidyselect::eval_select(rlang::enquo(explanatory), data, error_call = call)



    data <-
      dplyr::mutate(data,
                    dplyr::across(c({{response}}, {{explanatory}}),
                           ~labelled::remove_val_labels(.x)),
                    dplyr::across(c({{response}}, {{explanatory}}),
                           ~{if(is.factor(.x)) droplevels(.x) else .x}))

    resp_cols <- names(response_cols)
    resp_cols <- stats::setNames(resp_cols, nm=resp_cols)
    expl_cols <- names(explanatory_cols)
    expl_cols <- stats::setNames(expl_cols, nm=expl_cols)

    cli::cli_progress_bar("Computing tests", total = length(resp_cols))

    out <-
      purrr::map_dfr(.x = resp_cols,
                     .id = "response",
                     .f = function(response) {
                       cli::cli_progress_update(.envir = rlang::env_parent())


                       resp_col <- rlang::sym(response)

                       if(length(expl_cols) == 0L) {

                         if(test == "t") {

                           means <- dplyr::summarize(data,
                                                     mean = mean(!!resp_col, na.rm=TRUE),
                                                     mu = mu)
                           res_test <-
                             infer::t_test(x = data,
                                           response = !!resp_col,
                                           order = order,
                                           alternative = alternative,
                                           mu = mu,
                                           conf_int = conf_int,
                                           conf_level = conf_level)
                           dplyr::bind_cols(res_test, means)


                         } else if(test == "chisq") {
                           infer::chisq_test(x = data,
                                             response = !!resp_col,
                                             correct = correct,
                                             p = p)

                         } else if(test == "prop") {
                           infer::prop_test(x = data,
                                             response = !!resp_col,
                                             correct = correct,
                                             p = p)
                         }
                       } else {
                         purrr::map(.x = expl_cols,
                                        .f = function(explanatory) {

                                          # cli::cli_progress_update(.envir = rlang::env_parent(env = n = 1))

                                          expl_col <- rlang::sym(explanatory)

                                          if(test == "t") {



                                            means <-
                                              dplyr::group_by(data, !!expl_col) %>%
                                              dplyr::summarize(mean = mean(!!resp_col, na.rm=TRUE),
                                                               n = dplyr::n()) %>%
                                              tidyr::pivot_wider(names_from = !!expl_col,
                                                                 values_from = c("mean", "n"))

                                            res_test <-
                                              infer::t_test(x = data,
                                                            response = !!resp_col,
                                                            explanatory = !!expl_col,
                                                            order = order,
                                                            alternative = alternative,
                                                            mu = mu,
                                                            conf_int = conf_int,
                                                            conf_level = conf_level)
                                            dplyr::bind_cols(res_test, means)




                                          } else if(test == "chisq") {

                                            miss <- dplyr::filter(data, !is.na(!!resp_col), !is.na(!!expl_col))
                                            if(nrow(miss)>3 &&
                                               dplyr::n_distinct(data[[response]]) >= 2 &&
                                               dplyr::n_distinct(data[[explanatory]]) >= 2) {

                                              tryCatch(
                                                infer::chisq_test(x = data,
                                                                  response = !!resp_col,
                                                                  explanatory = !!expl_col,
                                                                  correct = correct,
                                                                  p = p),
                                                error = function(e) {

                                                  cli::cli_warn("Bivariate cells are all zero: {{response}} by {{explanatory}}. Dropping chisq-test.")
                                                  return(NULL)
                                                })
                                            } else {
                                              cli::cli_warn("Bivariate cells are all zero: {{response}} by {{explanatory}}. Dropping chisq-test.")
                                              NULL
                                            }


                                          } else if(test == "prop") {
                                            infer::prop_test(x = data,
                                                             response = !!resp_col,
                                                             explanatory = !!expl_col,
                                                             order = order,
                                                             alternative = alternative,
                                                             p = p,
                                                             success = success,
                                                             correct = correct,
                                                             z = z,
                                                             conf_int = conf_int,
                                                             conf_level = conf_level)
                                          }
                                        }) %>%
                           dplyr::bind_rows(.id = "explanatory")
                       }
                     })
    cli::cli_progress_done()
    out
  }
