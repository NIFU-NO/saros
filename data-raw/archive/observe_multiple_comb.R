
#' Extention to the infer package for multiple testing
#'
#' @inheritParams infer::t_test
#' @inheritParams infer::chisq_test
#' @inheritParams infer::prop_test
#' @param test String indicating type of test done for all combinations.
#'
#' @importFrom dplyr mutate across bind_cols summarize group_by n
#' @importFrom tidyselect eval_select
#' @importFrom labelled remove_val_labels
#' @importFrom purrr map_dfr
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names enquo arg_match sym :=
#' @importFrom infer t_test chisq_test prop_test
#' @return A tibble containing for each response-explanatory row combination
#' columns summarizing the test statistic.
#' @export
#'
#' @examples
#' library(dplyr)
#' x <- test_multiple_comb(x = mtcars,
#'                      response = matches("mpg|disp|drat|hp|qsec"),
#'                      explanatory = matches("vs|am"),
#'                      test = "t")
test_multiple_comb <-
  function(x,
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
           z = FALSE
  ) {

    test <- rlang::arg_match(test, multiple = FALSE)
    response_cols <- tidyselect::eval_select(rlang::enquo(response), x)

    explanatory_cols <- tidyselect::eval_select(rlang::enquo(explanatory), x)



    x <-
      dplyr::mutate(x,
                    across(c({{response}}, {{explanatory}}),
                           ~labelled::remove_val_labels(.x)),
                    across(c({{response}}, {{explanatory}}),
                           ~{if(is.factor(.x)) droplevels(.x) else .x}))

    resp_cols <- names(response_cols)
    resp_cols <- rlang::set_names(resp_cols)
    expl_cols <- names(explanatory_cols)
    expl_cols <- rlang::set_names(expl_cols)

    cli::cli_progress_bar("Computing tests", total = length(resp_cols))

    out <-
      purrr::map_dfr(.x = resp_cols,
                   .id = "response",
                   .f = function(response) {
                     cli::cli_progress_update(.envir = rlang::env_parent())


                     resp_col <- rlang::sym(response)

                     if(length(expl_cols) == 0L) {

                       if(test == "t") {

                         means <- dplyr::summarize(x,
                                                   mean = mean(!!resp_col, na.rm=TRUE),
                                                   mu = mu)
                         res_test <-
                           infer::t_test(x = x,
                                         response = !!resp_col,
                                         order = order,
                                         alternative = alternative,
                                         mu = mu,
                                         conf_int = conf_int,
                                         conf_level = conf_level)
                         dplyr::bind_cols(res_test, means)


                       } else if(test == "chisq") {
                         infer::chisq_test(x = x,
                                           response = !!resp_col,
                                           correct = correct,
                                           p = p)
                       }
                     } else {
                       purrr::map_dfr(.x = expl_cols,
                                      .id = "explanatory",
                                      .f = function(explanatory) {

                                        # cli::cli_progress_update(.envir = rlang::env_parent(env = n = 1))

                                        expl_col <- rlang::sym(explanatory)

                                        if(test == "t") {



                                          means <- dplyr::group_by(x, !!expl_col)
                                          means <- dplyr::summarize(means,
                                                                    mean = mean(!!resp_col, na.rm=TRUE),
                                                                    n = dplyr::n())
                                          means <- tidyr::pivot_wider(means,
                                                                      names_from = !!expl_col,
                                                                      values_from = c(mean, n))

                                          res_test <-
                                            infer::t_test(x = x,
                                                          response = !!resp_col,
                                                          explanatory = !!expl_col,
                                                          order = order,
                                                          alternative = alternative,
                                                          mu = mu,
                                                          conf_int = conf_int,
                                                          conf_level = conf_level)
                                          dplyr::bind_cols(res_test, means)




                                        } else if(test == "chisq") {

                                          miss <- dplyr::filter(x, !is.na(!!resp_col), !is.na(!!expl_col))
                                          if(nrow(miss)>3 &&
                                             dplyr::n_distinct(x[[response]]) >= 2 &&
                                             dplyr::n_distinct(x[[explanatory]]) >= 2) {
                                            tryCatch(
                                          infer::chisq_test(x = x,
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
                                          infer::prop_test(x = x,
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
                                      })
                     }
                   })
    cli::cli_progress_done()
    out
  }
