#' Simply Run Measurement Invariance Model in lavaan
#'
#' @param data data.frame or tibble.
#' @param cols Columns as character vector
#' @param group Grouping variable as string
#' @param ... Not in use
#'
#' @return A tibble.
#' @importFrom lavaan cfa fitmeasures lavTestLRT
#' @importFrom semTools measEq.syntax
#' @importFrom labelled remove_val_labels
#' @importFrom dplyr mutate across bind_rows
#' @importFrom tidyselect everything
#' @importFrom purrr map map_dfr
#' @importFrom tibble tibble
#'
lavaan_mi <- function(data, cols, group, ...) {

  data <-
    dplyr::mutate(data, dplyr::across(tidyselect::everything(), ~labelled::remove_val_labels(.x)))

  var_syntax <- paste0("F1 =~ ", paste0(cols, collapse="+"))

  mi_model_options <- list(configural = "configural",
                           thresholds = "thresholds",
                           scalar = c("threshold", "loadings"))
  mi_fits <-
    purrr::map(mi_model_options,
               .f = ~{

                 mi_syntax <- semTools::measEq.syntax(configural.model = var_syntax,
                                            data = data,
                                            ordered = cols,
                                            parameterization = "delta",
                                            ID.fac = "std.lv",
                                            ID.cat = "Wu.Estabrook.2016",
                                            group = group,
                                            group.equal = .x) #

                 # mi_fit <-
                 lavaan::cfa(as.character(mi_syntax), data = data, group = group, ordered = cols)
               })
  comp <- lavaan::lavTestLRT(mi_fits[[1]], mi_fits[[2]], mi_fits[[3]], method = "satorra.bentler.2010")

  comp <-
    tibble::tibble(
    X2_diff=comp[["Chisq diff"]],
    df_diff=comp[["Df diff"]],
    X2_p=comp[["Pr(>Chisq)"]],
    holds = .data$X2_p > .05)
  out <-
    purrr::map_dfr(mi_fits, .id = "model", .f = ~{
      round(lavaan::fitmeasures(.x,
                        fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                         "rmsea.scaled", "cfi.scaled", "tli.scaled")), digits=3)
    })
  out <- dplyr::mutate(out, dplyr::across(-.data$model, ~as.vector(.x)))
  dplyr::bind_cols(out, comp)
  mi_fits[[2]]
}
