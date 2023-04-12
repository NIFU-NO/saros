
saros_default_options <-
  list(translations =
         list(last_sep = " and ",
              intro_prefix = "We will now look at the questions asked regarding ",
              intro_suffix = "",
              mode_max_onfix = " on ",
              mode_max_prefix = "The most common responses were ",
              mode_max_suffix = "",
              not_used_prefix = "The following response categories were not used: ",
              not_used_suffix = "",
              value_max_prefix = "",
              value_max_infix = " {?is/are} the {n_top_bottom} item{?s} where the most have responded ",
              value_max_suffix = "",
              value_min_prefix = "",
              value_min_infix = " {?is/are} the {n_top_bottom} item{?s} where the fewest have responded ",
              value_min_suffix = "",
              mean_onfix = "M = ",
              mean_max_prefix = "They have highest mean on ",
              mean_max_suffix = "",
              mean_min_prefix = "They have lowest mean on ",
              mean_min_suffix = "",
              median_onfix = "Median = ",
              median_max_prefix = "They have highest median on ",
              median_max_suffix = "",
              median_min_prefix = "They have lowest median on ",
              median_min_suffix = "",


              intro_by_prefix = "We will now look at the questions asked regarding ",
              intro_by_infix = " broken down by ",
              intro_by_suffix = ""

         ))

.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("saros"))) {
    options("saros" = saros_default_options)
  }
  invisible()
}

