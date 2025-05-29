get_common_levels <- function(data, col_pos = NULL) {
  if (is.null(col_pos)) {
    return(character())
  }
  fct_unions <- if (!inherits(data, "survey.design")) data[, col_pos, drop = FALSE] else data$variables[, col_pos, drop = FALSE]
  fct_unions <- forcats::fct_unify(fs = fct_unions)[[1]]
  levels(fct_unions)
}


get_common_data_type <- function(data, col_pos = NULL) {
  x <- unique(unlist(lapply(data[, col_pos, drop = FALSE], function(x) class(x)[1])))
  if (length(x) == 1) {
    return(x)
  }
  if (all(x %in% c("ordered", "factor"))) {
    return("factor")
  }
  "integer"
}
