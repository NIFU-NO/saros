get_common_levels <- function(data, col_pos=NULL) {
  if(!rlang::is_integer(col_pos)) {
    col_pos <- if(!inherits(data, "survey.design")) seq_len(ncol(data)) else seq_len(ncol(data$variables))
  }
  fct_unions <- if(!inherits(data, "survey.design")) data[, col_pos] else data$variables[, col_pos]
  fct_unions <- forcats::fct_unify(fs = fct_unions)[[1]]
  levels(fct_unions)
}


get_common_data_type <- function(data, col_pos=NULL) {
  if(!rlang::is_integer(col_pos)) col_pos <- seq_len(ncol(data))
  x <- unique(unlist(lapply(data[, col_pos], function(x) class(x)[1])))
  if(length(x)==1) return(x)
  if(all(x %in% c("ordered", "factor"))) return("factor")
  "integer"
}
