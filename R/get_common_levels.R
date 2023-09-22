get_common_levels <- function(data, col_names=NULL) {
  if(!rlang::is_character(col_names)) col_names <- colnames(data)
  fct_unions <- if(!inherits(data, "survey.design")) data[, col_names] else data$variables[, col_names]
  fct_unions <- forcats::fct_unify(fs = fct_unions)[[1]]
  levels(fct_unions)
}


get_common_data_type <- function(data, col_names=NULL) {
  if(!rlang::is_character(col_names)) col_names <- colnames(data)
  x <- unique(unlist(lapply(data[, col_names], function(x) class(x)[1])))
  if(length(x)==1) return(x)
  if(all(x %in% c("ordered", "factor"))) return("factor")
  "integer"
}
