keep_indep_cats <- function(data, indep) {
  lapply(rlang::set_names(indep), function(x) {
    as.character(unique(data[[x]]))
  })
}
