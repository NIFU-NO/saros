pull_uniques <- function(x) {
  if(is.factor(x)) {
    levels(x)
  } else {
    as.character(unique(x))
  }
}
