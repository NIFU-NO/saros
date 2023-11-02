pull_uniques <- function(x) {
  if(is.factor(x)) {
    x <- levels(x)
  } else {
    x <- as.character(unique(x))
  }
  x[!is.na(x)]
}
