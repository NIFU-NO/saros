get_common_name <- function(x) {
  filename_prefix_alt <- Reduce(f = intersect, strsplit(x, split = ""))

  if(length(filename_prefix_alt)>0 && all(!is.na(filename_prefix_alt))) {
    filename_prefix_alt <- stringi::stri_c(filename_prefix_alt, collapse = "", ignore_null = TRUE)
    if(nchar(filename_prefix_alt)>0) {
      x <- filename_prefix_alt
    }
  }
  x
}
