ungroup_data <- function(data) {
  if(inherits(data, "survey")) {
     srvyr::ungroup(data)
  } else dplyr::ungroup(data)
}
