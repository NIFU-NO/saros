err_msg <- function(infix) {
  stringi::stri_c(
    ignore_null = TRUE,
    "{.arg {arg}} must be a",
    infix,
    ", not {.obj_type_friendly {x}}."
  )
}
