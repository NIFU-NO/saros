update_dots <- function(dots = list(),
                        caller_function = NULL,
                        allow_unique_overrides = TRUE) {

  # if(rlang::is_null(caller_function)) {
  #   caller_function <- rlang::caller_call(n=1)
  #   caller_function <- as.character(caller_function)
  #   caller_function <- paste0(stringi::stri_replace_all_regex(caller_function,
  #                                     pattern = "^(.+)\\(.+", replacement = "$1"),
  #                             collapse="_")
  # }
  # print(caller_function)

  dots <- utils::modifyList(
    x = formals(draft_report)[!names(formals(draft_report)) %in% .saros.env$ignore_args],
    val = dots[!names(dots) %in% c("...")], keep.null = TRUE
  )

  if(isTRUE(allow_unique_overrides) &&
     rlang::is_list(dots[[caller_function]])) {
    dots <- utils::modifyList(
      x = dots,
      val = dots[[caller_function]], keep.null = TRUE
    )
    dots[[caller_function]] <- NULL
  }

  dots_nms <- names(dots)
  dots <- lapply(seq_along(dots), function(i) {
    if(!any(c("call", "...", "chapter_overview", "path") == names(dots)[i])) {
      out <- eval(dots[[i]])
    } else {
      out <- dots[[i]]
    }
    if(names(dots)[i] %in% c("data_label", "showNA")) out <- out[1]
    out
  })
  names(dots) <- dots_nms
  dots
}
