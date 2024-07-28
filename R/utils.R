inform_startup <- function(msg, ...) { #
  if (is.null(msg)) {
    return()
  }
  if (isTRUE(getOption("saros.quiet"))) {
    return()
  }

  cli::cli_inform(msg, ..., class = "packageStartupMessage")
}

#' List all packages in saros
#'
#' @param include_self Include saros in the list?
#' @export
#' @examples
#' saros_packages()
saros_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("saros")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "saros")
  }

  names
}

invert <- function(x) { #
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}
