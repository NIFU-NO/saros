

.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("saros"))) {
    options("saros" = .saros.env$defaults)
  }
  invisible()
}

