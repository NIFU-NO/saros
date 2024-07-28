
.onAttach <- function(...) { #
  attached <- saros_attach()
  if (!is_loading_for_tests()) {
    inform_startup(saros_attach_message(attached))
  }

  if (!is_attached("conflicted") && !is_loading_for_tests()) {
    conflicts <- saros_conflicts()
    inform_startup(saros_conflict_message(conflicts))
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

is_loading_for_tests <- function() { #
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "saros")
}
