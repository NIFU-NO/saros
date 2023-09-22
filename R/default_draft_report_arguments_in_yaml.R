
#' Write Default Arguments for `draft_report()` to YAML-file
#'
#' @param path
#'
#'   `scalar<character>` // Required. *default:* `settings.yaml`
#'
#' @return The defaults as a `yaml`-object.
#' @export
#'
#' @examples
#' write_default_draft_report_args(path=tempfile(fileext=".yaml"))
write_default_draft_report_args <-
  function(path="settings.yaml") {
    args <- formals(draft_report)
    args <- args[!names(args) %in% c("data", "...", "chapter_overview")]
    args <- lapply(args, eval)
    args <- yaml::as.yaml(args)
    dir.create(fs::path_dir(path), recursive = TRUE, showWarnings = FALSE)
    cat(args, file = path)
    path
  }


#' Read Default Arguments for `draft_report()` from YAML-file
#'
#' @param path
#'
#'   `scalar<character>` // Required. *default:* `settings.yaml`
#'
#' @return The defaults as a `yaml`-object.
#' @export
#'
#' @examples
#' path <- write_default_draft_report_args(path=tempfile(fileext=".yaml"))
#' read_default_draft_report_args(path=path)
read_default_draft_report_args <-
  function(path="settings.yaml") {
    x <- yaml::read_yaml(file = as.character(path))
    x$translations <- unlist(x$translations, recursive = FALSE)
    x
  }
