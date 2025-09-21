#' Save data to a file and return a Markdown link
#'
#' The file is automatically named by a hash of the object, removing the need
#' to come up with unique file names inside a Quarto report. This has the
#' added benefit of reducing storage needs if the objects needing linking to
#' are identical, and all are stored in the same folder. It also allows the user
#' to download multiple files without worrying about accidentally overwriting them.
#'
#'
#' @inheritParams makeme
#'
#' @param data *Data or object*
#'
#'   `<data.frame|tbl|obj>`
#'
#'   Data frame if using a tabular data `save_fn`, or possibly any
#'   R object, if a serializing `save_fn` is provided (e.g. [saveRDS()]).
#'
#' @param folder *Where to store file*
#'
#'   `scalar<character>` // *default:* `"."` (`optional`)
#'
#'   Defaults to same folder.
#'
#' @param file_prefix,file_suffix *File prefix/suffix*
#'
#'   `scalar<character>` // *default:* `""` and `".csv"` (`optional`)
#'
#'   `file_suffix` should include the dot before the extension.
#'
#' @param save_fn *Saving function*
#'
#'   `function` // *default:* `utils::write.csv`
#'
#'   Can be any saving/writing function. However, first argument must be
#'   the object to be saved, and the second must be the path. Hence,
#'   [ggplot2::ggsave()] must be wrapped in another function with `filename` and
#'   `object` swapped. See [ggsaver()] for an example of such a wrapper function.
#'
#' @param link_prefix,link_suffix *Link prefix/suffix*
#'
#'   `scalar<character>` // *default:* `"[download data]("` and `")"`
#'
#'   The stuff that is returned.
#'
#'
#' @return String.
#' @export
#'
#' @examples
#' make_link(mtcars, folder = tempdir())
make_link <- function(
  data,
  folder = NULL,
  file_prefix = NULL,
  file_suffix = ".csv",
  save_fn = utils::write.csv,
  link_prefix = "[download figure data](",
  link_suffix = ")",
  ...
) {
  if (is.null(data)) {
    cli::cli_warn("{.arg data} should not be NULL. Returning NULL.")
    return(NULL)
  }

  UseMethod("make_link", data)
}


#' Save data to a file and return a Markdown link
#'
#' The file is automatically named by a hash of the object, removing the need
#' to come up with unique file names inside a Quarto report. This has the
#' added benefit of reducing storage needs if the objects needing linking to
#' are identical, and all are stored in the same folder. It also allows the user
#' to download multiple files without worrying about accidentally overwriting them.
#'
#'
#' @inheritParams make_link
#' @param separator_list_items *Separator string between multiple list items*
#'
#'   `scalar<character>` // *default:* `". "` (`optional`)
#'
#'
#' @export
make_link.list <- function(
  data,
  ...,
  folder = NULL,
  file_prefix = NULL,
  file_suffix = ".csv",
  save_fn = utils::write.csv,
  link_prefix = "[download figure data](",
  link_suffix = ")",
  separator_list_items = ". "
) {
  # Capture all arguments, including those in `...`
  args <- match.call(expand.dots = TRUE)

  # Remove the `data` argument from the list
  args$data <- NULL

  # Apply `funB` to each element in `data`
  out <- lapply(data, function(x) {
    args$data <- x # Set the current element of the list as `data`
    do.call(make_link, as.list(args[-1])) # Forward all arguments to `funB`
  })

  I(paste0(out, collapse = separator_list_items))
}


#' Save data to a file and return a Markdown link
#'
#' The file is automatically named by a hash of the object, removing the need
#' to come up with unique file names inside a Quarto report. This has the
#' added benefit of reducing storage needs if the objects needing linking to
#' are identical, and all are stored in the same folder. It also allows the user
#' to download multiple files without worrying about accidentally overwriting them.
#'
#'
#' @inheritParams make_link
#'
#'
#' @return String.
#' @export
#'
#' @examples
#' make_link(mtcars, folder = tempdir())
make_link.default <- function(
  data,
  ...,
  folder = NULL,
  file_prefix = NULL,
  file_suffix = ".csv",
  save_fn = utils::write.csv,
  link_prefix = "[download figure data](",
  link_suffix = ")"
) {
  args <-
    check_options(
      call = match.call(),
      ignore_args = .saros.env$ignore_args,
      defaults_env = global_settings_get(fn_name = "make_link"),
      default_values = formals(make_link.default)
    )

  if (!rlang::is_string(args$folder)) {
    args$folder <- "."
  }
  if (!rlang::is_string(args$file_prefix)) {
    args$file_prefix <- ""
  }
  if (!fs::dir_exists(args$folder)) {
    cli::cli_warn(
      "The folder '{args$folder}' does not exist. Attempting to create it."
    )
    fs::dir_create(args$folder)
  }

  path <- fs::path(
    args$folder,
    paste0(args$file_prefix, rlang::hash(data), args$file_suffix)
  )

  # save_fn <- args$save_fn

  tryCatch(
    {
      if (!file.exists(path)) {
        args$save_fn(data, path)
      }
      I(paste0(args$link_prefix, path, args$link_suffix))
    },
    error = function(cnd) {
      # ={data}
      cli::cli_warn(
        c(
          x = "Function {rlang::call_name(quote(safe_fn()))} failed with arguments {.arg path}={path}, {.arg data} is {.obj_type_friendly {data}}."
        ),
        parent = cnd
      )
    }
  )
}
