#' Conflicts between saros and other packages
#'
#' This function lists all the conflicts between packages in saros
#' and other packages that you have loaded.
#'
#'
#' @export
#' @param only Set this to a character vector to restrict to conflicts only
#'   with these packages.
#' @examples
#' saros_conflicts()
saros_conflicts <- function(only = NULL) { #
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)

  if (!is.null(only)) {
    only <- union(only, core)
    envs <- envs[names(envs) %in% paste0("package:", only)]
  }

  objs <- invert(lapply(envs, function(env) ls(pos = env)))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", saros_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "saros_conflicts")
}

saros_conflict_message <- function(x) { #
  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "saros_conflicts()"
  )

  pkgs <- x |> purrr::map(~ gsub("^package:", "", .x))
  others <- pkgs |> purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(cli::col_blue(.x), "::", .y, "()", collapse = ", ")
  )

  if(length(other_calls)>0) {

    winner <- pkgs |> purrr::map_chr(1)
    funs <- format(paste0(cli::col_blue(winner), "::", cli::col_green(paste0(names(x), "()"))))
    bullets <- paste0(
      cli::col_red(cli::symbol$cross), " ", funs, " masks ", other_calls,
      collapse = "\n"
    )

  conflicted <- paste0(
    cli::col_cyan(cli::symbol$info), " ",
    cli::format_inline("Use the {.href [conflicted package](http://conflicted.r-lib.org/)} to force all conflicts to become errors"
    ))
  return(
    paste0(
      header, "\n",
      bullets, "\n",
      conflicted
    ))

  } else return(header)

}

#' @export
print.saros_conflicts <- function(x, ..., startup = FALSE) { #
  cli::cat_line(saros_conflict_message(x))
  invisible(x)
}

confirm_conflict <- function(packages, name) { #
  # Only look at functions
  objs <- packages |>
    purrr::map(~ get(name, pos = .x)) |>
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()

  packages
}
