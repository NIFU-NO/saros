glue_together_range <- function(
  n,
  glue_template_1,
  glue_template_2
) {
  if (is.null(n)) {
    return("")
  }
  n <-
    range(n, na.rm = TRUE) |>
    unique()

  if (all(is.na(n)) || all(n %in% c(Inf, -Inf))) {
    return("")
  }

  template <-
    if (length(n) == 1 && rlang::is_string(glue_template_1)) {
      glue_template_1
    } else if (rlang::is_string(glue_template_2)) {
      glue_template_2
    } else {
      ""
    }

  glue::glue(template)
}

#' Obtain range of N for a given data set and other settings.
#'
#'
#' @param data Dataset
#' @param dep,indep Character vector, names of (in)dependent variables
#' @param crowd String, one of "all", "target" or "others".
#' @param mesos_var Optional, NULL or string specifying name of variable used to
#'   split dataset.
#' @param mesos_group Optional, NULL or string specifying value in `mesos_var`
#' indicating the target group.
#' @param glue_template_1,glue_template_2 String, for the case of a single
#' value (1) or a range with minimum-maximum of values (2).
#'
#' @return Always a string.
#' @keywords internal
n_rng <- function(
  data,
  dep,
  indep = NULL,
  crowd = "all",
  mesos_var = NULL,
  mesos_group = NULL,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
) {
  # Should always return a string, no matter the inputs
  # args <- check_options(call = match.call(),
  #                       ignore_args = .saros.env$ignore_args,
  #                       defaults_env = global_settings_get(fn_name = "n_rng"),
  #                       default_values = formals(n_rng))

  deps <- as.character(unique(dep))
  deps <- deps[!is.na(deps)]
  indeps <- as.character(unique(indep))
  indeps <- indeps[!is.na(indeps)]

  data <- data[
    makeme_keep_rows(
      data = data,
      crwd = crowd,
      mesos_var = mesos_var,
      mesos_group = mesos_group
    ),
    ,
    drop = FALSE
  ]

  n <-
    lapply(deps, function(d) {
      if (length(indeps) > 0) {
        lapply(indeps, function(i) {
          out <-
            dplyr::filter(
              data,
              dplyr::if_all(
                .cols = tidyselect::all_of(c(d, i)),
                .fns = ~ !is.na(.x)
              )
            ) |>
            nrow()
          return(out)
        }) |>
          unlist()
      } else {
        out <-
          dplyr::filter(
            data,
            dplyr::if_all(
              .cols = tidyselect::all_of(c(d)),
              .fns = ~ !is.na(.x)
            )
          ) |>
          nrow()
        return(out)
      }
    }) |>
    unlist()

  glue_together_range(
    n = n,
    glue_template_1 = glue_template_1,
    glue_template_2 = glue_template_2
  )
}

#' Provides a range (or single value) for N in data, given dep and indep
#'
#' @inheritParams n_rng
#' @param dep,indep Tidyselect syntax
#' @return String.
#' @export
#'
#' @examples n_range(data = ex_survey, dep = b_1:b_3, indep = x1_sex)
n_range <- function(
  data,
  dep,
  indep = NULL,
  mesos_var = NULL,
  mesos_group = NULL,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
) {
  dep_enq <- rlang::enquo(arg = dep)
  dep_pos <- tidyselect::eval_select(dep_enq, data = data)
  indep_enq <- rlang::enquo(arg = indep)
  indep_pos <- tidyselect::eval_select(indep_enq, data = data)
  mesos_var_enq <- rlang::enquo(arg = mesos_var)
  mesos_var_pos <- tidyselect::eval_select(mesos_var_enq, data = data)

  args <- check_options(
    call = match.call(),
    ignore_args = .saros.env$ignore_args,
    defaults_env = global_settings_get(fn_name = "n_rng"),
    default_values = formals(n_rng)
  )

  args$data <- data # reinsert after check_options
  args$dep <- names(dep_pos)
  args$indep <- names(indep_pos)
  n_rng(
    data = data,
    dep = args$dep,
    indep = args$indep,
    mesos_var = args$mesos_var,
    mesos_group = args$mesos_group,
    glue_template_1 = args$glue_template_1,
    glue_template_2 = args$glue_template_2
  )
}


#' Obtain range of N for a given `ggobj`.
#'
#'
#' @param ggobj A `ggplot2`-object.
#' @param glue_template_1,glue_template_2 String, for the case of a single
#' value (1) or a range with minimum-maximum of values (2).
#'
#' @return Always a string.
#' @keywords internal
n_rng2 <- function(
  ggobj,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
) {
  if (!ggplot2::is.ggplot(ggobj)) {
    cli::cli_warn("{.arg ggobj} must be a ggplot2-object, returning NULL.")
    return(NULL)
  }
  data <- ggobj$data

  n <- unique(range(data$.count_per_indep_group, na.rm = TRUE))

  glue_together_range(
    n = n,
    glue_template_1 = glue_template_1,
    glue_template_2 = glue_template_2
  )
}

#' Provides a range (or single value) for N in a `ggplot2`-object from `makeme()`
#'
#' @inheritParams n_rng2
#' @return String.
#' @export
#' @examples
#' n_range2(makeme(data = ex_survey, dep = b_1:b_3))
n_range2 <- function(
  ggobj,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
) {
  args <- check_options(
    call = match.call(),
    ignore_args = .saros.env$ignore_args,
    defaults_env = global_settings_get(fn_name = "n_rng"),
    default_values = formals(n_rng)
  )

  n_rng2(
    ggobj = ggobj,
    glue_template_1 = args$glue_template_1,
    glue_template_2 = args$glue_template_2
  )
}
