summarize_int_cat_data <-
  function(
    data,
    dep = colnames(data),
    indep = NULL,
    sort_indep_by = ".factor_order",
    descend_indep = FALSE,
    ...,
    call = rlang::caller_env()
  ) {
    if (
      !(inherits(data, what = "data.frame") || !inherits(data, what = "survey"))
    ) {
      cli::cli_abort(
        "{.arg data} should be a data.frame/tibble or survey object, not {.obj_type_friendly {data}}."
      )
    }

    if (any(dep %in% indep)) {
      cli::cli_abort(
        "Dep column{?s} {.var {invalid_deps}} {?is/are} among indep columns."
      )
    }
    invalid_deps <- dep[!dep %in% colnames(data)]
    if (length(invalid_deps) > 0) {
      cli::cli_abort("Column{?s} {.var {invalid_deps}} {?doesn't/don't} exist.")
    }
    invalid_indeps <- indep[!indep %in% colnames(data)]
    if (length(invalid_indeps) > 0) {
      cli::cli_abort(
        "Column{?s} {.var {invalid_indeps}} {?doesn't/don't} exist."
      )
    }

    out <- if (length(indep) <= 1) {
      simple_descriptives(
        data = data,
        y_var = dep,
        x_var = indep
      )
    } else {
      lapply(indep, function(i) {
        simple_descriptives(
          data = data,
          y_var = dep,
          x_var = i
        )
      }) |>
        dplyr::bind_rows(.id = indep)
    }

    # `.indep_order` is what makes `sort_indep_by` and `descend_indep` reach
    # the table and plot types; without it they were accepted and discarded
    # (#608).
    add_indep_order_int(
      out,
      indep = if (length(indep) == 1) indep,
      sort_by = sort_indep_by,
      descend = descend_indep,
      call = call
    )
  }
