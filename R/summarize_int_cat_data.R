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

    # `.indep_order` is what makes `sort_indep_by` and `descend_indep` reach
    # the table and plot types; without it they were accepted and discarded
    # (#608). It is added per independent variable, before the blocks are
    # stacked, so each carries its own ordering.
    summarize_one <- function(i) {
      add_indep_order_int(
        simple_descriptives(
          data = data,
          y_var = dep,
          x_var = i
        ),
        indep = i,
        sort_by = sort_indep_by,
        descend = descend_indep,
        call = call
      )
    }

    if (length(indep) <= 1) {
      return(summarize_one(if (length(indep) == 1) indep))
    }

    # One block per independent variable, identified by `.indep_name`. `.id`
    # takes a single column name and the list has to be named for it to record
    # anything useful; passing the whole `indep` vector was invalid (#613).
    lapply(rlang::set_names(indep), summarize_one) |>
      dplyr::bind_rows(.id = ".indep_name")
  }
