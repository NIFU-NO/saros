summarize_int_cat_data <-
  function(data,
           ...,
           dep = colnames(data),
           indep = NULL,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)

    if(!(inherits(data, what = "data.frame") || !inherits(data, what = "survey"))) {
      cli::cli_abort("{.arg data} should be a data.frame/tibble or survey object, not {.obj_type_friendly {data}}.")
    }

    if(any(dep %in% indep)) {
      cli::cli_abort("Dep column{?s} {.var {invalid_deps}} {?is/are} among indep columns.")
    }
    invalid_deps <- dep[!dep %in% colnames(data)]
    if(length(invalid_deps)>0) {
      cli::cli_abort("Column{?s} {.var {invalid_deps}} {?doesn't/don't} exist.")
    }
    invalid_indeps <- indep[!indep %in% colnames(data)]
    if(length(invalid_indeps)>0) {
      cli::cli_abort("Column{?s} {.var {invalid_indeps}} {?doesn't/don't} exist.")
    }


    # for(d in dep) {
    #   simple_descriptives(data = data,
    #                       y_var = dep,
    #                       x_var = indep,
    #                       na.rm = dots$showNA %in% c("ifany", "always"))
    # }

  }
