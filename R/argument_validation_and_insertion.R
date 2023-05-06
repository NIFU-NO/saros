argument_validation_and_insertion <- function(params) {
  check_and_set_default <- function(target, param_name, validation_fun, env = .saros.env$defaults) {
    if (!validation_fun(target[[param_name]])) {
      default <- env[[param_name]]
      cli::cli_warn(paste0("{.arg {param_name}} is invalid (it is {.obj_type_friendly {target[[param_name]]}}), using default: {default}"))
      target[[param_name]] <- default
    }
  }
  is_scalar_finite_doubleish <- function(x) {
    rlang::is_double(x, n = 1, finite = TRUE) || rlang::is_integerish(x, n = 1, finite = TRUE)
  }

  arg_params <- list(
    label_separator = list(fun = function(x) rlang::is_null(x) || rlang::is_string(x)),
    name_separator = list(fun = function(x) rlang::is_null(x) || rlang::is_string(x)),
    element_names = list(fun = function(x) rlang::is_character(x) && all(x %in% .saros.env$defaults$element_names)),
    report_yaml_file = list(fun = function(x) rlang::is_null(x) || (rlang::is_string(x) && fs::file_exists(x))),
    chapter_yaml_file = list(fun = function(x) rlang::is_null(x) || (rlang::is_string(x) && fs::file_exists(x))),
    index_filename = list(fun = rlang::is_string),
    translations = list(fun = function(x) rlang::is_bare_list(x) && all(purrr::map_lgl(x, ~is.character(.x)))) ### SHOULD BE MORE SPECIFIC FOR EACH ITEM?
  )

  element_arg_params <- list(
    return_raw = list(fun = rlang::is_bool),
    descend = list(fun = rlang::is_bool),
    vertical = list(fun = rlang::is_bool),
    include_numbers = list(fun = rlang::is_bool),
    require_common_categories = list(fun = rlang::is_bool),
    plot_height_multiplier = list(fun = function(x) is_scalar_finite_doubleish(x) && x > 0),
    plot_height_fixed_constant = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0),
    hide_label_if_prop_below = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
    hide_bi_entry_if_sig_above = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
    digits = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
    label_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
    main_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
    n_top_bottom = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
    reps = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
    hide_test_if_n_below = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE)),
    png_scale = list(fun = function(x) is_scalar_finite_doubleish(x) && x > 0),
    png_width = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x > 0),
    png_height = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x > 0),
    max_width_file = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 8),
    max_width_obj = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 8),
    font_family = list(fun = rlang::is_string),
    data_label_decimal_symbol = list(fun = rlang::is_string),
    data_label = list(fun = function(x) rlang::is_character(x) && any(.saros.env$defaults$element_args$data_label == x[1])),
    showNA = list(fun = function(x) rlang::is_character(x) && any(.saros.env$defaults$element_args$showNA == x[1]))
  )

  for (par in names(arg_params)) {
    check_and_set_default(target = params, param_name = par,
                          validation_fun = arg_params[[par]]$fun,
                          env = .saros.env$defaults)
  }

  for (par in names(element_arg_params)) {
    check_and_set_default(target = params$element_args, param_name = par,
                          validation_fun = element_arg_params[[par]]$fun,
                          env = .saros.env$defaults$element_args)
  }

  params$element_args$data_label <- params$element_args$data_label[1]
  params$element_args$showNA <- params$element_args$showNA[1]
  check_sort_by(params$element_args$sort_by)

  params
}
