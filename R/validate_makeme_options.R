validate_makeme_options <- function(params) {
  unwanted_args <- names(params)[!names(params) %in% c(names(formals(makeme)))]
  if (length(unwanted_args) > 0) cli::cli_abort("{.arg {unwanted_args}} are not recognized valid arguments.")

  env <- lapply(formals(makeme)[!names(formals(makeme)) %in% .saros.env$ignore_args], eval)
  check_and_set_default <- function(target,
                                    param_name,
                                    validation_fun) {
    if (!validation_fun(target[[param_name]])) {
      default <- env[[param_name]]
      cli::cli_warn(paste0("{.arg {param_name}} is invalid (it is {.obj_type_friendly {target[[param_name]]}}, and specified as {target[[param_name]]}). Using default: {default}"))
      default
    } else {
      target[[param_name]]
    }
  }
  is_scalar_finite_doubleish <- function(x) {
    is.numeric(x) && length(x) == 1 && is.finite(x)
  }
  is_bool <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)


  arg_params <-
    list(
      # Data frames
      data = list(fun = function(x) inherits(x, "data.frame") || inherits(x, "survey")),

      # Character vectors (not enums)
      type = list(fun = function(x) rlang::is_string(x)),

      # For mesos, see also checks at the bottom of function
      mesos_var = list(fun = function(x) is.null(x) || rlang::is_string(x)),
      mesos_group = list(fun = function(x) is.null(x) || rlang::is_string(x)),
      hide_for_all_if_hidden_for_crowd = list(fun = function(x) is.null(x) || rlang::is_string(x)),
      path = list(fun = function(x) is.null(x) || rlang::is_string(x)),
      label_separator = list(fun = function(x) is.null(x) || is.character(x)),
      labels_always_at_top = list(fun = function(x) is.null(x) || is.character(x)),
      labels_always_at_bottom = list(fun = function(x) is.null(x) || is.character(x)),
      font_family = list(fun = function(x) rlang::is_string(x)),
      data_label_decimal_symbol = list(fun = function(x) rlang::is_string(x)),

      # Boolean
      require_common_categories = list(fun = is_bool),
      simplify_output = list(fun = is_bool),
      descend = list(fun = is_bool),
      vertical = list(fun = is_bool),
      hide_for_crowd_if_all_na = list(fun = is_bool),
      hide_axis_text_if_single_variable = list(fun = is_bool),
      totals = list(fun = is_bool),
      table_main_question_as_header = list(fun = is_bool),
      hide_for_crowd_if_all_na = list(fun = is_bool),
      table_wide = list(fun = is_bool),
      add_n_to_label = list(fun = is_bool),
      add_n_to_category = list(fun = is_bool),

      # Numeric and integer
      hide_label_if_prop_below = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
      n_categories_limit = list(fun = function(x) rlang::is_integerish(x, n = 1) && x >= 0),
      digits = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
      label_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      main_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      strip_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      legend_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      strip_width = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 200),
      plot_height = list(fun = function(x) rlang::is_double(x, n = 1, finite = TRUE) && x >= 0 && x <= 200),
      hide_for_crowd_if_valid_n_below = list(fun = function(x) rlang::is_integerish(x, n = 1)),
      hide_for_crowd_if_category_k_below = list(fun = function(x) rlang::is_integerish(x, n = 1)),
      hide_for_crowd_if_category_n_below = list(fun = function(x) rlang::is_integerish(x, n = 1)),
      hide_for_crowd_if_cell_n_below = list(fun = function(x) rlang::is_integerish(x, n = 1)),

      # Enums
      crowd = list(fun = function(x) is.character(x) && all(x %in% c("target", "others", "all"))),
      data_label = list(fun = function(x) is.character(x) && any(env$data_label == x[1])),
      showNA = list(fun = function(x) is.character(x) && any(env$showNA == x[1])),
      colour_palette = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)),
      colour_na = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)),

      # List
      translations = list(fun = function(x) rlang::is_bare_list(x) && all(unlist(lapply(x, function(.x) is.character(.x))))) ### SHOULD BE MORE SPECIFIC FOR EACH ITEM?
    )

  for (par in names(arg_params)) {
    params[[par]] <-
      check_and_set_default(
        target = params,
        param_name = par,
        validation_fun = arg_params[[par]]$fun
      )
  }

  params$type <- params$type[1]
  params$showNA <- params$showNA[1]
  params$data_label <- params$data_label[1]


  if (any(c("target", "others") %in% params$crowd) &&
    !(rlang::is_string(params$mesos_var) && rlang::is_string(params$mesos_group))) {
    cli::cli_abort("{.arg mesos_var} and {.arg mesos_group} must be specified (as strings) when {.arg crowd} contains {.val 'target'} or {.val 'others'}.")
  }

  if (rlang::is_string(params$mesos_var)) {
    if (!any(colnames(params$data) == params$mesos_var)) {
      cli::cli_abort("{.arg mesos_var}: {.arg {params$mesos_var}} not found in data.")
    }
    if (all(is.na(params$data[[params$mesos_var]]))) {
      cli::cli_abort("{.arg mesos_var}: All mesos_var entries are NA.")
    }
    if (!rlang::is_string(params$mesos_group) || !params$mesos_group %in% as.character(unique(params$data[[params$mesos_var]]))) {
      cli::cli_abort("{.arg mesos_group} ({.val {params$mesos_group}}) must be a value found in {params$mesos_var}.")
    }
  }

  # labels_always_at_top_invalid <- params$labels_always_at_top[!params$labels_always_at_top %in% colnames(params$data)]
  # if (length(labels_always_at_top_invalid) > 0) {
  #   cli::cli_warn(c("{.arg labels_always_at_top} contains variables not found in {.arg data}:",
  #     i = "{.val {labels_always_at_top_invalid}}."
  #   ))
  # }
  # labels_always_at_bottom_invalid <- params$labels_always_at_bottom[!params$labels_always_at_bottom %in% colnames(params$data)]
  # if (length(labels_always_at_bottom_invalid) > 0) {
  #   cli::cli_warn(c("{.arg labels_always_at_bottom} contains variables not found in {.arg data}:",
  #     i = "{.val {labels_always_at_bottom_invalid}}."
  #   ))
  # }


  params
}
