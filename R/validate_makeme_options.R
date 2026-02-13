validate_makeme_options <- function(params) {
  unwanted_args <- names(params)[!names(params) %in% c(names(formals(makeme)))]
  if (length(unwanted_args) > 0) {
    cli::cli_abort("{.arg {unwanted_args}} are not recognized valid arguments.")
  }

  # Safely evaluate defaults for makeme() formals.
  # Some formals (e.g. data, dep, indep, ...) have no default value; attempting
  # to eval() those symbols causes an error. We treat missing/unevaluable
  # defaults as NULL which is then overridden by supplied params or validated
  # downstream.
  fm <- formals(makeme)[!names(formals(makeme)) %in% .saros.env$ignore_args]
  env <- lapply(
    fm,
    function(x) {
      tryCatch(
        eval(x, envir = parent.frame()),
        error = function(.) NULL
      )
    }
  )
  check_and_set_default <- function(target, param_name, validation_fun) {
    if (!validation_fun(target[[param_name]])) {
      default <- env[[param_name]]
      cli::cli_warn(paste0(
        "{.arg {param_name}} is invalid (it is {.obj_type_friendly {target[[param_name]]}}, and specified as {target[[param_name]]}). Using default: {default}"
      ))
      default
    } else {
      target[[param_name]]
    }
  }

  arg_params <-
    list(
      # Data frames
      data = list(fun = function(x) {
        inherits(x, "data.frame") || inherits(x, "survey")
      }),

      # Character vectors (not enums)
      type = list(fun = validate_string_rule()),

      # String parameters (possibly NULL)
      mesos_var = list(fun = validate_string_rule(null_ok = TRUE)),
      mesos_group = list(fun = validate_string_rule(null_ok = TRUE)),
      hide_for_all_if_hidden_for_crowd = list(fun = validate_string_rule(null_ok = TRUE)),
      path = list(fun = validate_string_rule(null_ok = TRUE)),
      label_separator = list(fun = function(x) is.null(x) || is.character(x)),
      labels_always_at_top = list(fun = function(x) is.null(x) || is.character(x)),
      labels_always_at_bottom = list(fun = function(x) is.null(x) || is.character(x)),
      font_family = list(fun = validate_string_rule()),
      data_label_decimal_symbol = list(fun = validate_string_rule()),

      # Boolean
      require_common_categories = list(fun = validate_bool_rule()),
      simplify_output = list(fun = validate_bool_rule()),
      descend = list(fun = validate_bool_rule()),
      vertical = list(fun = validate_bool_rule()),
      hide_for_crowd_if_all_na = list(fun = validate_bool_rule()),
      hide_axis_text_if_single_variable = list(fun = validate_bool_rule()),
      totals = list(fun = validate_bool_rule()),
      table_main_question_as_header = list(fun = validate_bool_rule()),
      table_wide = list(fun = validate_bool_rule()),
      add_n_to_label = list(fun = validate_bool_rule()),
      add_n_to_category = list(fun = validate_bool_rule()),

      # Numeric and integer with ranges
      hide_label_if_prop_below = list(fun = validate_double_rule(min = 0, max = 1)),
      n_categories_limit = list(fun = validate_integerish_rule(min = 0)),
      digits = list(fun = validate_integerish_rule(min = 0)),
      label_font_size = list(fun = validate_integerish_rule(min = 0, max = 72)),
      main_font_size = list(fun = validate_integerish_rule(min = 0, max = 72)),
      strip_font_size = list(fun = validate_integerish_rule(min = 0, max = 72)),
      legend_font_size = list(fun = validate_integerish_rule(min = 0, max = 72)),
      strip_width = list(fun = validate_integerish_rule(min = 0, max = 200)),
      plot_height = list(fun = validate_double_rule(min = 0, max = 200)),
      hide_for_crowd_if_valid_n_below = list(fun = validate_integerish_rule()),
      hide_for_crowd_if_category_k_below = list(fun = validate_integerish_rule()),
      hide_for_crowd_if_category_n_below = list(fun = validate_integerish_rule()),
      hide_for_crowd_if_cell_n_below = list(fun = validate_integerish_rule()),

      # Enums
      crowd = list(fun = function(x) {
        is.character(x) && all(x %in% c("target", "others", "all"))
      }),
      data_label = list(fun = function(x) {
        is.character(x) && any(env$data_label == x[1])
      }),
      data_label_position = list(fun = function(x) {
        is.character(x) && any(env$data_label_position == x[1])
      }),
      showNA = list(fun = function(x) {
        is.character(x) && any(env$showNA == x[1])
      }),
      colour_palette = list(fun = function(x) {
        (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)
      }),
      colour_na = list(fun = function(x) {
        (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)
      }),

      # List
      translations = list(fun = function(x) {
        rlang::is_bare_list(x) &&
          all(vapply(x, is.character, logical(1)))
      }) ### SHOULD BE MORE SPECIFIC FOR EACH ITEM?
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
  params$data_label_position <- params$data_label_position[1]

  if (
    any(c("target", "others") %in% params$crowd) &&
      !(rlang::is_string(params$mesos_var) &&
        rlang::is_string(params$mesos_group))
  ) {
    cli::cli_abort(
      "{.arg mesos_var} and {.arg mesos_group} must be specified (as strings) when {.arg crowd} contains {.val 'target'} or {.val 'others'}."
    )
  }

  if (rlang::is_string(params$mesos_var)) {
    if (!any(colnames(params$data) == params$mesos_var)) {
      cli::cli_abort(
        "{.arg mesos_var}: {.arg {params$mesos_var}} not found in data."
      )
    }
    if (all(is.na(params$data[[params$mesos_var]]))) {
      cli::cli_abort("{.arg mesos_var}: All mesos_var entries are NA.")
    }
    if (
      !rlang::is_string(params$mesos_group) ||
        !params$mesos_group %in%
          as.character(unique(params$data[[params$mesos_var]]))
    ) {
      cli::cli_abort(
        "{.arg mesos_group} ({.val {params$mesos_group}}) must be a value found in {params$mesos_var}."
      )
    }
  }

  params
}
