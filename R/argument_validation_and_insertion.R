argument_validation_and_insertion <- function(params) {

  unwanted_args <- names(params)[!names(params) %in% c(names(formals(draft_report)),
                                                       .saros.env$element_names_simplified)]
  if(length(unwanted_args) > 0) cli::cli_abort("{.arg {unwanted_args}} are not recognized valid arguments.")

  env <- lapply(formals(draft_report)[!names(formals(draft_report)) %in% c("data", "chapter_overview", "...")], eval)
  check_and_set_default <- function(target,
                                    param_name,
                                    validation_fun) {

    if (!validation_fun(target[[param_name]])) {
      default <- env[[param_name]]
      cli::cli_warn(paste0("{.arg {param_name}} is invalid (it is {.obj_type_friendly {target[[param_name]]}}, and specified as {target[[param_name]]}). Using default: {default}"))
      default
    } else target[[param_name]]
  }
  is_scalar_finite_doubleish <- function(x) {
    rlang::is_double(x, n = 1, finite = TRUE) || rlang::is_integerish(x, n = 1, finite = TRUE)
  }

  arg_params <-
    list(
      data = list(fun = function(x) inherits(x, "data.frame") || inherits(x, "survey")),
      chapter_overview = list(fun = function(x) is.null(x) || inherits(x, "data.frame")),

      label_separator = list(fun = function(x) rlang::is_null(x) || rlang::is_string(x)),
      name_separator = list(fun = function(x) rlang::is_null(x) || rlang::is_string(x)),
      mesos_var = list(fun = function(x) rlang::is_null(x) || rlang::is_string(x)),
      element_names = list(fun = function(x) rlang::is_character(x) && all(x %in% env$element_names)),
      auxiliary_variables = list(fun = function(x) rlang::is_null(x) || (rlang::is_character(x) && all(x %in% colnames(params$data)))),
      always_show_bi_for_indep = list(fun = function(x) rlang::is_null(x) || (rlang::is_character(x) && all(x %in% colnames(params$data)))),
      variables_always_at_top = list(fun = function(x) rlang::is_null(x) || (rlang::is_character(x) && all(x %in% colnames(params$data)))),
      variables_always_at_bottom = list(fun = function(x) rlang::is_null(x) || (rlang::is_character(x) && all(x %in% colnames(params$data)))),
      variables_show_bi_for_by = list(fun = function(x) rlang::is_null(x) || (rlang::is_character(x) && all(x %in% colnames(params$data)))),
      path = list(fun = function(x) rlang::is_null(x) || rlang::is_string(x)),
      index_yaml_file = list(fun = function(x) rlang::is_null(x) || (rlang::is_string(x) && file.exists(x))),
      report_yaml_file = list(fun = function(x) rlang::is_null(x) || (rlang::is_string(x) && file.exists(x))),
      chapter_yaml_file = list(fun = function(x) rlang::is_null(x) || (rlang::is_string(x) && file.exists(x))),
      qmd_start_section_filepath = list(fun = function(x) rlang::is_null(x) || (rlang::is_string(x) && file.exists(x))),
      qmd_end_section_filepath = list(fun = function(x) rlang::is_null(x) || (rlang::is_string(x) && file.exists(x))),
      index_filename = list(fun = rlang::is_string),
      translations = list(fun = function(x) rlang::is_bare_list(x) && all(unlist(lapply(x, function(.x) is.character(.x))))), ### SHOULD BE MORE SPECIFIC FOR EACH ITEM?

      mesos_report = list(fun = rlang::is_bool),
      open_after_drafting = list(fun = rlang::is_bool),
      return_raw = list(fun = rlang::is_bool),
      attach_chapter_dataset = list(fun = rlang::is_bool),
      descend = list(fun = rlang::is_bool),
      vertical = list(fun = rlang::is_bool),
      include_numbers = list(fun = rlang::is_bool),
      require_common_categories = list(fun = rlang::is_bool),
      hide_chr_for_others = list(fun = rlang::is_bool),
      hide_variable_if_all_na = list(fun = rlang::is_bool),
      hide_axis_text_if_single_variable = list(fun = rlang::is_bool),
      mesos_first = list(fun = rlang::is_bool),
      panel_tabset_mesos = list(fun = rlang::is_bool),
      single_y_bivariate_elements = list(fun = rlang::is_bool),
      totals = list(fun = rlang::is_bool),
      pdf = list(fun = rlang::is_bool),
      flexi = list(fun = rlang::is_bool),
      micro = list(fun = rlang::is_bool),

      plot_height_multiplier_per_horizontal_line = list(fun = function(x) (is_scalar_finite_doubleish(x) && x > 0) || is.na(x)),
      plot_height_multiplier_per_vertical_letter = list(fun = function(x) (is_scalar_finite_doubleish(x) && x > 0) || is.na(x)),
      plot_height_fixed_constant = list(fun = function(x) (is_scalar_finite_doubleish(x) && x >= 0) || is.na(x)),
      plot_height_max = list(fun = function(x) is_scalar_finite_doubleish(x) && x > 0),
      plot_height_min = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0),
      vertical_height = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0),
      hide_label_if_prop_below = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
      hide_bi_entry_if_sig_above = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
      digits = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
      strip_angle = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
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
      data_label = list(fun = function(x) rlang::is_character(x) && any(env$data_label == x[1])),
      colour_palette_nominal = list(fun = function(x) (rlang::is_character(x) && all(is_colour(x))) || rlang::is_null(x) || rlang::is_function(x)),
      colour_palette_ordinal = list(fun = function(x) (rlang::is_character(x) && all(is_colour(x))) || rlang::is_null(x) || rlang::is_function(x)),
      colour_na = list(fun = function(x) (rlang::is_character(x) && all(is_colour(x))) || rlang::is_null(x) || rlang::is_function(x)),
      organize_by = list(fun = function(x) rlang::is_character(x)),
      arrange_output_by = list(fun = function(x) rlang::is_character(x) && all(x %in% .saros.env$refined_chapter_overview_columns)),
      showNA = list(fun = function(x) rlang::is_character(x) && any(env$showNA == x[1]))
    )

  for(par in names(arg_params)) {
    # if(par == "return_raw") print(params[[par]])
    params[[par]] <-
      check_and_set_default(target = params,
                            param_name = par,
                            validation_fun = arg_params[[par]]$fun)
  }

  params$data_label <- params$data_label[1]
  params$showNA <- params$showNA[1]
  check_sort_by(params$sort_by)
  if(rlang::is_string(params$mesos_var)) {
    if(!any(colnames(params$data) == params$mesos_var)) {
      cli::cli_abort("{.arg mesos_var}: {.arg {params$mesos_var}} not found in data.")
    }
    if(all(is.na(params$data[[params$mesos_var]]))) {
      cli::cli_abort("{.arg mesos_var}: All mesos_var entries are NA.")
    }
  }


  if(!all(c("chapter", ".element_name") %in% params$organize_by)) {
    cli::cli_abort(c("{.arg organize_by} must contain both {.var {c('chapter', '.element_name')}}.",
                     i = "You provided {.arg {params$organize_by}}."))
  }
  if(!all(params$organize_by %in% .saros.env$refined_chapter_overview_columns)) {
    cli::cli_abort(c("{.arg organize_by} is not valid. Must be one or more of {(.saros.env$refined_chapter_overview_columns)}.",
                     i = "You provided {.arg {params$organize_by}}."))
  }
  if(!all(params$arrange_output_by %in% .saros.env$refined_chapter_overview_columns)) {
    cli::cli_abort(c("{.arg arrange_output_by} is not valid. Must be one or more of {(.saros.env$refined_chapter_overview_columns)}.",
                     i = "You provided {.arg {params$arrange_output_by}}."))
  }

  if(rlang::is_null(params$chapter_overview)) {
    params$chapter_overview <-
      data.frame(chapter = "", dep = "everything()")
  }


  params
}
