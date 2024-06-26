argument_validation_and_insertion <- function(params) {

  unwanted_args <- names(params)[!names(params) %in% c(names(formals(draft_report)),
                                                       .saros.env$element_names_simplified)]
  if(length(unwanted_args) > 0) cli::cli_abort("{.arg {unwanted_args}} are not recognized valid arguments.")

  env <- lapply(formals(draft_report)[!names(formals(draft_report)) %in% .saros.env$ignore_args], eval)
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
    is.numeric(x) && length(x) == 1 && is.finite(x)
  }
  is_bool <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)


  arg_params <-
    list(
      # Data frames
      data = list(fun = function(x) inherits(x, "data.frame") || inherits(x, "survey")),
      chapter_overview = list(fun = function(x) is.null(x) || inherits(x, "data.frame")),

      # Character vectors (not enums)
      label_separator = list(fun = function(x) is.null(x) || is.character(x)),
      name_separator = list(fun = function(x) is.null(x) || is.character(x)),
      mesos_var = list(fun = function(x) is.null(x) || is_string(x)),
      auxiliary_variables = list(fun = function(x) is.null(x) || (is.character(x) && all(x %in% colnames(params$data)))),
      always_show_bi_for_indep = list(fun = function(x) is.null(x) || (is.character(x) && all(x %in% colnames(params$data)))),
      variables_always_at_top = list(fun = function(x) is.null(x) || (is.character(x) && all(x %in% colnames(params$data)))),
      variables_always_at_bottom = list(fun = function(x) is.null(x) || (is.character(x) && all(x %in% colnames(params$data)))),
      variables_show_bi_for_by = list(fun = function(x) is.null(x) || (is.character(x) && all(x %in% colnames(params$data)))),
      path = list(fun = function(x) is_string(x)),

      chapter_yaml_file = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      index_yaml_file = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      report_yaml_file = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      chapter_qmd_start_section_filepath = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      chapter_qmd_end_section_filepath = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      index_qmd_start_section_filepath = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      index_qmd_end_section_filepath = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      report_qmd_start_section_filepath = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      report_qmd_end_section_filepath = list(fun = function(x) is.null(x) || (is_string(x) && file.exists(x))),
      index_filename = list(fun = function(x) is_string(x) || is.null(x)),
      report_filename = list(fun = function(x) is_string(x) || is.null(x)),

      log_file = list(fun = function(x) is.null(x) || is_string(x)),
      font_family = list(fun = is_string),
      data_label_decimal_symbol = list(fun = is_string),

      # List
      translations = list(fun = function(x) rlang::is_bare_list(x) && all(unlist(lapply(x, function(.x) is.character(.x))))), ### SHOULD BE MORE SPECIFIC FOR EACH ITEM?

      # Boolean
      mesos_report = list(fun = is_bool),
      open_after_drafting = list(fun = is_bool),
      return_raw = list(fun = is_bool),
      attach_chapter_dataset = list(fun = is_bool),
      descend = list(fun = is_bool),
      vertical = list(fun = is_bool),
      include_numbers = list(fun = is_bool),
      require_common_categories = list(fun = is_bool),
      hide_chr_for_others = list(fun = is_bool),
      hide_variable_if_all_na = list(fun = is_bool),
      hide_axis_text_if_single_variable = list(fun = is_bool),
      mesos_first = list(fun = is_bool),
      panel_tabset_mesos = list(fun = is_bool),
      totals = list(fun = is_bool),
      pdf = list(fun = is_bool),
      micro = list(fun = is_bool),
      table_main_question_as_header = list(fun = is_bool),
      na_first_in_section = list(fun = is_bool),

      # Numeric and integer
      plot_height_multiplier_per_horizontal_line = list(fun = function(x) (is_scalar_finite_doubleish(x) && x > 0) || is.na(x)),
      plot_height_multiplier_per_vertical_letter = list(fun = function(x) (is_scalar_finite_doubleish(x) && x > 0) || is.na(x)),
      plot_height_multiplier_per_facet = list(fun = function(x) (is_scalar_finite_doubleish(x) && x > 0)),
      plot_height_multiplier_per_legend_line = list(fun = function(x) (is_scalar_finite_doubleish(x) && x > 0)),
      plot_height_fixed_constant = list(fun = function(x) (is_scalar_finite_doubleish(x) && x >= 0) || is.na(x)),
      plot_height_max = list(fun = function(x) is_scalar_finite_doubleish(x) && x > 0),
      plot_height_min = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0),
      vertical_height = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0),
      hide_label_if_prop_below = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
      hide_bi_entry_if_sig_above = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
      hide_result_if_n_below = list(fun = function(x) rlang::is_scalar_integerish(x) && x >= 0),
      single_y_bivariates_if_indep_cats_above = list(fun = function(x) length(x)== 1 && (rlang::is_scalar_integerish(x) && x >= 0) || is.na(x)),
      single_y_bivariates_if_deps_above = list(fun = function(x) length(x)== 1 && (rlang::is_scalar_integerish(x) && x >= 0) || is.na(x)),
      digits = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
      strip_angle = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
      label_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      main_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      strip_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      legend_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      strip_width = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 200),
      strip_angle = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= -360 && x <= 360),
      n_top_bottom = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
      reps = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
      hide_test_if_n_below = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE)),
      png_scale = list(fun = function(x) is_scalar_finite_doubleish(x) && x > 0),
      png_width = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x > 0),
      png_height = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x > 0),
      max_width_file = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 8),
      max_width_obj = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 8),
      max_clean_folder_name = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 8),

      # Enums
      data_label = list(fun = function(x) is.character(x) && any(env$data_label == x[1])),
      organize_by = list(fun = function(x) is.character(x)), # BETTER CHECKS NEEDED
      arrange_section_by = list(fun = function(x) is.null(x) ||
                                  (is.character(x) && all(x %in% .saros.env$refined_chapter_overview_columns)) ||
                                  (is.logical(x) && rlang::is_named(x) && all(names(x) %in% .saros.env$refined_chapter_overview_columns))),
      showNA = list(fun = function(x) is.character(x) && any(env$showNA == x[1])),
      element_names = list(fun = function(x) is.character(x) && all(x %in% env$element_names)),
      serialized_format = list(fun = function(x) is.character(x) && any(env$serialized_format == x[1])),
      tabular_format = list(fun = function(x) is.character(x) && any(env$tabular_format == x[1])),

      # Colour enums
      colour_palette_nominal = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)),
      colour_palette_ordinal = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)),
      colour_na = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x))

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
  params$serialized_format <- params$serialized_format[1]
  params$tabular_format <- params$tabular_format[1]

  check_sort_by(params$sort_by)
  if(is_string(params$mesos_var)) {
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
  if(!all(params$arrange_section_by %in% .saros.env$refined_chapter_overview_columns) &&
     !all(names(params$arrange_section_by %in% .saros.env$refined_chapter_overview_columns))) {
    cli::cli_abort(c("{.arg arrange_section_by} is not valid. Must be one or more of {(.saros.env$refined_chapter_overview_columns)}.",
                     i = "You provided {.arg {params$arrange_section_by}}."))
  }

  if(is.null(params$chapter_overview)) {
    params$chapter_overview <-
      data.frame(chapter = "", dep = "everything()")
  }

  if(params$serialized_format != "rds" &&
     !requireNamespace(params$serialized_format, quietly = TRUE)) {
    cli::cli_abort("Needs {.pkg {params$serialized_format}} to use {.arg serialized_format}={params$serialized_format}: {.run [install.packages(pkg)](install.packages())}")
   }

  pkg <- switch(params$tabular_format,
                "delim" = "utils",
                "xlsx" = "openxlsx",
                "csv" = "readr",
                "tsv" = "readr",
                "sav" = "haven",
                "dta" = "haven")
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort("Needs {.pkg {pkg}} to use {.arg tabular_format}={params$tabular_format}: {.run [install.packages(pkg)](install.packages())}")
  }

  params
}
