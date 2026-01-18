#' Evaluate Variable Selection
#'
#' Internal helper function that evaluates tidyselect expressions for dependent
#' and independent variables, returning their column positions in the data frame.
#'
#' @param data A data frame containing the variables to be selected
#' @param dep Quosure or tidyselect expression for dependent variables
#' @param indep Quosure or tidyselect expression for independent variables
#'
#' @return A list with two named elements:
#'   - `dep_pos`: Named integer vector of column positions for dependent variables
#'   - `indep_pos`: Named integer vector of column positions for independent variables
#'
#' @keywords internal
evaluate_variable_selection <- function(data, dep, indep) {
  dep_enq <- rlang::enquo(arg = dep)
  dep_pos <- tidyselect::eval_select(dep_enq, data = data)
  indep_enq <- rlang::enquo(arg = indep)
  indep_pos <- tidyselect::eval_select(indep_enq, data = data)
  list(dep_pos = dep_pos, indep_pos = indep_pos)
}

#' Resolve Variable Overlaps Between Dependent and Independent Variables
#'
#' Internal helper function that handles cases where variables are selected for
#' both dependent and independent roles. Automatically removes overlapping variables
#' from the dependent list and provides user feedback.
#'
#' @param dep Character vector of dependent variable names
#' @param indep Character vector of independent variable names
#'
#' @return Character vector of dependent variable names with overlaps removed
#'
#' @details
#' If overlapping variables are found:
#' - Informs user about the overlap via cli::cli_inform()
#' - Removes overlapping variables from dep vector
#' - Throws error if no dependent variables remain after removal
#'
#' @keywords internal
resolve_variable_overlaps <- function(dep, indep) {
  if (length(indep) > 0 && length(dep) > 0) {
    overlapping_vars <- intersect(dep, indep)
    if (length(overlapping_vars) > 0) {
      cli::cli_inform(c(
        "i" = "Variables {.var {overlapping_vars}} were selected for both dep and indep.",
        "i" = "Automatically excluding them from dep to prevent conflicts."
      ))
      dep <- setdiff(dep, indep)

      # Check if we have any dep variables left
      if (length(dep) == 0) {
        cli::cli_abort(c(
          "x" = "After removing overlapping variables, no dependent variables remain.",
          "i" = "Please adjust your dep selection to exclude indep variables, e.g., {.code dep = c(where(~is.factor(.x)), -{indep})}"
        ))
      }
    }
  }
  dep
}

#' Auto-Detect Appropriate Output Type Based on Variable Types
#'
#' Internal helper function that determines the most appropriate output type
#' based on the classes of dependent and independent variables.
#'
#' @param data Data frame to analyze
#' @param dep Character vector of dependent variable names
#' @param indep Character vector of independent variable names (can be NULL/empty)
#'
#' @return Character string with the detected type:
#'   - `"int_plot_html"` for numeric/integer dependent variables
#'   - `"chr_table_html"` for single character dependent variable
#'   - `"cat_plot_html"` for factor/ordered dependent variables or multiple character variables
#'
#' @keywords internal
auto_detect_makeme_type <- function(data, dep, indep = NULL) {
  # Detect types of dependent variables
  dep_types <- vapply(
    dep,
    function(v) class(data[[v]])[1],
    character(1),
    USE.NAMES = FALSE
  )

  # Check if all deps are numeric/integer
  if (all(dep_types %in% c("integer", "numeric", "double"))) {
    return("int_plot_html")
  }

  # Check for single character variable - use chr_table_html
  if (length(dep) == 1 && dep_types[1] == "character") {
    return("chr_table_html")
  }

  # Check if all deps are categorical (factor/ordered/character)
  if (all(dep_types %in% c("factor", "ordered", "character"))) {
    return("cat_plot_html")
  }

  # Mixed types - provide informative error
  numeric_vars <- dep[dep_types %in% c("integer", "numeric", "double")]
  categorical_vars <- dep[dep_types %in% c("factor", "ordered", "character")]

  cli::cli_abort(c(
    "x" = "Cannot auto-detect type: dependent variables have mixed types.",
    "i" = "Numeric variables: {.val {numeric_vars}}",
    "i" = "Categorical variables: {.val {categorical_vars}}",
    "!" = "Please specify {.arg type} explicitly:",
    " " = "- Use {.code type = 'int_plot_html'} for numeric variables",
    " " = "- Use {.code type = 'cat_plot_html'} for categorical variables"
  ))
}

#' Normalize Multi-Choice Arguments to Single Values
#'
#' Internal helper function that ensures makeme arguments that might be vectors
#' are normalized to single values by taking the first element.
#'
#' @param args List of makeme function arguments
#' @param data Data frame being analyzed (needed for auto type detection)
#'
#' @return Modified args list with normalized single-value arguments:
#'   - `showNA`: First element of showNA vector
#'   - `data_label`: First element of data_label vector
#'   - `data_label_position`: First element of data_label_position vector
#'   - `type`: Auto-detected type if "auto", otherwise first element of evaluated type expression
#'
#' @keywords internal
normalize_makeme_arguments <- function(args, data = NULL) {
  args$showNA <- args$showNA[1]
  args$data_label <- args$data_label[1]
  args$data_label_position <- args$data_label_position[1]

  # Handle type - auto-detect if "auto"
  type_value <- eval(args$type)[1]
  if (identical(type_value, "auto")) {
    if (is.null(data)) {
      cli::cli_abort(c(
        "x" = "Cannot auto-detect type without data.",
        "i" = "This is an internal error - please report it."
      ))
    }
    args$type <- auto_detect_makeme_type(data, args$dep, args$indep)
  } else {
    args$type <- type_value
  }

  args
}

#' Perform Type-Specific Validation Checks
#'
#' Internal helper function that validates arguments based on the specific
#' output type requested. Different types have different constraints.
#'
#' @param args List of makeme function arguments
#' @param data Data frame being analyzed
#' @param indep Character vector of independent variable names
#' @param dep_pos Named integer vector of dependent variable positions
#'
#' @return NULL (function used for side effects - validation errors)
#'
#' @details
#' Current type-specific validations:
#' - `chr_table_html`: Requires exactly one dependent variable
#'
#' @keywords internal
validate_type_specific_constraints <- function(args, data, indep, dep_pos) {
  # chr_table_html only supports single dependent variable
  if (args$type %in% c("chr_table_html") && length(args$dep) > 1) {
    cli::cli_abort(c(
      "x" = "`type = 'chr_table_html'` only supports a single dependent variable.",
      "i" = "You supplied {length(args$dep)} dependent variables."
    ))
  }

  # Some types require additional validation checks
  if (!args$type %in% .saros.env$types_skip_multiple_indep_validation) {
    check_multiple_indep(data, indep = {{ indep }})
    check_category_pairs(data = data, cols_pos = c(dep_pos))
  }

  invisible(TRUE)
}

#' Detect Variable Types for Dependent and Independent Variables
#'
#' Internal helper function that examines the class of variables in the subset
#' data to determine their types (factor, numeric, character, etc.).
#'
#' @param subset_data Data frame subset containing the relevant variables
#' @param dep_crwd Character vector of dependent variable names for current crowd
#' @param indep_crwd Character vector of independent variable names for current crowd
#'
#' @return List with two elements:
#'   - `dep`: Character vector of classes for dependent variables
#'   - `indep`: Character vector of classes for independent variables (empty if none)
#'
#' @keywords internal
detect_variable_types <- function(subset_data, dep_crwd, indep_crwd) {
  variable_type_dep <- vapply(
    dep_crwd,
    function(v) class(subset_data[[v]])[1],
    character(1),
    USE.NAMES = FALSE
  )
  variable_type_indep <- if (length(indep_crwd) > 0) {
    vapply(
      indep_crwd,
      function(v) class(subset_data[[v]])[1],
      character(1),
      USE.NAMES = FALSE
    )
  } else {
    character(0)
  }

  list(
    dep = variable_type_dep,
    indep = variable_type_indep
  )
}

#' Generate Appropriate Data Summary Based on Variable Types
#'
#' Internal helper function that routes to the appropriate data summarization
#' function based on the detected variable types (categorical vs continuous).
#'
#' @param variable_types List with dep and indep variable type information
#' @param subset_data Data frame subset for the current crowd
#' @param dep_crwd Character vector of dependent variable names for current crowd
#' @param indep_crwd Character vector of independent variable names for current crowd
#' @param args List of makeme function arguments
#' @param ... Additional arguments passed to summarization functions
#'
#' @return Data summary object (type depends on variable types):
#'   - For integer/numeric dep + factor/character indep: calls summarize_int_cat_data()
#'   - For factor/character dep: calls summarize_cat_cat_data()
#'   - For mixed types: throws error
#'
#' @keywords internal
generate_data_summary <- function(
  variable_types,
  subset_data,
  dep_crwd,
  indep_crwd,
  args,
  ...
) {
  # Future: switch or S3
  if (
    all(variable_types$dep %in% c("integer", "numeric")) &&
      (all(variable_types$indep %in% c("factor", "ordered", "character")) ||
        length(indep_crwd) == 0)
  ) {
    summarize_int_cat_data(
      data = subset_data,
      dep = dep_crwd,
      indep = indep_crwd,
      ...
    )
  } else if (all(variable_types$dep %in% c("factor", "ordered", "character"))) {
    summarize_cat_cat_data(
      data = subset_data,
      dep = dep_crwd,
      indep = indep_crwd,
      ...,
      label_separator = args$label_separator,
      showNA = args$showNA,
      totals = args$totals,
      sort_dep_by = args$sort_dep_by,
      sort_indep_by = args$sort_indep_by,
      descend = args$descend,
      descend_indep = args$descend_indep,
      data_label = args$data_label,
      digits = args$digits,
      add_n_to_dep_label = args$add_n_to_dep_label,
      add_n_to_indep_label = args$add_n_to_indep_label,
      add_n_to_label = args$add_n_to_label,
      add_n_to_category = args$add_n_to_category,
      hide_label_if_prop_below = args$hide_label_if_prop_below,
      data_label_decimal_symbol = args$data_label_decimal_symbol,
      categories_treated_as_na = args$categories_treated_as_na,
      labels_always_at_bottom = args$labels_always_at_bottom,
      labels_always_at_top = args$labels_always_at_top,
      translations = args$translations
    )
  } else {
    cli::cli_abort(c(
      "You have provided a mix of categorical and continuous variables.",
      "I do not know what to do with that!"
    ))
  }
}

#' Reorder Crowd Array Based on Hide Settings
#'
#' Internal helper function that reorders the crowd array to prioritize crowds
#' specified in hide_for_all_crowds_if_hidden_for_crowd, ensuring they are
#' processed first to determine variable exclusions early.
#'
#' @param crowd Character vector of crowd identifiers
#' @param hide_for_all_crowds_if_hidden_for_crowd Character vector of crowd identifiers
#'   that should be processed first to determine global exclusions
#'
#' @return Character vector with reordered crowd identifiers:
#'   - Priority crowds first (those in hide_for_all_crowds_if_hidden_for_crowd)
#'   - Remaining crowds after
#'
#' @keywords internal
reorder_crowd_array <- function(
  crowd,
  hide_for_all_crowds_if_hidden_for_crowd
) {
  # Set hide_for_all_crowds_if_hidden_for_crowd first to get its excluded variables early
  # This only happens if hide_for_all_crowds_if_hidden_for_crowd are in the set of crowd.
  c(
    hide_for_all_crowds_if_hidden_for_crowd[
      hide_for_all_crowds_if_hidden_for_crowd %in% crowd
    ],
    crowd[
      !crowd %in%
        hide_for_all_crowds_if_hidden_for_crowd[
          hide_for_all_crowds_if_hidden_for_crowd %in% crowd
        ]
    ]
  )
}

#' Initialize Crowd-Based Filtering Data Structures
#'
#' Internal helper function that sets up the data structures needed for
#' crowd-based filtering and processing of variables and categories.
#'
#' @param crowd Character vector of crowd identifiers
#' @param args List of makeme function arguments
#'
#' @return List with three named elements:
#'   - `kept_cols_list`: Named list of kept column information for each crowd
#'   - `omitted_cols_list`: Named list of omitted variables for each crowd
#'   - `kept_indep_cats_list`: Named list of kept independent categories for each crowd
#'
#' @details
#' For each crowd, this function calls keep_cols() and keep_indep_cats() to
#' determine which variables and categories should be retained based on the
#' various hiding criteria (NA values, sample sizes, etc.).
#'
#' @keywords internal
initialize_crowd_filtering <- function(crowd, args) {
  kept_cols_list <- rlang::set_names(
    vector(mode = "list", length = length(crowd)),
    crowd
  )
  omitted_cols_list <- rlang::set_names(
    vector(mode = "list", length = length(crowd)),
    crowd
  )
  kept_indep_cats_list <- rlang::set_names(
    vector(mode = "list", length = length(crowd)),
    crowd
  )

  for (crwd in names(kept_cols_list)) {
    kept_cols_tmp <-
      keep_cols(
        data = args$data,
        dep = args$dep,
        indep = args$indep,
        crowd = crwd,
        mesos_var = args$mesos_var,
        mesos_group = args$mesos_group,
        hide_for_crowd_if_all_na = args$hide_for_crowd_if_all_na,
        hide_for_crowd_if_valid_n_below = args$hide_for_crowd_if_valid_n_below,
        hide_for_crowd_if_category_k_below = args$hide_for_crowd_if_category_k_below,
        hide_for_crowd_if_category_n_below = args$hide_for_crowd_if_category_n_below,
        hide_for_crowd_if_cell_n_below = args$hide_for_crowd_if_cell_n_below
      )
    omitted_cols_list[[crwd]] <- kept_cols_tmp[["omitted_vars"]]

    kept_indep_cats_list[[crwd]] <-
      keep_indep_cats(
        data = kept_cols_tmp[["data"]],
        indep = args$indep
      )
  }

  list(
    kept_cols_list = kept_cols_list,
    omitted_cols_list = omitted_cols_list,
    kept_indep_cats_list = kept_indep_cats_list
  )
}

#' Process Independent Categories for Global Hiding Logic
#'
#' Internal helper function that applies global hiding logic to independent
#' variable categories based on the hide_for_all_crowds_if_hidden_for_crowd setting.
#'
#' @param kept_indep_cats_list Named list of kept independent categories for each crowd
#' @param hide_for_all_crowds_if_hidden_for_crowd Character vector of crowd identifiers
#'   that determine global category exclusions
#'
#' @return Modified kept_indep_cats_list with global hiding logic applied:
#'   - For crowds not in hide_for_all_crowds_if_hidden_for_crowd: only categories
#'     that were kept in the priority crowds are retained
#'   - For priority crowds: original category lists are preserved
#'
#' @keywords internal
process_global_indep_categories <- function(
  kept_indep_cats_list,
  hide_for_all_crowds_if_hidden_for_crowd
) {
  lapply(rlang::set_names(names(kept_indep_cats_list)), function(crwd) {
    lapply(
      rlang::set_names(names(kept_indep_cats_list[[crwd]])),
      function(x) {
        if (
          is.character(hide_for_all_crowds_if_hidden_for_crowd) &&
            !crwd %in% hide_for_all_crowds_if_hidden_for_crowd
        ) {
          kept_globally <-
            kept_indep_cats_list[
              hide_for_all_crowds_if_hidden_for_crowd
            ] |>
            unlist() |>
            unique()

          kept_indep_cats_list[[crwd]][[x]][
            kept_indep_cats_list[[crwd]][[x]] %in%
              kept_globally
          ]
        } else {
          kept_indep_cats_list[[crwd]][[x]]
        }
      }
    )
  })
}

#' Validate and Initialize Arguments
#'
#' Internal helper function that finalizes the arguments list by adding
#' resolved variable names and normalizing multi-value arguments.
#'
#' @param data Data frame being analyzed
#' @param dep_pos Named integer vector of dependent variable positions
#' @param indep_pos Named integer vector of independent variable positions
#' @param args List of makeme function arguments
#'
#' @return Modified args list with additional elements:
#'   - `data`: The input data frame
#'   - `dep`: Character vector of dependent variable names (from dep_pos)
#'   - `indep`: Character vector of independent variable names (from indep_pos)
#'   - Normalized single-value arguments (showNA, data_label, type)
#'
#' @keywords internal
initialize_arguments <- function(data, dep_pos, indep_pos, args) {
  args$data <- data
  args$dep <- names(dep_pos)
  args$indep <- names(indep_pos)
  args$showNA <- args$showNA[1]
  args$data_label <- args$data_label[1]
  args$type <- eval(args$type)[1]
  args
}

#' Process Crowd Settings
#'
#' Internal helper function that reorders the crowd array to ensure priority
#' crowds (specified in hide_for_all_crowds_if_hidden_for_crowd) are processed first.
#'
#' @param args List of makeme function arguments
#'
#' @return Modified args list with reordered crowd vector:
#'   - Priority crowds (in hide_for_all_crowds_if_hidden_for_crowd) first
#'   - Remaining crowds after
#'   - This ensures global hiding logic is applied correctly
#'
#' @keywords internal
process_crowd_settings <- function(args) {
  args$crowd <- c(
    args$hide_for_all_crowds_if_hidden_for_crowd[
      args$hide_for_all_crowds_if_hidden_for_crowd %in% args$crowd
    ],
    args$crowd[
      !args$crowd %in%
        args$hide_for_all_crowds_if_hidden_for_crowd[
          args$hide_for_all_crowds_if_hidden_for_crowd %in% args$crowd
        ]
    ]
  )
  args
}

#' Handle Kept and Omitted Columns for Crowds
#'
#' Internal helper function that processes the kept and omitted column information
#' for crowd-based filtering and applies global hiding logic.
#'
#' @param args List of makeme function arguments
#' @param kept_cols_list Named list of kept column information for each crowd
#' @param omitted_cols_list Named list of omitted variables for each crowd
#'
#' @return List containing processed crowd column information with global
#'   hiding logic applied based on hide_for_all_crowds_if_hidden_for_crowd settings
#'
#' @keywords internal
handle_crowd_columns <- function(
  args,
  kept_cols_list,
  omitted_cols_list,
  kept_indep_cats_list
) {
  for (crwd in names(kept_cols_list)) {
    kept_cols_tmp <- keep_cols(
      data = args$data,
      dep = args$dep,
      indep = args$indep,
      crowd = crwd,
      mesos_var = args$mesos_var,
      mesos_group = args$mesos_group,
      hide_for_crowd_if_all_na = args$hide_for_crowd_if_all_na,
      hide_for_crowd_if_valid_n_below = args$hide_for_crowd_if_valid_n_below,
      hide_for_crowd_if_category_k_below = args$hide_for_crowd_if_category_k_below,
      hide_for_crowd_if_category_n_below = args$hide_for_crowd_if_category_n_below,
      hide_for_crowd_if_cell_n_below = args$hide_for_crowd_if_cell_n_below
    )
    omitted_cols_list[[crwd]] <- kept_cols_tmp[["omitted_vars"]]
    kept_indep_cats_list[[crwd]] <- keep_indep_cats(
      data = kept_cols_tmp[["data"]],
      indep = args$indep
    )
  }
  list(
    omitted_cols_list = omitted_cols_list,
    kept_indep_cats_list = kept_indep_cats_list
  )
}

#' Summarize Data Based on Variable Types
#'
#' Internal helper function that determines the appropriate data summarization
#' approach based on variable types and calls the corresponding function.
#'
#' @param args List of makeme function arguments
#' @param subset_data Data frame subset for the current crowd
#' @param dep_crwd Character vector of dependent variable names for current crowd
#' @param indep_crwd Character vector of independent variable names for current crowd
#' @param ... Additional arguments passed to summarization functions
#'
#' @return Modified args list with data_summary element added:
#'   - For integer/numeric variables: calls summarize_int_cat_data()
#'   - For factor/ordered variables: calls summarize_cat_cat_data() with full argument set
#'
#' @keywords internal
summarize_data_by_type <- function(
  args,
  subset_data,
  dep_crwd,
  indep_crwd,
  ...
) {
  variable_type_dep <- vapply(
    args$dep,
    function(v) class(subset_data[[v]])[1],
    character(1)
  )
  if (all(variable_type_dep %in% c("integer", "numeric"))) {
    args$data_summary <- rlang::exec(summarize_int_cat_data, !!!args)
  } else if (all(variable_type_dep %in% c("factor", "ordered"))) {
    args$data_summary <- summarize_cat_cat_data(
      data = subset_data,
      dep = dep_crwd,
      indep = indep_crwd,
      ...,
      label_separator = args$label_separator,
      showNA = args$showNA,
      totals = args$totals,
      sort_dep_by = args$sort_dep_by,
      sort_indep_by = args$sort_indep_by,
      descend = args$descend,
      descend_indep = args$descend_indep,
      data_label = args$data_label,
      digits = args$digits,
      add_n_to_dep_label = args$add_n_to_dep_label,
      add_n_to_indep_label = args$add_n_to_indep_label,
      add_n_to_label = args$add_n_to_label,
      add_n_to_category = args$add_n_to_category,
      hide_label_if_prop_below = args$hide_label_if_prop_below,
      data_label_decimal_symbol = args$data_label_decimal_symbol,
      categories_treated_as_na = args$categories_treated_as_na,
      labels_always_at_bottom = args$labels_always_at_bottom,
      labels_always_at_top = args$labels_always_at_top,
      translations = args$translations
    )
  }
  args
}

#' Filter and Prepare Data for a Specific Crowd
#'
#' Internal helper function that filters data for a specific crowd identifier,
#' applying variable exclusions and category filtering as needed.
#'
#' @param data Data frame being analyzed
#' @param args List of makeme function arguments
#' @param crwd Character string identifying the current crowd
#' @param omitted_cols_list Named list of omitted variables for each crowd
#' @param kept_indep_cats_list Named list of kept independent categories for each crowd
#'
#' @return List with subset data and variables for the crowd, or NULL if no data remains:
#'   - `subset_data`: Filtered data frame for the crowd
#'   - `dep_crwd`: Character vector of dependent variables for this crowd
#'   - `indep_crwd`: Character vector of independent variables for this crowd
#'
#' @details
#' Applies the following filtering steps:
#' - Removes omitted variables based on hiding criteria
#' - Filters rows to match crowd membership
#' - Applies independent category filtering if enabled
#' - Returns NULL and warns if no data remains after filtering
#'
#' @keywords internal
filter_crowd_data <- function(
  data,
  args,
  crwd,
  omitted_cols_list,
  kept_indep_cats_list
) {
  omitted_vars_crwd <- omitted_cols_list[
    c(crwd, args$hide_for_all_crowds_if_hidden_for_crowd)
  ] |>
    lapply(FUN = function(x) {
      if ("omitted_vars" %in% names(x)) x["omitted_vars"]
    }) |>
    unlist() |>
    unique()

  dep_crwd <- args$dep[!args$dep %in% omitted_vars_crwd]
  if (length(dep_crwd) == 0) {
    return(NULL)
  }

  indep_crwd <- args$indep
  if (length(indep_crwd) == 0) {
    indep_crwd <- NULL
  }

  subset_data <- dplyr::filter(
    data[, !colnames(data) %in% omitted_vars_crwd, drop = FALSE],
    makeme_keep_rows(
      data = data,
      crwd = crwd,
      mesos_var = args$mesos_var,
      mesos_group = args$mesos_group
    )
  )

  if (isTRUE(args$hide_indep_cat_for_all_crowds_if_hidden_for_crowd)) {
    for (x in indep_crwd) {
      subset_data <- dplyr::filter(
        subset_data,
        as.character(subset_data[[x]]) %in% kept_indep_cats_list[[crwd]][[x]]
      )
    }
  }

  if (nrow(subset_data) == 0) {
    indep_msg <- if (is.character(args$indep)) {
      paste0("indep=", cli::ansi_collapse(args$indep))
    }

    cli::cli_warn(c(
      "No data left to make you {.arg {args$type}} with dep={.arg {args$dep}}, {.arg {indep_msg}}, crowd={.arg {crwd}}.",
      i = "Skipping."
    ))
    return(NULL)
    indep_msg
  }

  list(subset_data = subset_data, dep_crwd = dep_crwd, indep_crwd = indep_crwd)
}

#' Generate Output for a Specific Crowd
#'
#' Internal helper function that generates the final output object for a crowd
#' by processing data summary and calling the appropriate make_content function.
#'
#' @param args List of makeme function arguments
#' @param subset_data Data frame subset for the current crowd
#' @param dep_crwd Character vector of dependent variable names for current crowd
#' @param indep_crwd Character vector of independent variable names for current crowd
#'
#' @return Output object (type depends on makeme type):
#'   - Could be plot, table, or other analysis object
#'   - Generated by make_content() with crowd-specific arguments
#'
#' @details
#' Processing steps:
#' - Summarizes data using summarize_data_by_type()
#' - Sets main question from variable labels
#' - Post-processes data summary for most types
#' - Calls make_content() to generate final output
#'
#' @keywords internal
generate_crowd_output <- function(args, subset_data, dep_crwd, indep_crwd) {
  args <- summarize_data_by_type(args, subset_data, dep_crwd, indep_crwd)
  args$main_question <- get_main_question(
    as.character(unique(args$data_summary[[
      ".variable_label_prefix"
    ]])),
    label_separator = args$label_separator
  )

  if (!args$type %in% .saros.env$types_skip_factor_processing) {
    args$data_summary <- process_indep_factor_levels(
      data = args$data_summary,
      indep = indep_crwd
    )
  }

  args_crwd <- args
  args_crwd$dep <- dep_crwd
  args_crwd$indep <- indep_crwd

  suppressPackageStartupMessages(
    rlang::exec(
      make_content,
      type = args_crwd$type,
      !!!args_crwd[!names(args_crwd) %in% c("type")]
    )
  )
}

#' Process Data for a Single Crowd
#'
#' Internal helper function that handles the complete processing pipeline
#' for a single crowd, from data filtering to final output generation.
#'
#' @param crwd Character string identifying the current crowd
#' @param args List of makeme function arguments
#' @param omitted_cols_list Named list of omitted variables for each crowd
#' @param kept_indep_cats_list Named list of kept independent categories for each crowd
#' @param data Data frame being analyzed
#' @param mesos_var Mesos-level grouping variable
#' @param mesos_group Specific mesos group identifier
#' @param ... Additional arguments passed to data summarization functions
#'
#' @return Final output object for the crowd, or NULL if no data remains:
#'   - Plot, table, or other analysis object depending on type
#'   - NULL if crowd has no valid data after filtering
#'
#' @details
#' Complete processing pipeline:
#' - Calculates omitted variables for the crowd
#' - Filters data by crowd membership and variable exclusions
#' - Applies independent category filtering if enabled
#' - Detects variable types and generates data summary
#' - Performs validation and post-processing
#' - Generates final output via make_content()
#'
#' @keywords internal
process_crowd_data <- function(
  crwd,
  args,
  omitted_cols_list,
  kept_indep_cats_list,
  data,
  mesos_var,
  mesos_group,
  ...
) {
  # Calculate omitted variables for this crowd
  omitted_vars_crwd <- omitted_cols_list[
    c(crwd, args$hide_for_all_crowds_if_hidden_for_crowd)
  ] |>
    lapply(FUN = function(x) x) |>
    unlist() |>
    unique()

  # Get variables for this crowd
  dep_crwd <- args$dep[!args$dep %in% omitted_vars_crwd]
  if (length(dep_crwd) == 0) {
    return(NULL) # Skip this crowd if no dep variables remain
  }

  indep_crwd <- args$indep
  if (length(indep_crwd) == 0) {
    indep_crwd <- NULL
  }

  # Create subset data for this crowd
  subset_data <- dplyr::filter(
    args$data[, !colnames(args$data) %in% omitted_vars_crwd, drop = FALSE],
    makeme_keep_rows(
      data = data,
      crwd = crwd,
      mesos_var = mesos_var,
      mesos_group = mesos_group
    )
  )

  # Apply indep category filtering if needed
  if (isTRUE(args$hide_indep_cat_for_all_crowds_if_hidden_for_crowd)) {
    for (x in indep_crwd) {
      subset_data <- dplyr::filter(
        subset_data,
        as.character(subset_data[[x]]) %in% kept_indep_cats_list[[crwd]][[x]]
      )
    }
  }

  # Check if we have data left
  if (nrow(subset_data) == 0) {
    indep_msg <- if (is.character(args$indep)) {
      paste0("indep=", cli::ansi_collapse(args$indep))
    }

    cli::cli_warn(c(
      "No data left to make you {.arg {args$type}} with dep={.arg {args$dep}}, {.arg {indep_msg}}, crowd={.arg {crwd}}.",
      i = "Skipping."
    ))
    return(NULL)
  }

  # Detect variable types and generate data summary
  variable_types <- detect_variable_types(subset_data, dep_crwd, indep_crwd)
  args$data_summary <- generate_data_summary(
    variable_types,
    subset_data,
    dep_crwd,
    indep_crwd,
    args,
    ...
  )

  # Set main question
  args$main_question <- get_main_question(
    as.character(unique(args$data_summary[[".variable_label_prefix"]])),
    label_separator = args$label_separator
  )

  # Check for duplicated label suffixes
  check_no_duplicated_label_suffix(
    data_summary = args$data_summary,
    error_on_duplicates = args$error_on_duplicates
  )

  # Post-process data summary if needed
  if (!args$type %in% .saros.env$types_skip_factor_processing) {
    args$data_summary <- process_indep_factor_levels(
      data = args$data_summary,
      indep = indep_crwd
    )
  }

  # Prepare arguments for this crowd
  args_crwd <- args
  args_crwd$data <- subset_data # Use filtered data for this crowd
  args_crwd$dep <- dep_crwd
  args_crwd$indep <- indep_crwd

  # Generate final output
  suppressPackageStartupMessages(
    rlang::exec(
      make_content,
      type = args_crwd$type,
      !!!args_crwd[!names(args_crwd) %in% c("type")]
    )
  )
}

# Helper function: Process output results and apply transformations
#' Setup and Validate Makeme Arguments
#'
#' Internal helper function that performs final argument setup and validation
#' before processing. Consolidates variable resolution, normalization, and validation.
#'
#' @param args List of makeme function arguments
#' @param data Data frame being analyzed
#' @param dep_pos Named integer vector of dependent variable positions
#' @param indep_pos Named integer vector of independent variable positions
#' @param indep Independent variable selection (for validation)
#'
#' @return Modified and validated args list ready for processing:
#'   - Variable names resolved from positions
#'   - Overlaps between dep and indep resolved
#'   - Multi-choice arguments normalized
#'   - All validation checks passed
#'   - Crowd array reordered for optimal processing
#'
#' @keywords internal
setup_and_validate_makeme_args <- function(
  args,
  data,
  dep_pos,
  indep_pos,
  indep
) {
  args$data <- data # reinsert after check_options
  args$dep <- names(dep_pos)
  args$indep <- names(indep_pos)

  # Remove indep variables from dep to prevent overlap conflicts
  args$dep <- resolve_variable_overlaps(args$dep, args$indep)

  # Normalize multi-choice arguments to single values
  args <- normalize_makeme_arguments(args, data)

  validate_makeme_options(params = args)

  # Perform type-specific validation checks
  validate_type_specific_constraints(args, data, {{ indep }}, dep_pos)

  # Reorder crowd array and initialize crowd-based filtering
  args$crowd <- reorder_crowd_array(
    args$crowd,
    args$hide_for_all_crowds_if_hidden_for_crowd
  )

  args
}

#' Process All Crowds and Generate Output
#'
#' Internal helper function that iterates through all crowd identifiers
#' and generates the appropriate output for each crowd.
#'
#' @param args Validated list of makeme function arguments
#' @param omitted_cols_list Named list of omitted variables for each crowd
#' @param kept_indep_cats_list Named list of kept independent categories for each crowd
#' @param data Data frame being analyzed
#' @param mesos_var Mesos-level grouping variable
#' @param mesos_group Specific mesos group identifier
#' @param ... Additional arguments passed to process_crowd_data
#'
#' @return Named list of crowd outputs:
#'   - Each element corresponds to one crowd identifier
#'   - Content depends on the specific makeme type requested
#'   - May contain plots, tables, or other analysis objects
#'
#' @keywords internal
process_all_crowds <- function(
  args,
  omitted_cols_list,
  kept_indep_cats_list,
  data,
  mesos_var,
  mesos_group,
  ...
) {
  out <- rlang::set_names(
    vector(mode = "list", length = length(args$crowd)),
    args$crowd
  )

  # Process each crowd
  for (crwd in names(out)) {
    out[[crwd]] <- process_crowd_data(
      crwd,
      args,
      omitted_cols_list,
      kept_indep_cats_list,
      data,
      mesos_var,
      mesos_group,
      ...
    )
  }

  out
}

#' Process Output Results
#'
#' Internal helper function that performs final processing of makeme output,
#' including crowd renaming, NULL removal, and output simplification.
#'
#' @param out Named list of crowd outputs from process_all_crowds
#' @param args List of makeme function arguments (for translations and simplify_output)
#'
#' @return Processed output in final form:
#'   - Crowds renamed according to translations if provided
#'   - NULL results removed
#'   - Single element extracted if simplify_output=TRUE and length=1
#'   - Empty data.frame returned if no valid results
#'   - Otherwise returns the full named list
#'
#' @keywords internal
process_output_results <- function(out, args) {
  # Rename crowds based on translations
  for (crwd in names(out)) {
    if (rlang::is_string(args$translations[[paste0("crowd_", crwd)]])) {
      names(out)[names(out) == crwd] <- args$translations[[paste0(
        "crowd_",
        crwd
      )]]
    }
  }

  # Remove NULL results
  out <- out[!vapply(out, is.null, logical(1))]

  # Simplify output if requested
  if (isTRUE(args$simplify_output) && length(out) == 1) {
    out[[1]]
  } else if (length(out) == 0) {
    invisible(data.frame())
  } else {
    out
  }
}

#' Rename Crowd Outputs
#'
#' Internal helper function that renames crowd identifiers in the output
#' based on provided translations.
#'
#' @param out Named list of crowd outputs
#' @param translations Named list of translation mappings for crowd identifiers
#'
#' @return Modified out list with crowd names translated:
#'   - Names changed according to translations with crowd prefix pattern
#'   - Only string translations are applied
#'   - Untranslated crowds retain original names
#'
#' @keywords internal
rename_crowd_outputs <- function(out, translations) {
  for (crwd in names(out)) {
    if (rlang::is_string(translations[[paste0("crowd_", crwd)]])) {
      names(out)[names(out) == crwd] <- translations[[paste0("crowd_", crwd)]]
    }
  }
  out
}
