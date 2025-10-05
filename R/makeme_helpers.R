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

#' Normalize Multi-Choice Arguments to Single Values
#'
#' Internal helper function that ensures makeme arguments that might be vectors
#' are normalized to single values by taking the first element.
#'
#' @param args List of makeme function arguments
#'
#' @return Modified args list with normalized single-value arguments:
#'   - `showNA`: First element of showNA vector
#'   - `data_label`: First element of data_label vector  
#'   - `data_label_position`: First element of data_label_position vector
#'   - `type`: First element of evaluated type expression
#'
#' @keywords internal
normalize_makeme_arguments <- function(args) {
  args$showNA <- args$showNA[1]
  args$data_label <- args$data_label[1]
  args$data_label_position <- args$data_label_position[1]
  args$type <- eval(args$type)[1]
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
  if (!args$type %in% c("sigtest_table_html", "chr_table_html")) {
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
  variable_type_dep <- lapply(dep_crwd, function(v) class(subset_data[[v]])) |>
    unlist()
  variable_type_indep <- if (length(indep_crwd) > 0) {
    lapply(indep_crwd, function(v) class(subset_data[[v]])) |> unlist()
  } else {
    character(0)
  }

  list(
    dep = variable_type_dep,
    indep = variable_type_indep
  )
}

# Helper function: Generate appropriate data summary based on variable types
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
      sort_by = args$sort_by,
      descend = args$descend,
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

# Helper function: Reorder crowd array based on hide_for_all_crowds_if_hidden_for_crowd
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

# Helper function: Initialize crowd-based filtering data structures
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

# Helper function: Process kept_indep_cats_list for global hiding logic
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

# Helper function: Validate and initialize arguments
initialize_arguments <- function(data, dep_pos, indep_pos, args) {
  args$data <- data
  args$dep <- names(dep_pos)
  args$indep <- names(indep_pos)
  args$showNA <- args$showNA[1]
  args$data_label <- args$data_label[1]
  args$type <- eval(args$type)[1]
  args
}

# Helper function: Process crowd settings
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

# Helper function: Handle kept and omitted columns for crowds
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

# Helper function: Summarize data based on type
summarize_data_by_type <- function(
  args,
  subset_data,
  dep_crwd,
  indep_crwd,
  ...
) {
  variable_type_dep <- lapply(args$dep, function(v) class(subset_data[[v]])) |>
    unlist()
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
      sort_by = args$sort_by,
      descend = args$descend,
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

# Helper function: Filter and prepare data for a specific crowd
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

# Helper function: Generate output for a crowd
generate_crowd_output <- function(args, subset_data, dep_crwd, indep_crwd) {
  args <- summarize_data_by_type(args, subset_data, dep_crwd, indep_crwd)
  args$main_question <- get_main_question(
    as.character(unique(args$data_summary[[
      ".variable_label_prefix"
    ]])),
    label_separator = args$label_separator
  )

  if (
    !args$type %in%
      c(
        "sigtest_table_html",
        "int_table_html",
        "int_plot_html",
        "chr_table_html"
      )
  ) {
    args$data_summary <- post_process_makeme_data(
      data = args$data_summary,
      indep = indep_crwd,
      showNA = args$showNA,
      colour_2nd_binary_cat = if (grepl(x = args$type, pattern = "docx")) {
        args$colour_2nd_binary_cat
      }
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

# Helper function: Process data for a single crowd
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
  if (
    !args$type %in% c("sigtest_table_html", "int_table_html", "chr_table_html")
  ) {
    args$data_summary <- post_process_makeme_data(
      data = args$data_summary,
      indep = indep_crwd,
      showNA = args$showNA,
      colour_2nd_binary_cat = if (grepl(x = args$type, pattern = "docx")) {
        args$colour_2nd_binary_cat
      }
    )
  }

  # Prepare arguments for this crowd
  args_crwd <- args
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
# Helper function: Setup and validate makeme arguments
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
  args <- normalize_makeme_arguments(args)

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

# Helper function: Process all crowds and generate output
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
  out <- out[!sapply(out, is.null)]

  # Simplify output if requested
  if (isTRUE(args$simplify_output) && length(out) == 1) {
    out[[1]]
  } else if (length(out) == 0) {
    invisible(data.frame())
  } else {
    out
  }
}

# Helper function: Rename crowd outputs
rename_crowd_outputs <- function(out, translations) {
  for (crwd in names(out)) {
    if (rlang::is_string(translations[[paste0("crowd_", crwd)]])) {
      names(out)[names(out) == crwd] <- translations[[paste0("crowd_", crwd)]]
    }
  }
  out
}
