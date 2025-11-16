# Helper function to create overarching factor variable for the response categories column
category_var_as_fct <-
  function(data_summary, fct_unions) {
    data_summary$.category <- factor(
      x = data_summary$.category,
      levels = fct_unions,
      labels = fct_unions,
      exclude = NULL
    )
    data_summary
  }


mutate_data_label <-
  function(
    data,
    data_label = "count",
    digits = 1,
    hide_label_if_prop_below = 0,
    decimal_symbol = "."
  ) {
    percent_siblings <- any(c("percentage", "percentage_bare") == data_label)
    prop_family <- any(
      c("percentage", "percentage_bare", "proportion") == data_label
    )
    stat_family <- any(c("mean", "median") == data_label)

    stat_col <- if (prop_family) {
      ".proportion"
    } else if (stat_family) {
      stringi::stri_c(ignore_null = TRUE, ".", data_label)
    } else {
      stringi::stri_c(ignore_null = TRUE, ".", data_label)
    }

    fmt <-
      stringi::stri_c(
        ignore_null = TRUE,
        "%.",
        if (data_label == "count") 0 else digits,
        "f",
        if (data_label == "percentage") "%%"
      )

    ## Could replace fmt <- with a switch of scales::label_percent and scales::label_number

    out <- data
    out$.data_label <-
      dplyr::if_else(
        out[[stat_col]] >= hide_label_if_prop_below,
        out[[stat_col]],
        rep(NA, times = nrow(out))
      )
    if (percent_siblings) {
      out$.data_label <- out$.data_label * 100
    }
    out$.data_label <- sprintf(fmt = fmt, out$.data_label)
    out$.data_label <- stringi::stri_replace(
      out$.data_label,
      regex = " *NA%*$",
      replacement = ""
    )
    out$.data_label <- stringi::stri_replace(
      out$.data_label,
      regex = "\\.",
      replacement = decimal_symbol
    )

    out
  }


add_collapsed_categories <-
  function(
    data_summary,
    sort_by = NULL,
    data_label = ".proportion",
    categories_treated_as_na = c(),
    call = rlang::caller_env()
  ) {
    check_sort_by(x = data_summary$.category, sort_by = sort_by, call = call)

    lvls <- levels(data_summary$.category)
    lvls <- lvls[!lvls %in% categories_treated_as_na]
    if (
      length(sort_by) == 1 &&
        any(.saros.env$summary_data_sort1 == sort_by)
    ) {
      sort_by <- subset_vector(vec = lvls, set = sort_by)
    }

    non_grouping_vars <-
      c(
        ".category",
        ".count",
        ".count_se",
        ".proportion",
        ".proportion_se",
        ".mean",
        ".mean_se", # ".mean_base",
        ".data_label",
        ".sum_value"
      )

    data_summary |>
      dplyr::mutate(
        .comb_categories = as.character(.data$.category) %in% .env$sort_by
      ) |>
      dplyr::mutate(
        .sum_value = sum(
          .data[[if (data_label == "count") ".count" else ".proportion"]],
          na.rm = TRUE
        ), # THis should be an argument: counts or proportions.
        .sum_value = dplyr::if_else(
          condition = .data$.comb_categories, # NB! survey weights
          true = .data$.sum_value,
          false = NA_real_
        ),
        .by = !tidyselect::any_of(non_grouping_vars)
      )
  }

replace_with_empty_string <- function(str) {
  if (is.null(str) || is.na(str)) "" else str
}


add_n_to_label <- function(
  data_summary,
  add_n_to_dep_label = FALSE,
  add_n_to_dep_label_prefix = " (N = ",
  add_n_to_dep_label_suffix = ")",
  add_n_to_indep_label = FALSE,
  add_n_to_indep_label_prefix = " (N = ",
  add_n_to_indep_label_suffix = ")",
  add_n_to_label = FALSE,
  add_n_to_label_prefix = " (N = ",
  add_n_to_label_suffix = ")"
) {
  if (
    isFALSE(add_n_to_dep_label) &&
      isFALSE(add_n_to_indep_label) &&
      isFALSE(add_n_to_label)
  ) {
    return(data_summary)
  }

  add_n_to_dep_label_prefix <- replace_with_empty_string(
    add_n_to_dep_label_prefix
  )
  add_n_to_dep_label_suffix <- replace_with_empty_string(
    add_n_to_dep_label_suffix
  )
  add_n_to_indep_label_prefix <- replace_with_empty_string(
    add_n_to_indep_label_prefix
  )
  add_n_to_indep_label_suffix <- replace_with_empty_string(
    add_n_to_indep_label_suffix
  )
  add_n_to_label_prefix <- replace_with_empty_string(add_n_to_label_prefix)
  add_n_to_label_suffix <- replace_with_empty_string(add_n_to_label_suffix)

  if (isTRUE(add_n_to_dep_label)) {
    add_to_var <- ".variable_label"
    count_var <- ".count_per_dep"
    levels(data_summary[[add_to_var]]) <-
      paste0(
        levels(data_summary[[add_to_var]]),
        add_n_to_dep_label_prefix,
        unique(data_summary[
          order(as.integer(data_summary[[add_to_var]])),
          count_var,
          drop = TRUE
        ]),
        add_n_to_dep_label_suffix
      )
  }
  if (isTRUE(add_n_to_indep_label)) {
    add_to_var <- names(data_summary)[
      !names(data_summary) %in% .saros.env$summary_data_sort2
    ]
    count_var <- ".count_per_indep_group"
    if (length(add_to_var)) {
      data_summary <-
        dplyr::arrange(
          data_summary,
          as.integer(.data[[add_to_var]]),
          .data[[".variable_label"]]
        )
      data_summary[[add_to_var]] <-
        factor(
          paste0(
            as.character(data_summary[[add_to_var]]),
            add_n_to_indep_label_prefix,
            data_summary[[count_var]],
            add_n_to_indep_label_suffix
          ),
          levels = unique(paste0(
            as.character(data_summary[[add_to_var]]),
            add_n_to_indep_label_prefix,
            data_summary[[count_var]],
            add_n_to_indep_label_suffix
          )),
          exclude = character(),
          ordered = is.ordered(data_summary[[add_to_var]])
        )
    }
  }

  data_summary
}


add_n_to_category <- function(
  data_summary,
  add_n_to_category = FALSE,
  add_n_to_category_prefix = " (N = ",
  add_n_to_category_infix = ",",
  add_n_to_category_suffix = ")"
) {
  if (isFALSE(add_n_to_category)) {
    return(data_summary)
  }

  add_n_to_category_prefix <- replace_with_empty_string(
    add_n_to_category_prefix
  )
  add_n_to_category_infix <- replace_with_empty_string(add_n_to_category_infix)
  add_n_to_category_suffix <- replace_with_empty_string(
    add_n_to_category_suffix
  )

  out <-
    data_summary |>
    dplyr::mutate(
      .category_n_rng = paste0(
        unique(range(.data[[".count"]], na.rm = TRUE)),
        collapse = add_n_to_category_infix
      ),
      .by = tidyselect::all_of(".category")
    ) |>
    tidyr::unite(
      col = ".category_new",
      tidyselect::all_of(c(".category", ".category_n_rng")),
      sep = add_n_to_category_prefix,
      remove = FALSE,
      na.rm = TRUE
    ) |>
    dplyr::mutate(
      .category_new = paste0(.data[[".category_new"]], add_n_to_category_suffix)
    )
  new_labels <- unique(out$.category_new)
  names(new_labels) <- levels(out$.category)
  levels(out$.category) <- new_labels
  out$.category_new <- NULL # Remove the temporary column
  out$.category_n_rng <- NULL # Remove the temporary column
  out
}


flip_exception_categories <- function(
  data_summary,
  sort_by = NULL,
  categories_treated_as_na = c(),
  call = rlang::caller_env()
) {
  if (
    is.null(sort_by) ||
      length(categories_treated_as_na) == 0 ||
      !all(sort_by %in% .saros.env$summary_data_sort1) ||
      !any(unique(data_summary$.category) %in% categories_treated_as_na)
  ) {
    return(data_summary)
  }
  releveling_helper_function <- function(lvls) {
    lvls[lvls %in% categories_treated_as_na]
  }
  position <-
    if (sort_by %in% c(".top", ".upper", ".mid_upper")) {
      0
    } else {
      max(as.integer(factor(data_summary$.category)))
    }

  data_summary$.category <- forcats::fct_relevel(
    data_summary$.category,
    releveling_helper_function,
    after = position
  )
  data_summary
}


#' Shift labels_always_at
#'
#' @param data Dataset
#' @param labels_always_at Labels to move to bottom or top
#' @param after Position to move labels to (0 = top, Inf = bottom)
#' @return Dataset with data$.variable_label adjusted
#'
#' @keywords internal
shift_labels_always_at <- function(
  data,
  labels_always_at = NULL,
  after = Inf
) {
  labels_always_at <- labels_always_at[
    labels_always_at %in% levels(data$.variable_label)
  ]
  if (length(labels_always_at) > 0) {
    data$.variable_label <- forcats::fct_relevel(
      data$.variable_label,
      labels_always_at,
      after = after
    )
  }
  data
}

#' Apply legacy sorting adjustments for special cases
#'
#' @param data Dataset
#' @param indep_names Independent variable names
#' @param translations Translation strings
#' @return Dataset with legacy adjustments applied
#'
#' @keywords internal
apply_legacy_sorting_adjustments <- function(
  data,
  indep_names = character(0),
  translations = list()
) {
  # Handle independent variable adjustments
  if (length(indep_names) == 0) {
    return(data)
  }
  if (!all(indep_names %in% names(data))) {
    cli::cli_abort(
      "{.arg indep} contains variables not present in {.arg data}: {setdiff(indep_names, names(data))}."
    )
  }

  for (indep_name in indep_names) {
    # Handle crowd_others translation
    if (
      is.factor(data[[indep_name]]) &&
        !is.null(translations$crowd_others) &&
        any(levels(data[[indep_name]]) %in% translations$crowd_others)
    ) {
      current_levels <- levels(data[[indep_name]])
      data[[indep_name]] <- forcats::fct_relevel(
        data[[indep_name]],
        translations$crowd_others,
        after = length(current_levels)
      )
    }
  }

  data
}

#' Summarize a survey dataset for use in tables and graphs
#'
#' @inheritParams makeme
#' @param call *Internal call*
#'
#'   `obj:<call>` // *Default:* `rlang::caller_env()` (`optional`)
#'
#'   Both the absolute and relative folderpaths are required, as strings.
#' @importFrom rlang !!!
#' @keywords internal
#' @return Dataset with the columns: `.variable_name`, `.variable_label`, `.category`,
#'   `.count`, `.count_se`, `.count_per_dep`, `.count_per_indep_group`, `.proportion`, `.proportion_se`,
#'   `.mean`, `.mean_se`, `.median`, indep-variable(s), `.data_label`, `.comb_categories`, `.sum_value`,
#'   `.variable_label_prefix`
#'
summarize_cat_cat_data <-
  function(
    data,
    dep = colnames(data),
    indep = NULL,
    ...,
    showNA = c("ifany", "always", "never"),
    totals = FALSE,
    sort_by = ".upper",
    sort_dep_by = NULL,
    sort_indep_by = ".factor_order",
    data_label = c(
      "percentage_bare",
      "percentage",
      "proportion",
      "count",
      "mean",
      "median"
    ),
    digits = 0,
    add_n_to_dep_label = FALSE,
    add_n_to_indep_label = FALSE,
    add_n_to_label = FALSE,
    add_n_to_category = FALSE,
    hide_label_if_prop_below = .01,
    data_label_decimal_symbol = ".",
    categories_treated_as_na = NULL,
    label_separator = NULL,
    descend = FALSE,
    descend_indep = FALSE,
    labels_always_at_bottom = NULL,
    labels_always_at_top = NULL,
    translations = list(),
    call = rlang::caller_env()
  ) {
    showNA <- rlang::arg_match(showNA)
    data_label <- rlang::arg_match(data_label)

    # Handle backward compatibility with sort_by parameter
    # If new parameters are not specified but sort_by is, use sort_by for both
    if (
      is.null(sort_dep_by) &&
        is.null(sort_indep_by) &&
        !identical(sort_by, ".upper")
    ) {
      sort_dep_by <- sort_by
      sort_indep_by <- sort_by
    } else {
      # Use new parameters or defaults
      if (is.null(sort_dep_by)) {
        sort_dep_by <- sort_by
      }
      # Accept NULL and treat as .factor_order for indep
      if (is.null(sort_indep_by)) sort_indep_by <- ".factor_order"
    }

    if (
      !(inherits(data, what = "data.frame") || !inherits(data, what = "survey"))
    ) {
      cli::cli_abort(
        "{.arg data} should be a data.frame/tibble or survey object, not {.obj_type_friendly {data}}."
      )
    }

    if (any(dep %in% indep)) {
      return()
    }

    # Check if all dependent variables are ordered factors
    # This determines whether sort_by and descend should be ignored
    # dep_variable_order <- NULL # Store original order for ordered factors
    dep_vars_ordered <- all(vapply(
      dep,
      function(var_name) {
        if (var_name %in% colnames(data)) {
          is.factor(data[[var_name]]) && is.ordered(data[[var_name]])
        } else {
          FALSE
        }
      },
      logical(1)
    ))

    cross_table_output <-
      crosstable(
        data,
        dep = dep,
        indep = indep,
        showNA = showNA,
        totals = totals,
        translations = translations
      )

    check_sort_by(x = cross_table_output$.category, sort_by = sort_dep_by)

    fct_unions <- levels(cross_table_output[[".category"]])

    cross_table_output |>
      mutate_data_label(
        data_label = data_label,
        digits = digits,
        hide_label_if_prop_below = hide_label_if_prop_below,
        decimal_symbol = data_label_decimal_symbol
      ) |>
      category_var_as_fct(fct_unions = fct_unions) |>
      add_collapsed_categories(
        sort_by = if (dep_vars_ordered) NULL else sort_dep_by,
        categories_treated_as_na = categories_treated_as_na,
        data_label = data_label
      ) |>
      dplyr::mutate(
        .variable_label_prefix = get_main_question(
          .data$.variable_label,
          label_separator = label_separator,
          warn_multiple = FALSE
        ),
        .variable_label = keep_subitem(
          fct = .data$.variable_label,
          label_separator = label_separator
        )
      ) |>
      add_n_to_label(
        add_n_to_dep_label = add_n_to_dep_label,
        add_n_to_dep_label_prefix = translations$add_n_to_dep_label_prefix,
        add_n_to_dep_label_suffix = translations$add_n_to_dep_label_suffix,
        add_n_to_indep_label = add_n_to_indep_label,
        add_n_to_indep_label_prefix = translations$add_n_to_indep_label_prefix,
        add_n_to_indep_label_suffix = translations$add_n_to_indep_label_suffix,
        add_n_to_label = add_n_to_label,
        add_n_to_label_prefix = translations$add_n_to_label_prefix,
        add_n_to_label_suffix = translations$add_n_to_label_suffix
      ) |>
      add_n_to_category(
        add_n_to_category = add_n_to_category,
        add_n_to_category_prefix = translations$add_n_to_category_prefix,
        add_n_to_category_infix = translations$add_n_to_category_infix,
        add_n_to_category_suffix = translations$add_n_to_category_suffix
      ) |>
      flip_exception_categories(
        categories_treated_as_na = categories_treated_as_na,
        sort_by = if (dep_vars_ordered) NULL else sort_dep_by
      ) |>
      # NEW: Apply centralized sorting system
      add_sorting_order_vars(
        sort_dep_by = if (dep_vars_ordered) NULL else sort_dep_by, # Ignore sort_dep_by for ordered factors
        sort_indep_by = sort_indep_by,
        sort_category_by = if (dep_vars_ordered) NULL else sort_dep_by, # Ignore category sorting for ordered factors
        descend = descend,
        descend_indep = descend_indep
      ) |>
      # Set factor levels for backward compatibility
      set_factor_levels_from_order() |>
      # Respect labels_always_at_bottom and labels_always_at_top
      shift_labels_always_at(
        labels_always_at = labels_always_at_bottom,
        after = Inf
      ) |>
      shift_labels_always_at(
        labels_always_at = labels_always_at_top,
        after = 0
      ) |>
      apply_legacy_sorting_adjustments(
        indep_names = indep,
        translations = translations
      )
  }
