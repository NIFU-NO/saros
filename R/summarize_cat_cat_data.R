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
      decimal_symbol = ".") {
    percent_siblings <- any(c("percentage", "percentage_bare") == data_label)
    prop_family <- any(c("percentage", "percentage_bare", "proportion") == data_label)
    stat_col <- if (prop_family) ".proportion" else stringi::stri_c(ignore_null = TRUE, ".", data_label)

    fmt <-
      stringi::stri_c(
        ignore_null = TRUE, "%.", if (data_label == "count") 0 else digits, "f",
        if (data_label == "percentage") "%%"
      )


    ## Could replace fmt <- with a switch of scales::label_percent and scales::label_number

    out <- data
    out$.data_label <-
      dplyr::if_else(out[[stat_col]] >= hide_label_if_prop_below,
        out[[stat_col]],
        rep(NA, times = nrow(out))
      )
    if (percent_siblings) out$.data_label <- out$.data_label * 100
    out$.data_label <- sprintf(fmt = fmt, out$.data_label)
    out$.data_label <- stringi::stri_replace(out$.data_label, regex = " *NA%*$", replacement = "")
    out$.data_label <- stringi::stri_replace(out$.data_label, regex = "\\.", replacement = decimal_symbol)

    out
  }


add_collapsed_categories <-
  function(data_summary,
           sort_by = NULL,
           data_label = ".proportion",
           categories_treated_as_na = c(),
           call = rlang::caller_env()) {
    check_sort_by(x = data_summary$.category, sort_by = sort_by, call = call)


    lvls <- levels(data_summary$.category)
    lvls <- lvls[!lvls %in% categories_treated_as_na]
    if (length(sort_by) == 1 &&
      any(.saros.env$summary_data_sort1 == sort_by)) {
      sort_by <- subset_vector(vec = lvls, set = sort_by)
    }

    non_grouping_vars <-
      c(
        ".category",
        ".count", ".count_se",
        ".proportion", ".proportion_se",
        ".mean", ".mean_se", # ".mean_base",
        ".data_label", ".sum_value"
      )

    data_summary |>
      dplyr::mutate(.comb_categories = as.character(.data$.category) %in% .env$sort_by) |>
      dplyr::mutate(
        .sum_value = sum(.data[[if (data_label == "count") ".count" else ".proportion"]], na.rm = TRUE), # THis should be an argument: counts or proportions.
        .sum_value = dplyr::if_else(condition = .data$.comb_categories, # NB! survey weights
          true = .data$.sum_value,
          false = NA_real_
        ),
        .by = !tidyselect::any_of(non_grouping_vars)
      )
  }

replace_with_empty_string <- function(str) {
  if (is.null(str) || is.na(str)) "" else str
}


add_n_to_label <- function(data_summary,
                           add_n_to_dep_label = FALSE,
                           add_n_to_dep_label_prefix = " (N = ",
                           add_n_to_dep_label_suffix = ")",
                           add_n_to_indep_label = FALSE,
                           add_n_to_indep_label_prefix = " (N = ",
                           add_n_to_indep_label_suffix = ")",
                           add_n_to_label = FALSE,
                           add_n_to_label_prefix = " (N = ",
                           add_n_to_label_suffix = ")") {
  if (isFALSE(add_n_to_dep_label) && isFALSE(add_n_to_indep_label) && isFALSE(add_n_to_label)) {
    return(data_summary)
  }


  add_n_to_dep_label_prefix <- replace_with_empty_string(add_n_to_dep_label_prefix)
  add_n_to_dep_label_suffix <- replace_with_empty_string(add_n_to_dep_label_suffix)
  add_n_to_indep_label_prefix <- replace_with_empty_string(add_n_to_indep_label_prefix)
  add_n_to_indep_label_suffix <- replace_with_empty_string(add_n_to_indep_label_suffix)
  add_n_to_label_prefix <- replace_with_empty_string(add_n_to_label_prefix)
  add_n_to_label_suffix <- replace_with_empty_string(add_n_to_label_suffix)
  # browser()

  if (isTRUE(add_n_to_dep_label)) {
    add_to_var <- ".variable_label"
    count_var <- ".count_per_dep"
    levels(data_summary[[add_to_var]]) <-
      paste0(
        levels(data_summary[[add_to_var]]),
        add_n_to_dep_label_prefix,
        unique(data_summary[order(as.integer(data_summary[[add_to_var]])), count_var, drop = TRUE]),
        add_n_to_dep_label_suffix
      )
    # data_summary <-
    #   data_summary |>
    #   tidyr::unite(
    #     col = !!add_to_var,
    #     tidyselect::all_of(c(add_to_var, count_var)),
    #     sep = add_n_to_dep_label_prefix, remove = FALSE, na.rm = TRUE
    #   ) |>
    #   dplyr::mutate(.variable_label = paste0(.data[[add_to_var]], add_n_to_dep_label_suffix))
  }
  if (isTRUE(add_n_to_indep_label)) {
    add_to_var <- names(data_summary)[!names(data_summary) %in% .saros.env$summary_data_sort2]
    count_var <- ".count_per_indep_group"
    if (length(add_to_var)) {
      levels(data_summary[[add_to_var]]) <-
        paste0(
          levels(data_summary[[add_to_var]]),
          add_n_to_indep_label_prefix,
          unique(data_summary[order(as.integer(data_summary[[add_to_var]])), count_var, drop = TRUE]),
          add_n_to_indep_label_suffix
        )

      # data_summary <-
      #   data_summary |>
      #   tidyr::unite(
      #     col = !!add_to_var,
      #     tidyselect::all_of(c(add_to_var, count_var)),
      #     sep = add_n_to_indep_label_prefix, remove = FALSE, na.rm = TRUE
      #   )
      # data_summary[[add_to_var]] <- paste0(data_summary[[add_to_var]], add_n_to_indep_label_suffix)
      # if (var_ordered_status) data_summary[[add_to_var]] <- ordered(data_summary[[add_to_var]])
    }
  }


  data_summary
}



add_n_to_category <- function(data_summary,
                              add_n_to_category = FALSE,
                              add_n_to_category_prefix = " (N = ",
                              add_n_to_category_infix = ",",
                              add_n_to_category_suffix = ")") {
  if (isFALSE(add_n_to_category)) {
    return(data_summary)
  }

  add_n_to_category_prefix <- replace_with_empty_string(add_n_to_category_prefix)
  add_n_to_category_infix <- replace_with_empty_string(add_n_to_category_infix)
  add_n_to_category_suffix <- replace_with_empty_string(add_n_to_category_suffix)

  out <-
    data_summary |>
    dplyr::mutate(
      .category_n_rng = paste0(range(.data[[".count"]], na.rm = TRUE),
        collapse = add_n_to_category_infix
      ),
      .by = tidyselect::all_of(".category")
    ) |>
    tidyr::unite(
      col = ".category_new",
      tidyselect::all_of(c(".category", ".category_n_rng")),
      sep = add_n_to_category_prefix, remove = FALSE, na.rm = TRUE
    ) |>
    dplyr::mutate(.category_new = paste0(.data[[".category_new"]], add_n_to_category_suffix))
  new_labels <- unique(out$.category_new)
  names(new_labels) <- levels(out$.category)
  levels(out$.category) <- new_labels
  out
}



flip_exception_categories <- function(data_summary,
                                      sort_by = NULL,
                                      categories_treated_as_na = c(),
                                      call = rlang::caller_env()) {
  if (is.null(sort_by) ||
    length(categories_treated_as_na) == 0 ||
    !all(sort_by %in% .saros.env$summary_data_sort1) ||
    !any(unique(data_summary$.category) %in% categories_treated_as_na)) {
    return(data_summary)
  }
  releveling_helper_function <- function(lvls) {
    # c(lvls[lvls %in% categories_treated_as_na], lvls[!lvls %in% categories_treated_as_na])

    lvls[lvls %in% categories_treated_as_na]
  }
  position <-
    if (sort_by %in% c(".top", ".upper", ".mid_upper")) 0 else max(as.integer(factor(data_summary$.category)))

  data_summary$.category <- forcats::fct_relevel(data_summary$.category,
    releveling_helper_function,
    after = position
  )
  data_summary
}


# Helper function that sorts output (if !is.null(sort_by)), optionally in descending order
sort_data <- function(data_summary,
                      sort_by = NULL,
                      descend = FALSE,
                      variables_always_at_bottom = NULL,
                      variables_always_at_top = NULL,
                      translations = eval(formals(makeme)$translations),
                      indep_names = character(0),
                      call = rlang::caller_env()) {
  if (is.null(sort_by)) {
    return(dplyr::arrange(data_summary, as.integer(.data$.variable_label), as.integer(.data$.category)))
  }

  if (all(sort_by %in% names(data_summary))) { # E.g. .variabel_name, .variable_label, x1_sex

    sort_col <- sort_by
  } else if ((length(sort_by) == 1 &&
    sort_by %in% .saros.env$summary_data_sort1) || # E.g. .top, .upper, .mid_upper, c("A bit", "A lot")
    all(sort_by %in% unique(data_summary$.category))) {
    sort_col <- c(
      ".sum_value",
      indep_names[lapply(indep_names, function(indep_name) is.ordered(data_summary[[indep_name]])) |> unlist()],
      ".category"
    )
  }
  #### descend IS CURRENTLY GLOBAL ACROSS ALL VARIABLES:

  if (isTRUE(descend)) {
    data_summary <-
      dplyr::arrange(data_summary, dplyr::across(tidyselect::all_of(sort_col), dplyr::desc))
  } else {
    data_summary <-
      dplyr::arrange(data_summary, dplyr::pick(tidyselect::all_of(sort_col))) # Incorrect
  }

  uniques <- as.character(unique(data_summary$.variable_label))

  if (!all(is.na(data_summary$.variable_label))) {
    data_summary$.variable_label <- forcats::fct_relevel(data_summary$.variable_label, uniques)

    variables_always_at_bottom <- variables_always_at_bottom[variables_always_at_bottom %in% uniques]
    data_summary$.variable_label <- forcats::fct_relevel(data_summary$.variable_label,
      variables_always_at_bottom,
      after = length(uniques)
    )

    variables_always_at_top <- variables_always_at_top[variables_always_at_top %in% uniques]
    data_summary$.variable_label <- forcats::fct_relevel(data_summary$.variable_label,
      variables_always_at_top,
      after = 0
    )
  }

  if (length(indep_names) > 0) {
    for (indep_name in indep_names) {
      if (is.factor(data_summary[[indep_name]])) {
        uniques <- rev(levels(data_summary[[indep_name]]))
      } else {
        uniques <- rev(as.character(unique(data_summary[[indep_name]])))
      }
      data_summary[[indep_name]] <- forcats::fct_relevel(data_summary[[indep_name]], uniques)
      if (any(levels(data_summary[[indep_name]]) %in% translations$crowd_others)) {
        data_summary[[indep_name]] <- forcats::fct_relevel(data_summary[[indep_name]],
          translations$crowd_others,
          after = length(uniques)
        )
      }
    }
  }
  data_summary
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
#' @return Dataset
#'
summarize_cat_cat_data <-
  function(data,
           dep = colnames(data),
           indep = NULL,
           ...,
           showNA = c("ifany", "always", "never"),
           totals = FALSE,
           sort_by = ".upper",
           data_label = c("percentage_bare", "percentage", "proportion", "count"),
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
           variables_always_at_bottom = NULL,
           variables_always_at_top = NULL,
           translations = list(),
           call = rlang::caller_env()) {
    showNA <- rlang::arg_match(showNA)
    data_label <- rlang::arg_match(data_label)


    if (!(inherits(data, what = "data.frame") || !inherits(data, what = "survey"))) {
      cli::cli_abort("{.arg data} should be a data.frame/tibble or survey object, not {.obj_type_friendly {data}}.")
    }

    if (any(dep %in% indep)) {
      return()
    }


    cross_table_output <-
      crosstable(data,
        dep = dep,
        indep = indep,
        showNA = showNA,
        totals = totals,
        translations = translations
      )

    # fct_unions <- get_common_levels(data=data, col_pos=match(dep, colnames(data)))
    fct_unions <- levels(cross_table_output[[".category"]])

    valid_values <- c(
      .saros.env$summary_data_sort1,
      .saros.env$summary_data_sort2,
      unique(as.character(cross_table_output$.category))
    )
    if (!all(sort_by %in% valid_values)) {
      cli::cli_abort(c(
        x = "Not all {.arg sort_by} are valid: {sort_by}.",
        i = "Valid values are {valid_values}"
      ))
    }

    cross_table_output |>
      mutate_data_label(
        data_label = data_label,
        digits = digits,
        hide_label_if_prop_below = hide_label_if_prop_below,
        decimal_symbol = data_label_decimal_symbol
      ) |>
      category_var_as_fct(fct_unions = fct_unions) |>
      add_collapsed_categories(
        sort_by = sort_by,
        categories_treated_as_na = categories_treated_as_na,
        data_label = data_label
      ) |>
      dplyr::mutate(
        .variable_label_prefix = get_main_question(.data$.variable_label,
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
        sort_by = sort_by
      ) |>
      sort_data(
        indep_names = indep,
        sort_by = sort_by,
        descend = descend,
        variables_always_at_bottom = variables_always_at_bottom,
        variables_always_at_top = variables_always_at_top,
        translations = translations
      )
  }
