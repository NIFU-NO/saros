

# Helper function to create overarching factor variable for the response categories column
category_var_as_fct <-
  function(data_summary, fct_unions) {
    data_summary$.category <- factor(x = data_summary$.category,
                                       levels = fct_unions,
                                       labels = fct_unions)
    data_summary
  }


mutate_data_label <-
  function(
    data,
    data_label="count",
    digits = 1,
    hide_label_if_prop_below=0,
    decimal_symbol=".") {

    percent_siblings <- data_label %in% c("percentage", "percentage_bare")
    prop_family <- data_label %in% c("percentage", "percentage_bare", "proportion")
    stat_col <- if(prop_family) ".proportion" else stringr::str_c(".", data_label)

    fmt <-
      stringr::str_c("%.", if(data_label == "count") 0 else digits, "f",
                     if(data_label == "percentage") "%%")


    ## Could replace fmt <- with a switch of scales::label_percent and scales::label_number

    out <-
      data %>%
      dplyr::mutate(
        .data_label =
          dplyr::if_else(.data[[stat_col]] >= .env$hide_label_if_prop_below,
                         .data[[stat_col]],
                         rep(NA, times=nrow(.))))
    if(percent_siblings) out$.data_label <- out$.data_label*100
    out <-
      out %>%
      dplyr::mutate(
        .data_label = sprintf(fmt = .env$fmt, .data$.data_label),
        .data_label = stringi::stri_replace(.data$.data_label, regex = " *NA%*$", replacement = ""),
        .data_label = stringi::stri_replace(.data$.data_label, regex = "\\.", replacement = decimal_symbol)
      )

    out
  }


add_collapsed_categories <-
  function(data_summary,
           sort_by = NULL,
           data_label = ".proportion",
           categories_treated_as_na = c(),
           call = rlang::caller_env()) {

    # print(data_summary$.category)
    # print(sort_by)
    check_sort_by(x = data_summary$.category, sort_by = sort_by, call = call)


    lvls <- levels(data_summary$.category)
    lvls <- lvls[!lvls %in% categories_treated_as_na]
    if(length(sort_by)==1 &&
       sort_by %in% .saros.env$summary_data_sort1) {
      sort_by <- subset_vector(vec = lvls, set = sort_by)
    }

    non_grouping_vars <-
      c(".category",
        ".count", ".count_se",
        ".proportion", ".proportion_se",
        ".mean", ".mean_se", ".mean_base",
        ".data_label", ".sum_value")

    data_summary %>%
      dplyr::mutate(.comb_categories = as.character(.data$.category) %in% .env$sort_by) %>%
      dplyr::mutate(.sum_value = sum(.data[[if(data_label == "count") ".count" else ".proportion"]], na.rm = TRUE), # THis should be an argument: counts or proportions.
                    .sum_value = dplyr::if_else(condition = .data$.comb_categories, # NB! survey weights
                                                true = .data$.sum_value,
                                                false = NA_real_),
                    .by = !tidyselect::any_of(non_grouping_vars))

  }



keep_subitem <- function(fct, label_separator = NULL,
                         call = rlang::caller_env()) {
  lvls <- unique(as.character(fct)) # The items (including main question)
  lbls <-
    if(!is.null(label_separator)) {
      stringi::stri_replace(str = lvls,
                            regex = stringr::str_c("^(.*)", label_separator, "(.*)$"), # Assumes that the main question always comes first, and subitem always last
                            replacement = "$2")
    } else lvls

  factor(x = fct,
         levels = lvls,
         labels = lbls,
         ordered = TRUE)
}

add_n_to_bygroups <- function(data_summary, add_n_to_bygroup = FALSE, by_names = by_names) {

  # if(!(add_n_to_bygroup && length(by_names) > 0)) {
  return(data_summary)
  # } else {
  # n_per_group <-
  #   data_summary %>%
  #   dplyr::arrange(as.integer(.data$.category)) %>%
  #   dplyr::group_by(dplyr::pick(tidyselect::all_of(c(".variable_label", by_names)))) %>%
  #   dplyr::summarize(n_per_category = sum(.data$.count, na.rm = TRUE)) %>%
  #   dplyr::pull(.data$n_per_category)
  # data_summary$.category <- factor(data_summary$.category,
  #                                  labels = stringr::str_c(levels(data_summary$.category),
  #                                                          " (N=", n_per_category, ")"))
  # }
}


flip_exception_categories <- function(data_summary,
                                      sort_by = NULL,
                                      categories_treated_as_na = c(),
                                      call = rlang::caller_env()) {
  if(rlang::is_null(sort_by) ||
     length(categories_treated_as_na) == 0 ||
     !sort_by %in% .saros.env$summary_data_sort1 ||
     !any(unique(data_summary$.category) %in% categories_treated_as_na)) {

    return(data_summary)
  }
  releveling_helper_function <- function(lvls) {
    # c(lvls[lvls %in% categories_treated_as_na], lvls[!lvls %in% categories_treated_as_na])

    lvls[lvls %in% categories_treated_as_na]
  }
  position <-
    if(sort_by %in% c(".top", ".upper", ".mid_upper")) 0 else max(as.integer(factor(data_summary$.category)))

  data_summary$.category <- forcats::fct_relevel(data_summary$.category,
                                                 releveling_helper_function,
                                                 after = position)
  data_summary
}


# Helper function that sorts output (if !is.null(sort_by)), optionally in descending order
sort_data <- function(data_summary,
                      ...,
                      by_names = character(0),
                      call = rlang::caller_env()) {


  dots <- rlang::list2(...)
  if(is.null(dots$sort_by)) {
    return(data_summary %>%
             dplyr::arrange(as.integer(.data$.variable_label), as.integer(.data$.category)))

    #### descend IS CURRENTLY GLOBAL ACROSS ALL VARIABLES:
  } else if(all(dots$sort_by %in% names(data_summary))) {
    sort_col <- dots$sort_by
  } else if((length(dots$sort_by) == 1 &&
             dots$sort_by %in% .saros.env$summary_data_sort1) ||
            all(dots$sort_by %in% unique(data_summary$.category))) {
    sort_col <- c(".comb_categories", ".sum_value", ".category")
  }

  if(dots$descend) {
    data_summary <-
      data_summary %>%
      dplyr::arrange(dplyr::across(tidyselect::all_of(sort_col), dplyr::desc))
  } else {
    data_summary <-
      data_summary %>%
      dplyr::arrange(dplyr::pick(tidyselect::all_of(sort_col)))
  }
  uniques <- as.character(unique(data_summary$.variable_label))
  data_summary$.variable_label <- forcats::fct_relevel(data_summary$.variable_label, uniques)

  variables_always_at_bottom <- dots$variables_always_at_bottom[dots$variables_always_at_bottom %in% uniques]
  data_summary$.variable_label <- forcats::fct_relevel(data_summary$.variable_label,
                                                       variables_always_at_bottom, after = length(uniques))

  variables_always_at_top <- dots$variables_always_at_top[dots$variables_always_at_top %in% uniques]
  data_summary$.variable_label <- forcats::fct_relevel(data_summary$.variable_label, variables_always_at_top, after = 0)

  if(length(by_names) > 0) {
    for(by_name in by_names) {
      uniques <- rev(as.character(unique(data_summary[[by_name]])))
      data_summary[[by_name]] <- forcats::fct_relevel(data_summary[[by_name]], uniques)
    }
  }
  data_summary

}

#' Summarize a survey dataset for use in tables and graphs
#'
#' @param data Data frame.
#' @param cols Columns to select for reporting. Supports \code{\link[dplyr:dplyr_tidy_select]{tidy-select}}.
#' @param by \code{\link[dplyr:dplyr_data_masking]{data-masking}}\cr. Optional column used to break output by.
#' @param data_label [\code{character(1)}] What to display, one of "proportion", "percentage", "percentage_bare" (which is without the percentage symbol), "count", "mean", or "median". Defaults to "proportion".
#' @param showNA [\code{logical(1)}]\cr Whether to show NA in categorical variables (one of c("ifany", "always", "never"), like in table()).
#' @param digits [\code{integer(1)}]\cr Number of decimal places as integer.
#' @param data_label_decimal_symbol [\code{character(1)}] Although the English speaking world uses the dot '.' as decimal marker, some might prefer a comma ',' or something else entirely.
#' @param sort_by [\code{character(1)}] Sort output (and collapse if requested). Defaults to none (NULL).
#' \itemize{
#' \item{".top"}{The proportion for the highest category available in the variable.}
#' \item{".upper"}{The sum of the proportions for the categories above the middle category.}
#' \item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
#' \item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
#' \item{".lower"}{The sum of the proportions for the categories below the middle category.}
#' \item{".bottom"}{The proportions for the lowest category available in the variable.}
#' \item{".variable_label"}{Sort by the variable labels.}
#' \item{".id"}{Sort by the variable names.}
#' \item{".by_group"}{The groups of the by argument.}
#' \item{[\code{character(1)}]}{Character vector of category labels to sum together.}
#' }
#' @param descend [\code{logical(1)}]\cr Reverse sorting of sort_by. Defaults to ascending order (FALSE).
#' @param hide_label_if_prop_below [\code{numeric(1)}] Whether to hide label if below this value.
#' @param label_separator [\code{character(1)}]\cr Split pattern.
#' @param ... Optional parameters forwarded from above.
#' @param call Error call function, usually not needed.
#'
#' @importFrom rlang !!!
#'
#' @return Dataset
#'
summarize_data <-
  function(data,
           ...,
           cols = tidyselect::everything(),
           by = NULL,
           label_separator = NULL,
           add_n_to_bygroup = FALSE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)

    fct_unions <- levels(forcats::fct_unify(fs = dplyr::select(data, {{cols}}))[[1]])

    by_names <- colnames(dplyr::select(data, {{by}}))


    data %>%
      crosstable2(cols = {{cols}}, by = {{by}}, showNA = dots$showNA) %>%
      mutate_data_label(data_label = dots$data_label,
                        digits = dots$digits,
                        hide_label_if_prop_below = dots$hide_label_if_prop_below,
                        decimal_symbol = dots$data_label_decimal_symbol) %>%
      category_var_as_fct(fct_unions = fct_unions) %>%
      add_collapsed_categories(sort_by = dots$sort_by, categories_treated_as_na = dots$categories_treated_as_na,
                               data_label = dots$data_label) %>%
      dplyr::mutate(.variable_label = keep_subitem(fct = .data$.variable_label,
                                                   label_separator = label_separator)) %>%
      add_n_to_bygroups(add_n_to_bygroup = add_n_to_by_group, by_names = by_names) %>%
      flip_exception_categories(categories_treated_as_na = dots$categories_treated_as_na, sort_by = dots$sort_by) %>%
      sort_data(by_names = by_names, !!!dots)
  }

