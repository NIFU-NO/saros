

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

    percent_siblings <- any(c("percentage", "percentage_bare") == data_label)
    prop_family <- any(c("percentage", "percentage_bare", "proportion") == data_label)
    stat_col <- if(prop_family) ".proportion" else stringi::stri_c(ignore_null=TRUE, ".", data_label)

    fmt <-
      stringi::stri_c(ignore_null=TRUE, "%.", if(data_label == "count") 0 else digits, "f",
                     if(data_label == "percentage") "%%")


    ## Could replace fmt <- with a switch of scales::label_percent and scales::label_number

    out <- data
    out$.data_label <-
          dplyr::if_else(out[[stat_col]] >= hide_label_if_prop_below,
                         out[[stat_col]],
                         rep(NA, times=nrow(out)))
    if(percent_siblings) out$.data_label <- out$.data_label*100
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
    if(length(sort_by)==1 &&
       any(.saros.env$summary_data_sort1 == sort_by)) {
      sort_by <- subset_vector(vec = lvls, set = sort_by)
    }

    non_grouping_vars <-
      c(".category",
        ".count", ".count_se",
        ".proportion", ".proportion_se",
        ".mean", ".mean_se", #".mean_base",
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
                            regex = stringi::stri_c(ignore_null=TRUE, "^(.*)", label_separator, "(.*)$"), # Assumes that the main question always comes first, and subitem always last
                            replacement = "$2")
    } else lvls

  factor(x = fct,
         levels = lvls,
         labels = lbls,
         ordered = TRUE)
}

add_n_to_bygroups <- function(data_summary,
                              add_n_to_bygroup = FALSE,
                              by_names = NULL) {

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
  #                                  labels = stringi::stri_c(ignore_null=TRUE, levels(data_summary$.category),
  #                                                          " (N=", n_per_category, ")"))
  # }
}


flip_exception_categories <- function(data_summary,
                                      sort_by = NULL,
                                      categories_treated_as_na = c(),
                                      call = rlang::caller_env()) {
  if(rlang::is_null(sort_by) ||
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
    if(sort_by %in% c(".top", ".upper", ".mid_upper")) 0 else max(as.integer(factor(data_summary$.category)))

  data_summary$.category <- forcats::fct_relevel(data_summary$.category,
                                                 releveling_helper_function,
                                                 after = position)
  data_summary
}


# Helper function that sorts output (if !is.null(sort_by)), optionally in descending order
sort_data <- function(data_summary,
                      ...,
                      indep_names = character(0),
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
    sort_col <- c(".comb_categories", ".sum_value", indep_names, ".category")
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
  data_summary$.variable_label <- forcats::fct_relevel(data_summary$.variable_label,
                                                       variables_always_at_top, after = 0)

  if(length(indep_names) > 0) {

    for(indep_name in indep_names) {
      if(is.factor(data_summary[[indep_name]])) {
        uniques <- rev(levels(data_summary[[indep_name]]))
      } else {
        uniques <- rev(as.character(unique(data_summary[[indep_name]])))
      }
      data_summary[[indep_name]] <- forcats::fct_relevel(data_summary[[indep_name]], uniques)
      if(any(levels(data_summary[[indep_name]]) == dots$translations$mesos_label_all_others)) {
        data_summary[[indep_name]] <- forcats::fct_relevel(data_summary[[indep_name]],
                                                        dots$translations$mesos_label_all_others, after = length(uniques))
      }
    }
  }
  data_summary

}

#' Summarize a survey dataset for use in tables and graphs
#'
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#' @param dep,indep *Variable selections*
#'
#'  <`tidyselect`> // *Default:* `NULL`, meaning everything for dep, nothing for indep.
#'
#'  Columns in `data`. Currently allows tidyselect-syntax, which will be removed.
#'
#' @param call *Internal call*
#'
#'   `obj:<call>` // *Default:* `rlang::caller_env()` (`optional`)
#'
#'   Both the absolute and relative folderpaths are required, as strings.

#' @importFrom rlang !!!
#' @export
#' @return Dataset
#'
summarize_data <-
  function(data,
           ...,
           dep = colnames(data),
           indep = NULL,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)
    # dots_nms <- names(dots)
    # dots <- lapply(seq_along(dots), function(i) {
    #   if(!any(c("data", "call", "...", "chapter_overview") == names(dots)[i])) {
    #     eval(dots[[i]])
    #   } else {
    #     dots[[i]]
    #     }
    # })
    # names(dots) <- dots_nms
    if(!(inherits(data, what = "data.frame") || !inherits(data, what = "survey"))) {
      cli::cli_abort("{.arg data} should be a data.frame/tibble or survey object, not {.obj_type_friendly {data}}.")
    }

    if(any(dep %in% indep)) return()


    fct_unions <- if(!inherits(data, "survey.design")) data[, dep] else data$variables[, dep]
    fct_unions <- forcats::fct_unify(fs = fct_unions)[[1]]
    fct_unions <- levels(fct_unions)

    cross_table_output <-
      crosstable3(data,
                  dep = dep,
                  indep = indep,
                  showNA = dots$showNA,
                  totals = dots$totals,
                  translations = dots$translations)

    valid_values <- c(.saros.env$summary_data_sort1,
                      .saros.env$summary_data_sort2,
                      unique(as.character(cross_table_output$.category)))
    if(!all(dots$sort_by %in% valid_values)) {
      cli::cli_abort(c(x="Not all {.arg sort_by} are valid: {dots$sort_by}.",
                       i="Valid values are {valid_values}"))
    }

    cross_table_output %>%
      mutate_data_label(data_label = dots$data_label,
                        digits = dots$digits,
                        hide_label_if_prop_below = dots$hide_label_if_prop_below,
                        decimal_symbol = dots$data_label_decimal_symbol) %>%
      category_var_as_fct(fct_unions = fct_unions) %>%
      add_collapsed_categories(sort_by = dots$sort_by,
                               categories_treated_as_na = dots$categories_treated_as_na,
                               data_label = dots$data_label) %>%
      dplyr::mutate(.variable_label = keep_subitem(fct = .data$.variable_label,
                                                   label_separator = dots$label_separator)) %>%
      # add_n_to_bygroups(add_n_to_bygroup = add_n_to_bygroup, indep_names = indep) %>%
      flip_exception_categories(categories_treated_as_na = dots$categories_treated_as_na,
                                sort_by = dots$sort_by) %>%
      sort_data(indep_names = indep, !!!dots)
  }

