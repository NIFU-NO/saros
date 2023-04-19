

# Helper function to create overarching factor variable for the response categories column
category_var_as_fct <-
  function(data_summary) {
    # Find the highest number of response categories in battery
    fct_uniques <-
      data_summary %>%
      dplyr::group_by(.data$.variable_label) %>%
      dplyr::group_map(.f = ~{
        x <- levels(.x$.category)
        matrix(ncol = length(x)) %>%
          as.data.frame() %>%
          rlang::set_names(nm=x)
      }) %>%
      purrr::reduce(.f = dplyr::full_join) %>%
      colnames() %>%
      suppressMessages()

    data_summary %>%
      dplyr::mutate(.category = factor(x = .data$.category,
                                       levels = .env$fct_uniques,
                                       labels = .env$fct_uniques))
  }


add_collapsed_categories <-
  function(data_summary,
           sort_by = NULL,
           call = rlang::caller_env()) {

  check_sort_by(x = data_summary$.category, sort_by = sort_by, call = call)

  if(length(sort_by)==1 &&
     sort_by %in% .saros.env$summary_data_sort1) {
    sort_by <- subset_vector(vec = levels(data_summary$.category), set = sort_by)
  }

  non_grouping_vars <-
    c(".category",
      ".count", ".count_se",
      ".proportion", ".proportion_se",
      ".mean", ".mean_se", ".mean_base",
      ".data_label", ".sum_value")

  data_summary %>%
    dplyr::mutate(.comb_categories = as.character(.data$.category) %in% .env$sort_by) %>%
    dplyr::mutate(.sum_value = sum(.data$.count, na.rm = TRUE),
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
      stringr::str_replace(string = lvls,
                           pattern = paste0("^(.*)", label_separator, "(.*)$"), # Assumes that the main question always comes first, and subitem always last
                           replacement = "\\2")
    } else lvls

  factor(x = fct,
         levels = lvls,
         labels = lbls,
         ordered = TRUE)
}



# Helper function that sorts output (if !is.null(sort_by)), optionally in descending order
sort_data <- function(data_summary, sort_by = NULL, descend = FALSE,
                      call = rlang::caller_env()) {


  if(is.null(sort_by)) {
    return(data_summary %>%
             dplyr::arrange(as.integer(.data$.variable_label), .data$.category))

    #### descend IS CURRENTLY GLOBAL ACROSS ALL VARIABLES:
  } else if(all(sort_by %in% names(data_summary))) {
    sort_col <- sort_by
  } else if((length(sort_by) == 1 &&
             sort_by %in% .saros.env$summary_data_sort1) ||
            all(sort_by %in% unique(data_summary$.category))) {
    sort_col <- c(".comb_categories", ".sum_value", ".category")
  }

  if(descend) {
    data_summary %>%
      dplyr::arrange(dplyr::across(tidyselect::all_of(sort_col), dplyr::desc))
  } else {
    data_summary %>%
      dplyr::arrange(dplyr::pick(tidyselect::all_of(sort_col)))
  }

}


mutate_data_label <-
  function(
    data,
    data_label,
    digits,
    ignore_if_below,
    decimal_symbol
  ) {
    percent_siblings <- data_label %in% c("percentage", "percentage_bare")
    prop_family <- data_label %in% c("percentage", "percentage_bare", "proportion")
    stat_col <- if(prop_family) ".proportion" else stringr::str_c(".", data_label)

    fmt <-
      stringr::str_c("%.", digits, "f",
                     if(data_label == "percentage") "%%") %>%
      stringr::str_replace(pattern = "\\.", decimal_symbol)

    ## Could replace fmt <- with a switch of scales::label_percent and scales::label_number

    data %>%
    dplyr::mutate(
      .data_label =
        dplyr::if_else(.data[[stat_col]] >= .env$ignore_if_below,
                       .data[[stat_col]],
                       rep(NA, length=nrow(.))),
      .data_label = dplyr::if_else(rep(percent_siblings, nrow(.)), .data$.data_label*100, .data$.data_label),
      .data_label = sprintf(fmt = .env$fmt, .data$.data_label),
      .data_label = stringr::str_replace(.data$.data_label, " *NA%*$", "")
    )
  }

#' Summarize a survey dataset for use in tables and graphs
#'
#' @param data Data frame or tibble.
#' @param cols Columns to select for reporting. Supports \code{\link[dplyr:dplyr_tidy_select]{tidy-select}}.
#' @param by \code{\link[dplyr:dplyr_data_masking]{data-masking}}\cr. Optional column used to break output by.
#' @param data_label [\code{character(1)}] What to display, one of "proportion", "percentage", "percentage_bare" (which is without the percentage symbol), "count", "mean", or "median". Defaults to "proportion".
#' @param showNA [\code{logical(1)}]\cr Whether to show NA in categorical variables (one of c("ifany", "always", "no"), like in table()).
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
#' @param ignore_if_below [\code{numeric(1)}] Whether to hide label if below this value.
#' @param label_separator [\code{character(1)}]\cr Split pattern.
#' @param ... Optional parameters forwarded from above.
#' @param call Error call function, usually not needed.
#'
#' @importFrom rlang arg_match caller_env is_integerish .env !!!
#' @importFrom dplyr mutate group_by arrange ungroup if_else desc n_distinct %>%
#' @importFrom cli cli_abort
#' @importFrom stringr str_c
#' @importFrom stats ave
#'
#' @return Dataset
#'
#' @examples
#' library(dplyr)
#' x <-
#' ex_survey1 %>%
#' saros:::summarize_data(cols = matches("^b_"),
#'                  sort_by = c("A lot", "A bit"),
#'                  label_separator = " - ",
#'                  data_label = "percentage")
#' library(srvyr)
#' x <-
#' ex_survey1 %>%
#'   as_survey(strata = f_uni) %>%
#'   saros:::summarize_data(cols = matches("^b_"),
#'                                 sort_by = c("A lot", "A bit"),
#'                                 label_separator = " - ",
#'                                 data_label = "percentage")
summarize_data <-
  function(data,
           ...,
           cols = tidyselect::everything(),
           by = NULL,
           data_label = c("proportion", "percentage", "percentage_bare", "count", "mean", "median"),
           showNA = c("ifany", "always", "no"),
           digits = 1,
           sort_by = NULL,
           descend = FALSE,
           ignore_if_below = 0,
           label_separator = NULL,
           data_label_decimal_symbol = ".",
           call = rlang::caller_env()) {

    # Argument checks
    dots <- rlang::list2(...)

    showNA <- rlang::arg_match(showNA, multiple = FALSE, error_call = call)
    data_label <- rlang::arg_match(data_label, multiple = FALSE, error_call = call)
    # check_bool(percentage, call = call)
    check_bool(descend, call = call)
    # check_bool(percent_sign, call = call)
    check_string(label_separator, null.ok=TRUE, call = call)
    check_integerish(digits, min = 0, call = call)
    check_integerish(ignore_if_below, min = 0, call = call)


    data %>%
      crosstable2(cols = {{cols}}, by = {{by}}, showNA = showNA) %>%
      mutate_data_label(data_label = data_label,
                        digits = digits,
                        ignore_if_below = ignore_if_below,
                        decimal_symbol = data_label_decimal_symbol) %>%
      category_var_as_fct() %>%
      add_collapsed_categories(sort_by = sort_by) %>%
      dplyr::mutate(.variable_label = keep_subitem(fct = .data$.variable_label,
                                                   label_separator = label_separator)) %>%
      sort_data(sort_by = sort_by, descend = descend)
  }

