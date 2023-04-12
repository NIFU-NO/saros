
eval_cols <- function(x, data, call = rlang::caller_env()) {
  check_string(x = x, n = NULL, null.ok = FALSE, call = call)
  check_data_frame(data, call = call)
  x %>%
    # rlang::set_names() %>%
    purrr::map(.f = function(col_entry) {
      if(nchar(col_entry)>0) {
        col_entry <- paste0('tidyselect::eval_select(expr = rlang::expr(c(',
                            col_entry,
                            ')), data = data)')
        out <- eval(parse(text = col_entry))

      } else out <- NA_integer_
    })

}


look_for_extended <- function(data,
                              cols = tidyselect::everything(),
                              label_separator = NULL,
                              name_separator = NULL) {
  ### Assume that related columns always have identical label prefix AND overlapping response categories.
  ### Assume that variables with identical label prefix may not be related.
  ### Assume that related columns are always next to each other OR share same variable name prefix.

  data_part <- dplyr::select(data, {{cols}})
  if(ncol(data_part) == 0 || nrow(data_part) == 0) cli::cli_abort("data.frame is of 0 length.")

  x <- data.frame(
    col_pos = match(colnames(data_part), colnames(data)),
    col_name = colnames(data_part),
    col_label = get_raw_labels(data = data_part),
    col_type = purrr::map_chr(names(data_part), ~vctrs::vec_ptype_abbr(data_part[[.x]])),
    row.names = NULL
  )

  if(!is.null(name_separator)) {
    if(rlang::is_character(name_separator, n = 1)) {
      x <-
        x %>%
        tidyr::separate_wider_delim(cols = "col_name",
                                    delim = name_separator,
                                    names = c("name_prefix", "name_suffix"),
                                    cols_remove = FALSE,
                                    too_few = "align_end", too_many = "merge")
    } else cli::cli_abort("Non-string {.arg name_separator} currently not supported.")
  } else x <- x %>% dplyr::mutate(name_prefix = .data$col_name,
                                  name_suffix = .data$col_name)

  if(!is.null(label_separator)) {
    if(rlang::is_character(label_separator, n = 1)) {
      x <-
        x %>%
        tidyr::separate_wider_delim(cols = "col_label",
                                    names = c("label_prefix", "label_suffix"),
                                    delim = label_separator,
                                    too_few = "align_end", too_many = "merge")


    }  else cli::cli_abort("Non-string {.arg label_separator} currently not supported.")
  } else x <- x %>% dplyr::mutate(label_prefix = .data$col_label,
                                  label_suffix = .data$col_label,
                                  col_label = NULL)
  grouping_vars <-
    c(if(!is.null(label_separator)) "label_prefix",
      if(!is.null(name_separator)) "name_prefix")

  x %>%
    dplyr::mutate(

      name_prefix = dplyr::if_else(
        is.na(.data$name_prefix) & !is.na(.data$name_suffix),
        .data$name_suffix,
        .data$name_prefix),


      name_suffix = dplyr::if_else(
        is.na(.data$name_suffix) & !is.na(.data$name_prefix),
        .data$name_prefix,
        .data$name_suffix),

      label_prefix = dplyr::if_else(
        is.na(.data$label_prefix) & !is.na(.data$label_suffix),
        .data$label_suffix,
        .data$label_prefix),

      label_suffix = dplyr::if_else(
        is.na(.data$label_suffix) & !is.na(.data$label_prefix),
        .data$label_prefix,
        .data$label_suffix),

    ) %>%
    dplyr::relocate("col_pos", "col_name", "name_prefix", "name_suffix",
                    "label_prefix", "label_suffix", "col_type") %>%
    dplyr::mutate(col_group = dplyr::cur_group_id(),
                  .by = tidyselect::all_of(if(length(grouping_vars)>0) grouping_vars else "col_pos"))

  ### Return a grouped tibble with
  ### main question variable name prefix,
  ### main question variable label (prefix),
  ### subquestion variable name suffix,
  ### subquestion variable label (suffix)
  ### var_group,
  ### col_type,
  ### designated_role, designated_type, uni_bi_variate,
}
#




#' Processes A 'data_overview' Data Frame
#' Take Kapitteloversikt.xlsx, read it in, and send it to here.
#'
#' @param data_overview A data frame containing the columns:
#' @param data Optional full survey data set for which columns can be looked up.
#' @param group_by Character vector of colnames used for identifying chapters and sections.
#' @param sort_by Character vector of of colnames used to order pieces within each group_by combination.
#' @param label_separator Optional string for separating label between main question and sub-item.
#' @param name_separator Optional string for separating column name between main question and sub-item.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' saros::refine_data_overview(ex_survey_ch_overview)
refine_data_overview <- function(data_overview,
                                 data = NULL,
                                 group_by = c("chapter", "col_group", "label_prefix"),

                                 sort_by = NULL,
                                   # list(
                                   #   "chapter" = NULL,
                                   #   "col_group" = NULL, # Possibly each unique label_prefix-name_prefix combination
                                   #   "label_prefix" = NULL, # First part of variable label - usually battery main question
                                   #   "name_prefix" = NULL, # First part of variable name - often battery identifier
                                   #   "designated_role" = c("dep", "indep", "bi"), #* Tiltenkt rolle: avhengig, uavhengig, bivariat
                                   #   "designated_type" = c("cat", "int", "chr"), # Tiltenkt håndtering: kontinuerlig/intervall eller kategorisk
                                   #   "label_suffix" = NULL, # Underspørsmålet

                                     # "irrelevant_col" = NULL, # Any column in data_overview
                                     # "col_name" = NULL, # Variable name
                                     # "name_suffix" = NULL, # Second part of variable name - usually battery item identifier
                                     # "col_type" = c("chr", "int", "dbl", "fct", "ord", "lgl") # Actual data type
                                   # ),

                                 label_separator = NULL,
                                 name_separator = NULL) {
  col_headers <- c("dep_int", "dep_cat", "dep_text",
                   "indep_int", "indep_cat", "indep_text")
  data_present <- !is.null(data) && is.data.frame(data)
  out <-
    data_overview %>%
    tidyr::separate_longer_delim(cols = tidyselect::any_of(col_headers),
                                 delim = stringr::regex(",| ")) %>%
    tidyr::pivot_longer(cols = tidyselect::any_of(col_headers), values_to = "col_spec") %>%
    tidyr::separate(col = .data$name, into = c("designated_role", "designated_type"), sep="_") %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    dplyr::filter(!.data$col_spec == "") %>%
    dplyr::arrange(.data$designated_role, .data$designated_type) %>%
    dplyr::relocate(tidyselect::all_of(c("designated_role", "designated_type", "col_spec")))
  if(data_present) {
    out %>%
    dplyr::mutate(cols = eval_cols(x = .data$col_spec,
                                 data = .env$data,
                                 call = rlang::caller_env())) %>%
     tidyr::unnest_longer(col = "cols", values_to = "col_pos", indices_to = "col_name") %>%
     dplyr::left_join(y=look_for_extended(data = data,
                                          label_separator = label_separator,
                                          name_separator = name_separator),
                      by = dplyr::join_by("col_pos", "col_name")) %>%
      dplyr::group_by(dplyr::pick(tidyselect::all_of(group_by))) %>%
      dplyr::arrange(dplyr::pick(tidyselect::all_of(names(sort_by))))
  } else out
}
