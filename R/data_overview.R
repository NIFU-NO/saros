
eval_cols <- function(x, data,
                      call = rlang::caller_env()) {
  check_string(x = x, n = NULL, null.ok = FALSE, call = call)
  check_data_frame(data, call = call)
  x_cond_evaluate <- !is.na(x) & stringi::stri_length(x)>0
  lapply(seq_along(x), function(i) {
      if(x_cond_evaluate[i]) {
        expr <- stringi::stri_c('tidyselect::eval_select(expr = rlang::expr(c(',
                            x[i],
                            ')), data = data)',
                            ignore_null=TRUE)
        out <- rlang::try_fetch(eval(parse(text = expr)),
                 error = function(e) cli::cli_abort("Column {.var {x[i]}} doesn't exist in data.",
                                                    call = call)
          )

      } else out <- NA_integer_
    })

}


look_for_extended <- function(data,
                              cols = colnames(data),
                              label_separator = NULL,
                              name_separator = NULL) {
  ### Assume that related columns always have identical label prefix AND overlapping response categories.
  ### Assume that variables with identical label prefix may not be related.
  ### Assume that related columns are always next to each other OR share same variable name prefix.

  data_part <- data[,cols, drop=FALSE]
  if(ncol(data_part) == 0 || nrow(data_part) == 0) cli::cli_abort("data.frame is of 0 length.")


  .variable_position <- match(colnames(data_part), colnames(data))
  .variable_name <- colnames(data_part)
  .variable_label <- get_raw_labels(data = data_part)
  .variable_type <- as.character(unlist(lapply(names(data_part), function(.x) vctrs::vec_ptype_abbr(data_part[[.x]]))))
  if(length(.variable_position) != length(.variable_name) ||
     length(.variable_name) != length(.variable_label) ||
     length(.variable_label) != length(.variable_type)) browser()

  x <- data.frame(.variable_position = .variable_position,
                  .variable_name = .variable_name,
                  .variable_label = .variable_label,
                  .variable_type = .variable_type,
                  row.names = NULL
  )
  check_duplicates <- duplicated(x$.variable_label)
  if(any(check_duplicates)) {
    duplicates <- unique(x$.variable_label[check_duplicates])
    cli::cli_warn(c(i="Found duplicated variable labels, which will likely cause problems if you only group on variable_label(_prefix):"))
    cli::cli_ul(duplicates)
  }

  if(rlang::is_character(name_separator)) {
      if(rlang::is_character(names(name_separator)) &&
         all(c(".variable_name_prefix", ".variable_name_suffix") %in% names(name_separator))) {
        x <-
          tidyr::separate_wider_regex(x,
                                      cols = ".variable_name",
                                      patterns = name_separator,
                                      cols_remove = FALSE,
                                      too_few = "align_start")
        # if(sum(stringi::stri_count_fixed(str = x$.variable_name_suffix, pattern = name_separator), na.rm=TRUE) > 0) {
        #   cli::cli_warn(c("{.arg name_separator} matches more than one delimiter, your output is likely ugly.",
        #                   i="Consider renaming your variables with e.g. {.fun dplyr::rename_with()}."))
        # }


      } else if(rlang::is_string(name_separator) &&
                rlang::is_null(names(name_separator))) {
        x <-
          tidyr::separate_wider_delim(x,
                                      cols = ".variable_name",
                                      delim = name_separator,
                                      names = c(".variable_name_prefix", ".variable_name_suffix"),
                                      cols_remove = FALSE,
                                      too_few = "align_end",
                                      too_many = "merge")
        if(sum(stringi::stri_count_fixed(str = x$.variable_name_suffix, pattern = name_separator), na.rm=TRUE) > 0) {
          cli::cli_warn(c("{.arg name_separator} matches more than one delimiter, your output is likely ugly.",
                          i="Consider renaming your variables with e.g. {.fun dplyr::rename_with()}."))
        }

      } else cli::cli_abort("Unrecognizable {.arg name_separator}: {name_separator}.")


  } else {
    x$.variable_name_prefix <- x$.variable_name
    x$.variable_name_suffix <- x$.variable_name
  }

  if(rlang::is_character(label_separator)) {
    separator_fun <-
      if(rlang::is_character(names(label_separator)) &&
         all(c(".variable_label_prefix", ".variable_label_suffix") %in% names(label_separator))) {
        x <-
          tidyr::separate_wider_regex(x,
                                      cols = ".variable_label",
                                      patterns = label_separator,
                                      cols_remove = FALSE,
                                      too_few = "align_start")

      } else if(rlang::is_string(label_separator) &&
                rlang::is_null(names(label_separator))) {
        x <-
          tidyr::separate_wider_delim(x,
                        cols = ".variable_label",
                        delim = label_separator,
                        names = c(".variable_label_prefix", ".variable_label_suffix"),
                        cols_remove = FALSE,
                        too_few = "align_end",
                        too_many = "merge")
        if(sum(stringi::stri_count_fixed(str = x$.variable_label_suffix, pattern = label_separator), na.rm=TRUE) > 0) {
          cli::cli_warn(c("{.arg label_separator} matches more than one delimiter, your output is likely ugly.",
                          i="Consider renaming your variables with e.g. {.fun labelled::set_variable_labels}."))
        }
      } else cli::cli_abort("Unrecognizable {.arg label_separator}: {label_separator}.")


  } else {
    x$.variable_label_prefix <- x$.variable_label
    x$.variable_label_suffix <- x$.variable_label
  }

  grouping_vars <-
    c(if(!is.null(label_separator)) ".variable_label_prefix",
      if(!is.null(name_separator)) ".variable_name_prefix")

  x %>%
    dplyr::mutate(

      .variable_name_prefix = dplyr::if_else(
        is.na(.data$.variable_name_prefix) & !is.na(.data$.variable_name_suffix),
        .data$.variable_name_suffix,
        .data$.variable_name_prefix),


      .variable_name_suffix = dplyr::if_else(
        is.na(.data$.variable_name_suffix) & !is.na(.data$.variable_name_prefix),
        .data$.variable_name_prefix,
        .data$.variable_name_suffix),

      .variable_label_prefix = dplyr::if_else(
        is.na(.data$.variable_label_prefix) & !is.na(.data$.variable_label_suffix),
        .data$.variable_label_suffix,
        .data$.variable_label_prefix),

      .variable_label_suffix = dplyr::if_else(
        is.na(.data$.variable_label_suffix) & !is.na(.data$.variable_label_prefix),
        .data$.variable_label_prefix,
        .data$.variable_label_suffix),

    ) %>%
    dplyr::relocate(tidyselect::any_of(c(".variable_position", ".variable_name", ".variable_name_prefix", ".variable_name_suffix",
                                         ".variable_label", ".variable_label_prefix", ".variable_label_suffix",
                                         ".variable_type"))) %>%
    dplyr::mutate(.variable_group_id = dplyr::cur_group_id(),
                  .by = tidyselect::all_of(if(length(grouping_vars)>0) grouping_vars else ".variable_position")) %>%
    as.data.frame()

  ### Return a grouped data frame with
  ### main question variable name prefix,
  ### main question variable label (prefix),
  ### subquestion variable name suffix,
  ### subquestion variable label (suffix)
  ### var_group,
  ### .variable_type,
  ### .variable_role, designated_type, uni_bi_variate,
}
#

validate_labels <- function(data) {
  miss_label_vars <- vctrs::vec_slice(data,
                                      is.na(data[[".variable_label_prefix"]]) &
                                        !is.na(data[[".variable_position"]]))
  if(nrow(miss_label_vars) > 0) {
    cli::cli_warn("Using variable name in place of missing label for {.var {unique(miss_label_vars$.variable_name)}}.")
  }
  # if(data$.variable_label)
  data$.variable_label_prefix <- dplyr::if_else(is.na(data$.variable_label_prefix) & !is.na(data$.variable_position), data$.variable_name, data$.variable_label_prefix)
  data$.variable_label_suffix <- dplyr::if_else(is.na(data$.variable_label_suffix) & !is.na(data$.variable_position), data$.variable_name, data$.variable_label_suffix)
  data
}




add_element_names <- function(refined_chapter_overview, element_names) {

  out <- vctrs::vec_slice(refined_chapter_overview,
                          !is.na(refined_chapter_overview$.variable_name_dep))
  out <- tidyr::expand_grid(out, .element_name = element_names)
  out_na <- vctrs::vec_slice(refined_chapter_overview,
                               is.na(refined_chapter_overview$.variable_name_dep))

  dplyr::bind_rows(out_na, out)
}


set_vars_as_factor_with_na <- function(chapter_overview,
                                       data,
                                       element_names) {

  if(!is.null(chapter_overview$.element_name)) {
    has_na <- any(is.na(chapter_overview$.element_name))
    chapter_overview$.element_name <- forcats::fct(x = chapter_overview$.element_name, levels = element_names)
    if(has_na) {
      chapter_overview$.element_name <- forcats::fct_na_value_to_level(chapter_overview$.element_name)
      chapter_overview$.element_name <- forcats::fct_relevel(chapter_overview$.element_name, NA)
    }
  }

  if(!is.null(chapter_overview$.variable_name_dep)) {
    has_na <- any(is.na(chapter_overview$.variable_name_dep))
    all_na <- all(is.na(chapter_overview$.variable_name_dep))
    if(!all_na) {
      chapter_overview$.variable_name_dep <- forcats::fct(x = chapter_overview$.variable_name_dep,
                                             levels = colnames(data)[colnames(data) %in% chapter_overview$.variable_name_dep])
      if(has_na) {
        chapter_overview$.variable_name_dep <- forcats::fct_na_value_to_level(chapter_overview$.variable_name_dep)
        chapter_overview$.variable_name_dep <- forcats::fct_relevel(chapter_overview$.variable_name_dep, NA)
      }
    }
  }
  if(!is.null(chapter_overview$.variable_name_indep)) {
    has_na <- any(is.na(chapter_overview$.variable_name_dep))
    all_na <- all(is.na(chapter_overview$.variable_name_dep))
    if(!all_na) {
      chapter_overview$.variable_name_indep <- forcats::fct(x = chapter_overview$.variable_name_indep,
                                               levels = colnames(data)[colnames(data) %in% chapter_overview$.variable_name_indep])
      if(has_na) {
        chapter_overview$.variable_name_indep <- forcats::fct_na_value_to_level(chapter_overview$.variable_name_indep)
        chapter_overview$.variable_name_indep <- forcats::fct_relevel(chapter_overview$.variable_name_indep, NA)
      }
    }
  }
  chapter_overview
}


split_if_single_y_bivariates <-
  function(chapter_overview,
           data,
           single_y_bivariates_if_indep_cats_above = NA,
           single_y_bivariates_if_deps_above = NA,
           variable_group_dep = ".variable_group_dep",
           organize_by = NULL) {

    chapter_overview <-
      dplyr::mutate(chapter_overview,
                    .single_y_bivariate =
                      unlist(lapply(.data$.variable_name_indep,
                                    function(col) {
                                      !is.na(col) && dplyr::n_distinct(data[[col]], na.rm = TRUE) > single_y_bivariates_if_indep_cats_above
                                    })) |
                      dplyr::n() > single_y_bivariates_if_deps_above,
                    .by = tidyselect::all_of(organize_by))

    chapter_overview <-
      tidyr::unite(chapter_overview,
                   col = !!variable_group_dep,
                   tidyselect::all_of(c(".variable_name_dep", ".variable_name_indep")),
                   sep = "___", remove = FALSE, na.rm = TRUE)

    chapter_overview[[variable_group_dep]] <-
      ifelse(chapter_overview$.single_y_bivariate,
             as.character(chapter_overview[[variable_group_dep]]),
             as.character(chapter_overview[[".variable_label_prefix_dep"]]))

# browser()
    chapter_overview[[variable_group_dep]] <-
      factor(chapter_overview[[variable_group_dep]],
                        levels = unique(chapter_overview[[variable_group_dep]]))
    chapter_overview[[variable_group_dep]] <-
      as.integer(chapter_overview[[variable_group_dep]])
    chapter_overview$.single_y_bivariate <- NULL

    chapter_overview

}





#' Processes A 'chapter_overview' Data Frame
#'
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#' @param progress *Whether to display progress message*
#'
#'    `scalar<logical>` // *default:* `TRUE`
#' @param variable_group_dep *Name for the variable_group_dep column*
#'
#'   `scalar<string>` // *default:* `".variable_group_dep"`
#'
#'   This column is used to group variables that are part of the same bivariate analysis.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' ref_df <- refine_chapter_overview(chapter_overview = ex_survey_ch_overview)
#' ref_df2 <- refine_chapter_overview(chapter_overview = ex_survey_ch_overview,
#'                      data = ex_survey)
refine_chapter_overview <-
  function(chapter_overview = NULL,
           data = NULL,
           ...,
           progress = TRUE,
           variable_group_dep = ".variable_group_dep",
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)

    if(all(.saros.env$refined_chapter_overview_columns %in% names(chapter_overview))) {
      return(chapter_overview)
    }

    if(progress) cli::cli_progress_message(msg = "Refining chapter_overview...\n")


  col_headers <- c("dep", "indep")
  if(!rlang::is_null(chapter_overview) &&
     !any(colnames(chapter_overview) == "dep")) {
    cli::cli_abort("{.arg chapter_overview} must contain columns `dep` (and optionally `indep`).")
  } else {
    if(rlang::is_null(chapter_overview)) {
      chapter_overview <- data.frame(chapter = "All", dep = "everything()")
    }
  }
  data_present <- !is.null(data) && is.data.frame(data)


  ## separate function from here
  delim_regex <- "[,[:space:]]+"
  attr(delim_regex, "options") <-
    list(case_insensitive = FALSE,
         comments = FALSE,
         dotall = FALSE,
         multiline = FALSE)
  class(delim_regex) <- c("stringr_regex", "stringr_pattern", "character")

  out <-
    tidyr::pivot_longer(chapter_overview,
                        cols = tidyselect::any_of(col_headers),
                        values_to = ".variable_selection")
  out <-
    vctrs::vec_slice(out,
                     !(out$name == "indep" &
                         (is.na(out$.variable_selection) |
                            out$.variable_selection == "")))
  out <-
    tidyr::separate_longer_delim(out,
                                 cols = ".variable_selection",
                                 delim = delim_regex)
  out <-
    tidyr::separate(out,
                    col = .data$name,
                    into = ".variable_role",
                    sep="_")

  out[[".variable_role"]] <-
    ifelse(is.na(out[[".variable_selection"]]) |
             out[[".variable_selection"]] == "", NA_character_, out[[".variable_role"]])

  out[[".variable_selection"]] <-
    dplyr::if_else(!stringi::stri_detect(out$.variable_selection,
                                         regex = "matches\\(") &
                     stringi::stri_detect(out$.variable_selection,
                                          regex = "\\*"),
                   true = stringi::stri_c(ignore_null=TRUE,
                                          "matches('",
                                          out$.variable_selection,
                                          "')"),
                   false = out$.variable_selection)
  out <-
    dplyr::distinct(out, .keep_all = TRUE)
  out <-
    dplyr::relocate(out, tidyselect::all_of(c(".variable_role", ".variable_selection")))

  # to here

  if(data_present) {

    out$.variable_selection <-
      stringi::stri_replace_all_fixed(out$.variable_selection,
                                      pattern = '\"',
                                      replacement = "'")
    out$.variable_selection <-
      stringi::stri_replace_all_regex(out$.variable_selection,
                                      pattern = '[[:space:],]+',
                                      replacement = ",")

    out$.cols <- eval_cols(x = out$.variable_selection,
                          data = data,
                          call = call)

    out <-
      tidyr::unnest_longer(out,
                           col = ".cols",
                           values_to = ".variable_position",
                           indices_to = ".variable_name")
    out$.variable_name <- ifelse(out$.variable_name %in% c("1", ""), NA, out$.variable_name)


    if(rlang::is_true(dots$hide_variable_if_all_na)) {
      na_vars <- c()

      for(var in unique(out$.variable_name)) {
        if(!is.na(var) && all(is.na(data[[var]]))) {
          na_vars <- c(na_vars, var)
        }
      }

      out <- out[!out$.variable_name %in% na_vars, ]
    }

    # check_duplicates_in_chapter_overview(out)

    present_variable_names <-
      stringi::stri_remove_empty_na(unique(out$.variable_name))

    if(length(present_variable_names)>0) {
      out <-
        dplyr::left_join(x=out,
                         y=look_for_extended(data = data,
                                             cols = present_variable_names,
                                             label_separator = dots$label_separator,
                                             name_separator = dots$name_separator),
                         by = dplyr::join_by(".variable_position", ".variable_name"))



      out <-
        trim_columns(out, cols = c(".variable_label_prefix", ".variable_label_prefix"))
      out <-
        validate_labels(out)
    }
    out <- # TASK: SIMPLIFY INDEP IN data_overview
        attach_indep2(out)



      out <- # TASK: SIMPLIFY INDEP IN data_overview
        remove_non_significant_bivariates2(out,
                                           data = data,
                                           hide_bi_entry_if_sig_above = dots$hide_bi_entry_if_sig_above,
                                           always_show_bi_for_indep = dots$always_show_bi_for_indep,
                                           progress = progress)


    out <- add_element_names(out, element_names = dots$element_names)


    out <-
      dplyr::distinct(out,
                      dplyr::pick(tidyselect::everything()), #all_of(c("chapter", ".variable_position", ".element_name")
                      .keep_all = TRUE)
    out <-
      vctrs::vec_slice(out, # Remove bivariate entries without an indep variable
                       !((is.na(out$.variable_name_indep) &
                           !is.na(out$.element_name) &
                           stringi::stri_detect_regex(out$.element_name, pattern="^bi_")) |
                           !is.na(out$.variable_name_indep) &
                           !is.na(out$.element_name) &
                           stringi::stri_detect_regex(out$.element_name, pattern = "^uni_")))

    tapplied <- unlist(lapply(split(out, out$chapter), function(df) length(unique(df$.variable_name_dep))))
    # tapplied <- tapply(out, out$chapter, FUN = function(df) length(unique(df$.variable_name_dep)))
    if(length(unique(out$chapter)) > 1 &&
       max(tapplied) == ncol(data)) {
      cli::cli_warn("One of your chapters contain all the variables in the dataset. Is this what you intend?")
    }
    log_unused_variables(data=data,
                         chapter_overview=out,
                         auxiliary_variables=dots$auxiliary_variables,
                         mesos_var = dots$mesos_var,
                         log_file = dots$log_file)

    out <-
      set_vars_as_factor_with_na(chapter_overview = out,
                                 data = data,
                                 element_names = dots$element_names)


    if(!is.na(dots$single_y_bivariates_if_indep_cats_above)) {
      out <-
        split_if_single_y_bivariates(
          chapter_overview = out,
          data = data,
          single_y_bivariates_if_indep_cats_above = dots$single_y_bivariates_if_indep_cats_above,
          single_y_bivariates_if_deps_above = dots$single_y_bivariates_if_deps_above,
          variable_group_dep = variable_group_dep,
          organize_by = dots$organize_by)
      dots$organize_by <- c(dots$organize_by, variable_group_dep)
    }

  }



  if(!rlang::is_null(out$chapter)) {
    out$chapter <- factor(out$chapter, levels=unique(chapter_overview$chapter))
  }

  sorter_assistant <- function(x) {
    if(is.character(x) || is.numeric(x)) return(x)
    if(is.factor(x)) return(as.integer(x))
  }

  out <-
    dplyr::group_by(out, dplyr::pick(tidyselect::all_of(dots$organize_by[dots$organize_by %in% colnames(out)])))
  out <-
    dplyr::arrange(out, dplyr::across(tidyselect::all_of(dots$arrange_output_by[dots$arrange_output_by %in% colnames(out)]),
                                      ~sorter_assistant(.x)))

  out
}
