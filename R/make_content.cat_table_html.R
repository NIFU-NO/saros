#' @export
make_content.cat_table_html <-
  function(...) {
    dots <- rlang::list2(...)
    data_summary <- dots$data_summary

    if (dots$data_label %in% c("percentage", "percentage_bare", "proportion")) {
      data_label2 <- "count"
    } else {
      data_label2 <- "percentage"
    }




    ######### MUST TIDY UP FROM HERE ############

    if (length(dots$indep) > 0) {
      indep_label <- get_raw_labels(data = dots$data, col_pos = dots$indep)
      indep_label <- get_main_question(indep_label,
        label_separator = dots$label_separator,
        warn_multiple = TRUE
      )
      indep_label <- unique(indep_label)
      if (nchar(indep_label) == 0) indep_label <- dots$indep[1] # browser() #cli::cli_warn("Indep {.var {indep_pos}} lacks a label.")
    } else {
      indep_label <- character(0)
    }

    # indep_label <- unname(get_raw_labels(data = dots$data, col_pos = dots$indep))

    cat_lvls <- levels(data_summary[[".category"]])
    cat_lvls[is.na(cat_lvls)] <- "NA"

    if (length(indep_label) == 1 && length(dots$indep) == 0) {
      cli::cli_abort("Something wrong in function.")
    }

    data_out <-
      data_summary |>
      dplyr::arrange(
        as.integer(.data[[".variable_label"]]),
        if (length(dots$indep) > 0) as.integer(.data[[dots$indep]])
      ) |>
      tidyr::pivot_wider(
        id_cols = tidyselect::all_of(c(".variable_label", dots$indep, ".count_total")),
        names_from = ".category", values_from = ".data_label",
        names_expand = TRUE
      )
    names(data_out)[names(data_out) == "NA"] <- "NA"
    new_col_order <-
      c(".variable_label", dots$indep, cat_lvls, ".count_total")
    data_out <-
      data_out |>
      dplyr::relocate(tidyselect::all_of(new_col_order), .after = 1) |>
      dplyr::rename_with(
        .cols = tidyselect::all_of(cat_lvls),
        .fn = ~ stringi::stri_c(ignore_null = TRUE, .x, if (dots$data_label %in% c("percentage", "percentage_bare")) " (%)")
      ) |>
      dplyr::rename_with(
        .cols = ".count_total",
        .fn = function(x) dots$translations$table_heading_N
      )
    if (length(dots$indep) > 0 &&
      is.character(indep_label) &&
      length(indep_label) == length(dots$indep) &&
      all(nchar(indep_label) > 0)) {
      data_out <- dplyr::rename_with(data_out,
        .cols = tidyselect::all_of(dots$indep),
        .fn = function(x) indep_label
      )
    }

    if (isTRUE(dots$table_main_question_as_header) &&
      rlang::is_string(dots$main_question) && stringi::stri_length(dots$main_question) > 0) {
      data_out <- dplyr::rename_with(data_out,
        .cols = ".variable_label",
        .fn = function(x) dots$main_question
      )
    }
    data_out[[1]] <- if (dplyr::n_distinct(data_out[[1]], na.rm = FALSE) > 1) data_out[[1]]

    data_out
  }
