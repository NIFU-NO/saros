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

    levels(data_summary[[".category"]])[
      levels(data_summary[[".category"]]) == ""
    ] <- NA_character_

    if (dots$showNA == "never") {
      data_summary[[".category"]] <- forcats::fct_na_level_to_value(data_summary[[".category"]])
    } else {
      data_summary[[".category"]] <- forcats::fct_na_value_to_level(data_summary[[".category"]])
    }
    # levels(data_summary[[".category"]])[
    #   is.na(levels(data_summary[[".category"]])) |
    #     levels(data_summary[[".category"]]) == ""
    # ] <- "NA"
    cat_lvls <- levels(data_summary[[".category"]])

    if (length(indep_label) == 1 && length(dots$indep) == 0) {
      cli::cli_abort("Something wrong in function.")
    }
    col_as_basis <-
      if (all(!is.na(data_summary[[".variable_label"]]))) {
        ".variable_label"
      } else {
        cli::cli_warn("No variable labels found for {.var {sort(unique(data_summary[['.variable_name']]))}}. Using variable names.")

        ".variable_name"
      }

    data_out <-
      data_summary |>
      dplyr::arrange(
        as.integer(factor(.data[[col_as_basis]])),
        if (length(dots$indep) > 0) as.integer(.data[[dots$indep]])
      )
    if (length(cat_lvls) <= dots$n_categories_limit) {
      data_out <-
        data_out |>
        tidyr::pivot_wider(
          id_cols = tidyselect::all_of(c(col_as_basis, dots$indep, ".count_per_indep_group")),
          names_from = ".category", values_from = ".data_label",
          names_expand = TRUE
        )
      new_col_order <-
        c(col_as_basis, dots$indep, cat_lvls, ".count_per_indep_group")
      data_out <-
        data_out |>
        dplyr::relocate(tidyselect::all_of(new_col_order), .after = 1) |>
        dplyr::rename_with(
          .cols = tidyselect::all_of(cat_lvls),
          .fn = ~ stringi::stri_c(ignore_null = FALSE, .x, if (dots$data_label %in% c("percentage", "percentage_bare")) " (%)")
        ) |>
        dplyr::rename_with(
          .cols = ".count_per_indep_group",
          .fn = function(x) dots$translations$table_heading_N
        )
    } else {
      data_out <- data_out[, c(col_as_basis, ".category", ".count", ".data_label"), drop = FALSE] |>
        dplyr::rename_with(
          .cols = ".count",
          .fn = function(x) dots$translations$table_heading_N
        ) |>
        dplyr::rename_with(
          .cols = ".data_label",
          .fn = function(x) dots$translations$table_heading_data_label
        )
    }
    # browser()
    # names(data_out)[names(data_out) == "NA"] <- "NA"

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
        .cols = col_as_basis,
        .fn = function(x) dots$main_question
      )
    }
    data_out[[1]] <- if (dplyr::n_distinct(data_out[[1]], na.rm = FALSE) > 1) data_out[[1]]

    data_out
  }
