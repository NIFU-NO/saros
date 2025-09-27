#' @export
make_content.cat_table_html <-
  function(...) {
    dots <- rlang::list2(...)
    data_summary <- dots$data_summary

    # Setup data and early exit if empty
    if (nrow(data_summary) == 0) return(data.frame())
    
    # Get independent variable labels
    indep_label <- get_indep_labels(dots)
    
    # Process categorical NA handling
    data_summary <- process_categorical_na(data_summary, dots)
    
    # Exit again if data was filtered out
    if (nrow(data_summary) == 0) return(data.frame())

    cat_lvls <- levels(data_summary[[".category"]])

    if (length(indep_label) == 1 && length(dots$indep) == 0) {
      cli::cli_abort("Something wrong in function.")
    }
    
    # Determine column basis
    col_basis <- determine_variable_basis(data_summary)

    # Arrange data
    data_out <- arrange_table_data(data_summary, col_basis, dots$indep)
    
    if (length(cat_lvls) <= dots$n_categories_limit) {
      # Wide table format
      data_out <-
        data_out |>
        tidyr::pivot_wider(
          id_cols = tidyselect::all_of(c(
            col_basis,
            dots$indep,
            ".count_per_indep_group"
          )),
          names_from = ".category",
          values_from = ".data_label",
          names_expand = TRUE
        )
      new_col_order <-
        c(col_basis, dots$indep, cat_lvls, ".count_per_indep_group")

      data_out <-
        data_out |>
        dplyr::relocate(tidyselect::all_of(new_col_order), .after = 1) |>
        dplyr::rename_with(
          .cols = tidyselect::all_of(cat_lvls),
          .fn = ~ stringi::stri_c(
            ignore_null = FALSE,
            .x,
            if (dots$data_label %in% c("percentage", "percentage_bare")) {
              " (%)"
            } else {
              ""
            }
          )
        ) |>
        dplyr::rename_with(
          .cols = ".count_per_indep_group",
          .fn = function(x) dots$translations$table_heading_N
        )
    } else {
      # Long table format
      data_out <- data_out[,
        c(col_basis, dots$indep, ".category", ".data_label", ".count"),
        drop = FALSE
      ] |>
        dplyr::rename_with(
          .cols = ".count",
          .fn = function(x) dots$translations$table_heading_N
        ) |>
        dplyr::rename_with(
          .cols = ".data_label",
          .fn = function(x) dots$translations$table_heading_data_label
        )
    }

    # Handle independent variable labels
    if (
      length(dots$indep) > 0 &&
        is.character(indep_label) &&
        length(indep_label) == length(dots$indep) &&
        all(nchar(indep_label) > 0)
    ) {
      data_out <- dplyr::rename_with(
        data_out,
        .cols = tidyselect::all_of(dots$indep),
        .fn = function(x) indep_label
      )
    }

    # Handle main question as header
    if (
      isTRUE(dots$table_main_question_as_header) &&
        rlang::is_string(dots$main_question) &&
        stringi::stri_length(dots$main_question) > 0
    ) {
      data_out <- dplyr::rename_with(
        data_out,
        .cols = col_basis,
        .fn = function(x) dots$main_question
      )
    }
    
    # Hide axis text if single variable
    if (
      isTRUE(dots$hide_axis_text_if_single_variable) &&
        dplyr::n_distinct(data_out[[1]], na.rm = FALSE) == 1
    ) {
      data_out[[1]] <- NULL
    }

    data_out
  }