#' @export
make_content.int_table_html <-
  function(...) {
    dots <- rlang::list2(...)
    out <- dots$data_summary

    if (nrow(out) == 0) {
      return(data.frame())
    }

    # Helper function to round numeric statistics
    round_numeric_stats <- function(data, digits) {
      numeric_cols <- c(
        ".mean",
        ".mean_se",
        ".median",
        "mean",
        "sd",
        "median",
        "mad",
        "iqr",
        "min",
        "max",
        "Mean",
        "Mean_SE",
        "Median",
        "SD",
        "MAD",
        "IQR",
        "Min",
        "Max"
      )
      for (col in intersect(numeric_cols, colnames(data))) {
        data[[col]] <- round(data[[col]], digits = digits)
      }
      data
    }

    # Helper function for column renaming
    get_col_renaming_fn <- function(main_question, use_header) {
      function(col) {
        if (
          col %in%
            c(".variable_label", ".variable_label_suffix", ".variable_name")
        ) {
          if (use_header && nchar(main_question) > 0) {
            main_question
          } else {
            ".variable_label"
          }
        } else if (col == "n") {
          "N"
        } else if (col == "n_valid") {
          "N_valid"
        } else if (col == "n_miss") {
          "N_missing"
        } else if (col == ".count") {
          "N"
        } else if (col == ".mean") {
          "Mean"
        } else if (col == ".mean_se") {
          "Mean_SE"
        } else if (col == ".median") {
          "Median"
        } else if (col == "mean") {
          "Mean"
        } else if (col == "sd") {
          "SD"
        } else if (col == "median") {
          "Median"
        } else if (col == "mad") {
          "MAD"
        } else if (col == "iqr") {
          "IQR"
        } else if (col == "min") {
          "Min"
        } else if (col == "max") {
          "Max"
        } else {
          col
        }
      }
    }

    # Helper function to process data (select, rename, handle conflicts)
    process_data <- function(
      data,
      col_basis,
      indep_label,
      main_question,
      use_header
    ) {
      # Select relevant columns
      stat_cols <- c(
        "n",
        "n_valid",
        "n_miss",
        ".count",
        ".mean",
        ".mean_se",
        ".median",
        "mean",
        "sd",
        "median",
        "mad",
        "iqr",
        "min",
        "max"
      )
      output_cols <- c(
        col_basis,
        dots$indep,
        intersect(stat_cols, colnames(data))
      )
      data <- data[, output_cols, drop = FALSE]

      # Rename columns
      colnames(data) <- vapply(
        colnames(data),
        get_col_renaming_fn(main_question, use_header),
        character(1)
      )

      # Handle independent variable naming conflicts
      if (
        length(dots$indep) > 0 &&
          length(indep_label) > 0 &&
          nchar(indep_label[1]) > 0
      ) {
        final_indep_name <- if (indep_label[1] == colnames(data)[1]) {
          paste0(indep_label[1], " (indep)")
        } else {
          indep_label[1]
        }
        data <- dplyr::rename_with(
          data,
          .cols = tidyselect::all_of(dots$indep),
          .fn = ~final_indep_name
        )
      }
      data
    }

    # Determine which variable column to use (label or name)
    col_as_basis <-
      if (all(!is.na(out[[".variable_label"]]))) {
        ".variable_label"
      } else {
        cli::cli_warn(
          "No variable labels found for {.var {sort(unique(out[['.variable_name']]))}}. Using variable names."
        )
        ".variable_name"
      }

    # Handle independent variable labels if present
    indep_label <- character()
    if (length(dots$indep) > 0) {
      indep_label <- get_raw_labels(data = dots$data, col_pos = dots$indep)
      indep_label <- get_main_question(
        indep_label,
        label_separator = dots$label_separator,
        warn_multiple = TRUE
      )
      indep_label <- unique(indep_label)
      if (nchar(indep_label) == 0) indep_label <- dots$indep[1]
    }

    main_question <- ""
    if (
      !is.null(dots$label_separator) &&
        nchar(dots$label_separator) > 0 &&
        col_as_basis == ".variable_label"
    ) {
      main_question <- if (
        !is.null(dots$main_question) && nchar(dots$main_question) > 0
      ) {
        dots$main_question
      } else if (".variable_label_prefix" %in% colnames(out)) {
        as.character(unique(out[[".variable_label_prefix"]]))[1]
      } else {
        ""
      }

      if (nchar(main_question) > 0) {
        out[[".variable_label_suffix"]] <- keep_subitem(
          fct = out[[".variable_label"]],
          label_separator = dots$label_separator,
          ordered = is.ordered(out[[".variable_label"]]),
          call = rlang::caller_env()
        )
        col_as_basis <- ".variable_label_suffix"
      }
    }

    # Process main data
    data_out <- out |>
      dplyr::arrange(
        as.integer(factor(.data[[col_as_basis]])),
        if (length(dots$indep) > 0) as.integer(factor(.data[[dots$indep[1]]]))
      ) |>
      process_data(
        col_as_basis,
        indep_label,
        main_question,
        dots$table_main_question_as_header
      )

    # Add totals if requested
    if (isTRUE(dots$totals) && length(dots$indep) > 0) {
      totals_data <- summarize_int_cat_data(dots$data, dots$dep, indep = NULL)
      totals_data[[dots$indep[1]]] <- dots$translations$totals_name %||% "Total"

      if (
        col_as_basis == ".variable_label_suffix" &&
          !is.null(dots$label_separator)
      ) {
        totals_data[[".variable_label_suffix"]] <- keep_subitem(
          totals_data[[".variable_label"]],
          dots$label_separator,
          ordered = is.ordered(totals_data[[".variable_label"]]),
          call = rlang::caller_env()
        )
      }

      totals_processed <- process_data(
        totals_data,
        col_as_basis,
        indep_label,
        main_question,
        dots$table_main_question_as_header
      )
      data_out <- rbind(
        as.data.frame(data_out),
        as.data.frame(totals_processed)
      )
    }

    # Apply rounding and return
    round_numeric_stats(data_out, dots$digits)
  }
