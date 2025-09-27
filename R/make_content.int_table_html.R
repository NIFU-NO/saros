#' @export
make_content.int_table_html <-
  function(...) {
    dots <- rlang::list2(...)

    out <- dots$data_summary

    if (nrow(out) == 0) {
      return(data.frame())
    }

    numeric_stats_requiring_rounding <- c(
      "mean",
      "mean_se",
      "median",
      "sd",
      "mad",
      "iqr",
      "min",
      "max"
    )

    # Helper function to round numeric statistics
    round_numeric_stats <- function(data, digits) {
      # Column names in the actual data start with dots

      for (col in numeric_stats_requiring_rounding) {
        if (col %in% colnames(data)) {
          data[[col]] <- round(data[[col]], digits = digits)
        }
      }
      data
    }

    # Helper function to get column renaming function
    get_col_renaming_fn <- function(
      main_question_for_suffix,
      table_main_question_as_header
    ) {
      function(col) {
        switch(
          col,
          ".variable_label" = {
            if (
              isTRUE(table_main_question_as_header) &&
                rlang::is_string(main_question_for_suffix) &&
                stringi::stri_length(main_question_for_suffix) > 0
            ) {
              main_question_for_suffix
            } else {
              ".variable_label"
            }
          },
          ".variable_label_suffix" = {
            if (
              isTRUE(table_main_question_as_header) &&
                rlang::is_string(main_question_for_suffix) &&
                stringi::stri_length(main_question_for_suffix) > 0
            ) {
              main_question_for_suffix
            } else {
              ".variable_label"
            }
          },
          ".variable_name" = {
            if (
              isTRUE(table_main_question_as_header) &&
                rlang::is_string(main_question_for_suffix) &&
                stringi::stri_length(main_question_for_suffix) > 0
            ) {
              main_question_for_suffix
            } else {
              ".variable_label"
            }
          },
          "n" = "N",
          "n_valid" = "N_valid",
          "n_miss" = "N_missing",
          ".count" = "N",
          ".mean" = "Mean",
          ".mean_se" = "Mean_SE",
          ".median" = "Median",
          "mean" = "Mean",
          "sd" = "SD",
          "median" = "Median",
          "mad" = "MAD",
          "iqr" = "IQR",
          "min" = "Min",
          "max" = "Max",
          col # Keep original name if no mapping
        )
      }
    }

    # Helper function to process data (select columns, rename, handle indep labels)
    process_data <- function(
      data,
      col_as_basis,
      dots,
      indep_label,
      main_question_for_suffix
    ) {
      # Statistical columns in logical order (both old and new naming)
      stat_order <- c(
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

      # Select and reorder columns for output
      output_cols <- col_as_basis
      if (length(dots$indep) > 0) {
        output_cols <- c(output_cols, dots$indep)
      }
      available_stats <- intersect(stat_order, colnames(data))
      output_cols <- c(output_cols, available_stats)

      # Select only the columns we want in output
      data <- data[, output_cols, drop = FALSE]

      # Rename columns for better presentation
      colnames(data) <- vapply(
        colnames(data),
        get_col_renaming_fn(
          main_question_for_suffix,
          dots$table_main_question_as_header
        ),
        character(1)
      )

      # Handle independent variable column names
      if (
        length(dots$indep) > 0 &&
          is.character(indep_label) &&
          length(indep_label) == length(dots$indep) &&
          all(nchar(indep_label) > 0)
      ) {
        # Check for column name conflicts with the main variable column
        current_var_col_name <- colnames(data)[1]

        # If there's a naming conflict, add " (indep)" suffix to distinguish them
        final_indep_name <- if (indep_label[1] == current_var_col_name) {
          paste0(indep_label[1], " (indep)")
        } else {
          indep_label[1]
        }

        data <- dplyr::rename_with(
          data,
          .cols = tidyselect::all_of(dots$indep),
          .fn = function(x) final_indep_name
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

    main_question_for_suffix <- ""
    # Extract variable suffixes when label_separator is provided
    if (
      rlang::is_string(dots$label_separator) &&
        nchar(dots$label_separator) > 0 &&
        col_as_basis == ".variable_label"
    ) {
      # Check if we have a main question from makeme (via .variable_label_prefix)
      if (
        rlang::is_string(dots$main_question) && nchar(dots$main_question) > 0
      ) {
        main_question_for_suffix <- dots$main_question
      } else if (".variable_label_prefix" %in% colnames(out)) {
        main_question_for_suffix <- as.character(unique(out[[
          ".variable_label_prefix"
        ]]))[1]
      }

      if (nchar(main_question_for_suffix) > 0) {
        # Extract suffixes by removing the prefix and separator
        out[[
          ".variable_label_suffix"
        ]] <- keep_subitem(
          fct = out[[".variable_label"]],
          label_separator = dots$label_separator,
          ordered = is.ordered(out[[".variable_label"]]),
          call = rlang::caller_env()
        )
        # Use the suffix column as basis instead of the full label
        col_as_basis <- ".variable_label_suffix"
      }
    }

    # Round numeric columns according to digits setting
    out <- round_numeric_stats(out, dots$digits)

    # Prepare output data frame
    data_out <-
      out |>
      dplyr::arrange(
        as.integer(factor(.data[[col_as_basis]])),
        if (length(dots$indep) > 0) {
          # Handle different data types for independent variables
          indep_var <- .data[[dots$indep[1]]]
          if (is.factor(indep_var)) {
            as.integer(indep_var)
          } else {
            as.integer(factor(indep_var))
          }
        }
      )

    # Process main data (select columns, rename, handle indep labels)
    data_out <- process_data(
      data_out,
      col_as_basis,
      dots,
      indep_label,
      main_question_for_suffix
    )

    # Handle totals if requested
    if (isTRUE(dots$totals) && length(dots$indep) > 0) {
      # For totals, we need to go back to the original data and calculate overall statistics
      original_totals <- summarize_int_cat_data(
        data = dots$data,
        dep = dots$dep,
        indep = NULL # No indep for totals
      )

      # Round according to digits
      original_totals <- round_numeric_stats(original_totals, dots$digits)

      # Add the indep column with "Total" value
      original_totals[[dots$indep[1]]] <- dots$translations$totals_name %||%
        "Total"

      # Apply suffix extraction to totals if it was applied to main data
      totals_col_as_basis <- if (col_as_basis == ".variable_label_suffix") {
        # Extract suffixes for totals too
        if (
          !is.null(dots$label_separator) &&
            nchar(dots$label_separator) > 0 &&
            ".variable_label" %in% colnames(original_totals)
        ) {
          original_totals[[".variable_label_suffix"]] <- keep_subitem(
            fct = original_totals[[".variable_label"]],
            label_separator = dots$label_separator,
            ordered = is.ordered(original_totals[[".variable_label"]]),
            call = rlang::caller_env()
          )
        }
        ".variable_label_suffix"
      } else {
        col_as_basis
      }

      # Process totals data using the same helper function
      original_totals <- process_data(
        original_totals,
        totals_col_as_basis,
        dots,
        indep_label,
        main_question_for_suffix
      )

      # Combine with main data
      tryCatch(
        {
          data_out <- dplyr::bind_rows(data_out, original_totals)
        },
        error = function(e) {
          # If bind_rows fails due to factor incompatibility, convert to data.frame and use rbind
          data_out_df <- as.data.frame(data_out)
          original_totals_df <- as.data.frame(original_totals)
          # Convert all factor/ordered columns to character
          for (col in colnames(data_out_df)) {
            if (
              is.factor(data_out_df[[col]]) || is.ordered(data_out_df[[col]])
            ) {
              data_out_df[[col]] <- as.character(data_out_df[[col]])
            }
          }
          for (col in colnames(original_totals_df)) {
            if (
              is.factor(original_totals_df[[col]]) ||
                is.ordered(original_totals_df[[col]])
            ) {
              original_totals_df[[col]] <- as.character(original_totals_df[[
                col
              ]])
            }
          }
          data_out <<- rbind(data_out_df, original_totals_df)
        }
      )
    }

    data_out
  }
