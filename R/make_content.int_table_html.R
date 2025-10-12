#' @export
make_content.int_table_html <-
  function(...) {
    dots <- rlang::list2(...)

    # Setup data with common helper
    setup_result <- setup_table_data(dots)
    if (setup_result$should_return) {
      return(setup_result$data)
    }
    out <- setup_result$data

    # Get independent variable labels
    indep_label <- get_indep_labels(dots)

    # Determine variable column basis
    col_as_basis <- determine_variable_basis(out)

    # Process main question and extract suffixes
    processing_result <- process_main_question_and_suffixes(
      out,
      dots,
      col_as_basis
    )
    out <- processing_result$data
    main_question <- processing_result$main_question
    col_as_basis <- processing_result$col_basis

    # Process main data
    data_out <- out |>
      arrange_table_data(
        col_basis = col_as_basis,
        indep_vars = dots$indep
      ) |>
      process_table_data(
        col_basis = col_as_basis,
        indep_vars = dots$indep,
        indep_label = indep_label,
        main_question = main_question,
        use_header = dots$table_main_question_as_header,
        stat_columns = c(
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

      totals_processed <- process_table_data(
        totals_data,
        col_basis = col_as_basis,
        indep_vars = dots$indep,
        indep_label = indep_label,
        main_question = main_question,
        use_header = dots$table_main_question_as_header,
        stat_columns = c(
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
      )

      data_out <- rbind(
        as.data.frame(data_out),
        as.data.frame(totals_processed)
      )
    }

    # Apply rounding and return
    round_numeric_stats(data_out, dots$digits)
  }
