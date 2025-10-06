#' @export
make_content.chr_table_html <-
  function(...) {
    dots <- rlang::list2(...)

    # For character tables, we work with raw data, not data_summary
    data <- dots$data
    dep <- dots$dep
    indeps <- dots$indep %||% character(0)

    # Early exit if no data
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }

    # Extract the dependent variable (character responses)
    validate_single_dep_var(dep, "chr_table_html")

    # Get character responses, filter out NA and empty values
    char_responses <- data[[dep]]
    if (is.factor(char_responses)) {
      char_responses <- as.character(char_responses)
    }

    # Create base result with character responses
    valid_rows <- !is.na(char_responses) & char_responses != ""

    if (!any(valid_rows)) {
      return(data.frame())
    }

    # Filter data to valid responses
    filtered_data <- dplyr::filter(data, valid_rows)

    # Start with the character responses
    result <- data.frame(
      .variable_name_dep = filtered_data[[dep]],
      stringsAsFactors = FALSE
    )

    if (
      isTRUE(dots$table_main_question_as_header) &&
        rlang::is_string(attr(filtered_data[[dep]], "label"))
    ) {
      names(result)[1] <- attr(filtered_data[[dep]], "label")
    } else {
      names(result)[1] <- dep
    }
    # Add background information from independent variables
    if (length(indeps) > 0) {
      for (ind_var in indeps) {
        if (ind_var %in% colnames(filtered_data)) {
          # Add the independent variable as background info
          result[[ind_var]] <- as.character(filtered_data[[ind_var]])

          # Get variable label if available
          var_label <- attr(filtered_data[[ind_var]], "label")
          if (
            rlang::is_string(var_label) &&
              isTRUE(dots$table_main_question_as_header)
          ) {
            colnames(result)[which(
              colnames(result) == ind_var
            )] <- var_label
          }
        }
      }
    }

    result
  }
