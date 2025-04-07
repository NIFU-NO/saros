#' @export
make_content.sigtest_table_html <-
  function(...) {
    dots <- rlang::list2(...)

    data <- dots$data
    # data_summary <- dots$data_summary


    out <-
      tidyr::expand_grid(y = dots$dep, x = dots$indep) |>
      dplyr::rowwise() |>
      dplyr::group_map(
        .keep = TRUE,
        .f = ~ {
          if (rlang::is_string(.x$y) &&
            (!rlang::is_string(.x$x) || .x$y != .x$x)) {
            if (rlang::is_string(.x$x)) {
              # Filter out NAs
              data2 <- data[!is.na(data[[.x$y]]) &
                !is.na(data[[.x$x]]), , drop = FALSE]
            } else {
              # Filter out NAs
              data2 <- data[!is.na(data[[.x$y]]), , drop = FALSE]
            }

            y <- data2[[.x$y]]
            x <- if (rlang::is_string(.x$x)) data2[[.x$x]]

            stat_result <- find_test2(y = y, x = x)




            if (nrow(stat_result) >= 1) {
              .x[[".bi_test"]] <- stat_result$.bi_test
              .x[[".p_value"]] <- stat_result$.p_value

              .x <- cbind(.x, simple_descriptives(
                data = data, # Not data2, because we want total n
                y_var = .x$y,
                x_var = .x$x,
                na.rm = dots$showNA %in% c("never"),
                table_wide = dots$table_wide
              ))
            }
            .x
          }
        }
      ) |>
      dplyr::bind_rows()
    if (requireNamespace("scales") && !is.null(out$.p_value)) {
      out[[".p_value"]] <- scales::pvalue(out$.p_value)
    }
    if (rlang::is_string(dots$translations$sigtest_variable_header_1)) {
      names(out)[names(out) == "y"] <- dots$translations$sigtest_variable_header_1
    } else {
      cli::cli_abort("{.arg translations$sigtest_variable_header_1} must be a string, not {.obj_type_friendly {dots$translations$sigtest_variable_header_1}}.")
    }
    if (rlang::is_string(dots$translations$sigtest_variable_header_2)) {
      names(out)[names(out) == "x"] <- dots$translations$sigtest_variable_header_2
    } else {
      cli::cli_abort("{.arg translations$sigtest_variable_header_2} must be a string, not {.obj_type_friendly {dots$translations$sigtest_variable_header_2}}.")
    }

    out
  }
