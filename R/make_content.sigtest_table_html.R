#' @export
make_content.sigtest_table_html <-
  function(...) {
    dots <- rlang::list2(...)

    data <- dots$data
    # data_summary <- dots$data_summary

    if (!rlang::is_string(dots$translations$sigtest_variable_header_1)) {
      cli::cli_abort(
        "{.arg translations$sigtest_variable_header_1} must be a string, not {.obj_type_friendly {dots$translations$sigtest_variable_header_1}}."
      )
    }
    if (!rlang::is_string(dots$translations$sigtest_variable_header_2)) {
      cli::cli_abort(
        "{.arg translations$sigtest_variable_header_2} must be a string, not {.obj_type_friendly {dots$translations$sigtest_variable_header_2}}."
      )
    }

    out <-
      tidyr::expand_grid(y = dots$dep, x = dots$indep) |>
      dplyr::rowwise() |>
      dplyr::group_map(
        .keep = TRUE,
        .f = ~ {
          if (
            rlang::is_string(.x$y) &&
              (!rlang::is_string(.x$x) || .x$y != .x$x)
          ) {
            y_var <- .x$y
            x_var <- if (rlang::is_string(.x$x)) .x$x

            if (rlang::is_string(x_var)) {
              # Filter out NAs
              data2 <- data[
                !is.na(data[[y_var]]) &
                  !is.na(data[[x_var]]),
                ,
                drop = FALSE
              ]
            } else {
              # Filter out NAs
              data2 <- data[!is.na(data[[y_var]]), , drop = FALSE]
            }

            y <- data2[[y_var]]
            x <- if (rlang::is_string(x_var)) data2[[x_var]]

            stat_result <- find_test2(y = y, x = x)

            # Rename the placeholder columns before anything is bound to them,
            # so that variables actually named `x` or `y` cannot collide with
            # them. A long-format descriptives table keeps the name of `indep`
            # as its grouping column.
            names(.x)[
              names(.x) == "y"
            ] <- dots$translations$sigtest_variable_header_1
            names(.x)[
              names(.x) == "x"
            ] <- dots$translations$sigtest_variable_header_2

            if (nrow(stat_result) >= 1) {
              .x[[".bi_test"]] <- stat_result$.bi_test
              .x[[".p_value"]] <- stat_result$.p_value

              .x <- cbind(
                .x,
                simple_descriptives(
                  data = data, # Not data2, because we want total n
                  y_var = y_var,
                  x_var = x_var,
                  na.rm = dots$showNA %in% c("never"),
                  table_wide = dots$table_wide,
                  n_categories_limit = dots$n_categories_limit
                )
              )
            }
            .x
          }
        }
      ) |>
      dplyr::bind_rows()
    if (requireNamespace("scales") && !is.null(out$.p_value)) {
      out[[".p_value"]] <- scales::pvalue(out$.p_value)
    }

    attach_dep_label_prefix(out, dots$main_question)
  }
