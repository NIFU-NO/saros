crosstable <- function(data,
                       ...) {
  UseMethod("crosstable")
}


#' @export
crosstable.data.frame <-
  function(data,
           dep = colnames(data),
           indep = NULL,
           showNA = eval(formals(makeme)$showNA),
           totals = eval(formals(makeme)$totals),
           translations = eval(formals(makeme)$translations),
           ...,
           call = rlang::caller_env()) {
    showNA <- rlang::arg_match(showNA, values = eval(formals(makeme)$showNA), error_call = call)


    invalid_deps <- dep[!dep %in% colnames(data)]
    if (length(invalid_deps) > 0) {
      cli::cli_abort("Column{?s} {.var {invalid_deps}} {?doesn't/don't} exist.")
    }
    invalid_indeps <- indep[!indep %in% colnames(data)]
    if (length(invalid_indeps) > 0) {
      cli::cli_abort("Column{?s} {.var {invalid_indeps}} {?doesn't/don't} exist.")
    }

    # indep_names <- colnames(data[, indep, drop = FALSE])
    indep_labels <- get_raw_labels(data = data, col_pos = indep)
    col_names <- colnames(data[, dep, drop = FALSE])[!(colnames(data[, dep, drop = FALSE]) %in% indep)]



    if (length(col_names) == 0) {
      return()
    }

    if (length(indep) > 0 && isTRUE(totals)) {
      for (indep_var in indep) {
        data_duplicate <- data

        data_duplicate[[indep_var]] <- forcats::fct_na_value_to_level(data_duplicate[[indep_var]], level = translations$by_total)
        levels(data_duplicate[[indep_var]]) <- rep(translations$by_total,
          length = length(levels(data_duplicate[[indep_var]]))
        )
        # Below is to ensure it works with ordered factors. Still faster than using rbind
        if (is.ordered(data[[indep_var]])) {
          levels(data_duplicate[[indep_var]]) <-
            c(
              levels(data[[indep_var]]),
              levels(data_duplicate[[indep_var]])
            )
        }

        # levels(data_duplicate[[indep_var]]) <- forcats::fct_c(data[[indep_var]], data_duplicate[[indep_var]])
        levels(data[[indep_var]]) <- c(
          levels(data[[indep_var]]),
          levels(data_duplicate[[indep_var]])
        )
        data <- dplyr::bind_rows(data, data_duplicate)
        for (i in seq_len(ncol(data))) {
          attr(data[[i]], "label") <- attr(data_duplicate[[i]], "label")
        }
      }
    }

    output <- # For each dependent variable...
      lapply(stats::setNames(col_names, col_names), function(.x) {
        if (!any(indep == .x)) {
          out <- data
          names(out)[names(out) == .x] <- ".category"
          col <- out$.category

          if ( # !is.character(col) &&
            !is.factor(col) &&
              dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
            out$.category <- factor(col)
            col <- out$.category
          }

          if (showNA == "always" ||
            (showNA == "ifany" && any(is.na(col)))) {
            out$.category <- forcats::fct_na_value_to_level(f = col, level = "NA")
          } else {
            out <- out[!is.na(out$.category), , drop = FALSE]
          }


          for (indep_var in indep) {
            indep_col <- out[[indep_var]]

            if (showNA == "always" ||
              (showNA == "ifany" && any(is.na(indep_col)))) {
              out[[indep_var]] <- forcats::fct_na_value_to_level(f = indep_col, level = "NA")
            } else {
              out <- vctrs::vec_slice(out, !is.na(out[[indep_var]]))
            }
          }

          if (nrow(out) > 0) {
            col <- out$.category

            fct_lvls <-
              if (is.factor(col)) levels(col) else sort(unique(col))

            if (is.character(out$.category)) {
              cli::cli_warn("{.arg {.x}} is {.obj_type_friendly {out$.category}}. Taking its mean is meaningless and results in NAs.",
                call = call
              )
            }



            out <- out[rlang::inject(order(!!!out[, c(indep, ".category"), drop = FALSE])), , drop = FALSE]
            summary_mean <- out
            summary_mean$.mean <- suppressWarnings(as.numeric(summary_mean$.category))
            summary_mean <- tryCatch(
              stats::aggregate(x = .mean ~ ., data = summary_mean[, c(indep, ".mean"), drop = FALSE], FUN = mean, na.rm = TRUE),
              error = function(e) {
                cols <- c(indep, ".mean")
                data.frame(matrix(NA, ncol = length(cols), dimnames = list(NULL, cols)))
              }
            )

            summary_prop <- out
            summary_prop$.count <- 1L

            summary_prop <- tryCatch(
              stats::aggregate(x = summary_prop$.count, by = summary_prop[, c(indep, ".category"), drop = FALSE], FUN = length, simplify = TRUE),
              error = function(e) {
                cols <- c(indep, ".category")
                data.frame(matrix(NA, ncol = length(cols), dimnames = list(NULL, cols)))
              }
            )

            names(summary_prop)[ncol(summary_prop)] <- ".count"

            if (showNA %in% c("never")) {
              grouped_count <- summary_prop[summary_prop$.category != "NA", , drop = FALSE]
            } else {
              grouped_count <- summary_prop
            }


            # Summaries per dep variable (e.g. b_1, b_2)
            summary_prop[[".count_per_dep"]] <- sum(grouped_count$.count, na.rm = TRUE)


            # Summaries per indep group (e.g. males, females)
            grouped_count <- tryCatch(
              stats::aggregate(
                x = grouped_count$.count,
                by = grouped_count[, indep, drop = FALSE],
                FUN = sum, na.rm = TRUE, simplify = TRUE
              ),
              error = function(e) {
                data.frame(matrix(NA, ncol = max(c(1, length(indep))), dimnames = list(NULL, indep)))
              }
            )
            names(grouped_count)[ncol(grouped_count)] <- ".count_per_indep_group"
            summary_prop <- merge(summary_prop, grouped_count, by = indep)
            summary_prop$.proportion <- summary_prop$.count / summary_prop[[".count_per_indep_group"]]


            summary_prop$.category <- factor(
              x = summary_prop$.category,
              levels = fct_lvls,
              labels = fct_lvls,
              exclude = character()
            )
            summary_prop$.variable_label <- unname(get_raw_labels(data = data, col_pos = .x))
            # summary_prop$.mean_base <- as.integer(summary_prop$.category) * summary_prop$.count
            summary_prop$.count_se <- NA_real_
            summary_prop$.proportion_se <- NA_real_
            summary_prop$.mean_se <- NA_real_

            if (length(indep) > 0) {
              out <- dplyr::left_join(summary_prop, summary_mean,
                by = intersect(names(summary_prop), names(summary_mean))
              )
            } else {
              out <- cbind(summary_prop, summary_mean)
            }
            # if(length(indep) > 0) {
            #   merge(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)), all.x = TRUE)
            # } else {
            #   cbind(summary_prop, summary_mean)
            # }

            out$.variable_name <- .x
            out$.variable_position = match(.x, colnames(data))
          } else {
            out <- data.frame(
              .variable_name = .x,
              .variable_position = match(.x, colnames(data)),
              .variable_label = unname(get_raw_labels(data = data, col_pos = .x)),
              .category = factor(NA),
              .count = NA_integer_,
              .count_se = NA_real_,
              .count_per_dep = NA_integer_,
              .count_per_indep_group = NA_integer_,
              .proportion = NA_real_,
              .proportion_se = NA_real_,
              .mean = NA_real_,
              .mean_se = NA_real_
            )
            out[, indep] <- NA_character_
          }
          out
        }
      })
    out <- do.call(rbind, output)
    out <-
      out[
        , # do.call(order, out[c(".variable_name", indep, ".category", ".proportion")]),
        c(
          ".variable_name", 
          ".variable_position",
          ".variable_label",
          ".category",
          ".count", ".count_se",
          ".count_per_dep",
          ".count_per_indep_group",
          ".proportion", ".proportion_se",
          ".mean", ".mean_se",
          # ".mean_base",
          indep
        )
      ]


    for (indep_var in indep) {
      attr(out[[indep_var]], "label") <- indep_labels[[indep_var]]
    }

    out <- dplyr::arrange(out, dplyr::pick(tidyselect::all_of(c(
      ".variable_name", indep, ".category", ".proportion"
    ))))

    rownames(out) <- NULL
    out
  }

#' @export
crosstable.tbl_df <- crosstable.data.frame

#' @export
crosstable.tbl_svy <-
  function(data,
           dep = colnames(data),
           indep = NULL,
           showNA = eval(formals(makeme)$showNA),
           totals = eval(formals(makeme)$totals),
           translations = eval(formals(makeme)$translations),
           ...,
           call = rlang::caller_env()) {
    if (inherits(data, "tbl_svy") &&
      !requireNamespace("srvyr", quietly = TRUE)) {
      cli::cli_abort("Needs {.pkg srvyr} to use tbl_svy objects: {.run install.packages('srvyr')}.")
    }

    showNA <- rlang::arg_match(showNA, values = eval(formals(makeme)$showNA), error_call = call)

    # indep_names <- colnames(data[, indep, drop = FALSE])
    indep_labels <- get_raw_labels(data = data$variables, col_pos = indep)
    col_names <- colnames(data[, dep, drop = FALSE])[!(colnames(data[, dep, drop = FALSE]) %in% indep)]

    # indep_names <- colnames(srvyr::select(data, indep))
    # col_names <- colnames(srvyr::select(data, dep)) |> .[!. %in% indep_names]

    output <- lapply(stats::setNames(col_names, col_names), function(.x) {
      out <- srvyr::rename(data, .category = tidyselect::all_of(.x))
      col <- srvyr::pull(out, .data$.category)

      if (!is.character(col) && !is.factor(col) && dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
        out <- srvyr::mutate(out, .category = factor(.data$col))
        col <- srvyr::pull(out, .data$.category)
      }

      if (showNA == "always" || (showNA == "ifany" && any(is.na(col)))) {
        out <- srvyr::mutate(out, .category = forcats::fct_na_value_to_level(f = col, level = "NA"))
      } else {
        out <- srvyr::filter(out, !is.na(.data$.category))
      }

      if (nrow(out) > 0) {
        col <- srvyr::pull(out, .data$.category)

        fct_lvls <- if (is.factor(col)) levels(col) else sort(unique(col))

        # indep_vars <- colnames(srvyr::select(data, indep))

        for (indep_var in indep) {
          indep_col <- srvyr::pull(out, .data[[indep_var]])

          if (showNA == "always" || (showNA == "ifany" && any(is.na(indep_col)))) {
            out <- srvyr::mutate(out, srvyr::across(
              .cols = tidyselect::all_of(indep_var),
              .fns = ~ forcats::fct_na_value_to_level(f = .x, level = "NA")
            ))
          } else {
            out <- srvyr::filter(out, dplyr::if_all(
              .cols = tidyselect::all_of(indep_var),
              .fns = ~ !is.na(.x)
            ))
          }
        }

        summary_mean <- srvyr::group_by(out, srvyr::across(tidyselect::all_of(indep)))
        summary_mean <- srvyr::summarize(summary_mean,
          .mean = srvyr::survey_mean(as.numeric(.data$.category)),
          .count_per_dep = NA_integer_, # Not yet implemented here
          .count_per_indep_group = srvyr::survey_total(na.rm = TRUE)
        )
        summary_mean <- srvyr::ungroup(summary_mean)
        summary_mean <- srvyr::as_tibble(summary_mean)

        summary_prop <- srvyr::group_by(out, srvyr::across(tidyselect::all_of(c(indep, ".category"))))
        summary_prop <- srvyr::summarize(summary_prop,
          .count = srvyr::survey_total(na.rm = TRUE),
          .proportion = srvyr::survey_prop(proportion = TRUE)
        )
        summary_prop <- srvyr::ungroup(summary_prop)
        summary_prop <- srvyr::as_tibble(summary_prop)
        # print(get_raw_labels(data = srvyr::as_tibble(data), col_pos = .x))
        summary_prop <- dplyr::mutate(summary_prop,
          .category = factor(x = .data$.category, levels = fct_lvls, labels = fct_lvls),
          .variable_label = get_raw_labels(data = srvyr::as_tibble(data), col_pos = .x)
          # .mean_base = as.integer(.category) * .count
        )

        if (length(indep) > 0) {
          out <- dplyr::left_join(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)))
        } else {
          out <- cbind(summary_prop, summary_mean)
        }
        out$.variable_name <- .x
      } else {
        out <- data.frame(
          .variable_name = .x,
          .variable_label = unname(get_raw_labels(data = data, col_pos = .x)),
          .category = factor(NA),
          .count = NA_integer_,
          .count_per_dep = NA_integer_,
          .count_per_indep_group = NA_integer_,
          .proportion = NA_real_,
          .count_se = NA_real_,
          .proportion_se = NA_real_,
          .mean = NA_real_,
          .mean_se = NA_real_
        )
        out[, indep] <- NA_character_
      }
      as.data.frame(out)
    })

    out <- do.call(rbind, output)
    out <-
      out[
        , # do.call(order, out[c(".variable_name", indep, ".category", ".proportion")]),
        c(
          ".variable_name", ".variable_label",
          ".category",
          ".count", ".count_se",
          ".count_per_dep",
          ".count_per_indep_group",
          ".proportion", ".proportion_se",
          ".mean", ".mean_se",
          # ".mean_base",
          indep
        )
      ]

    # # Add totals when 'indep' is not NULL and 'totals' is TRUE
    # if(length(indep)>0 && isTRUE(totals)) {
    #   # Combine the indep_names and .variable_name into a single factor for aggregating
    #   out$group <- rlang::exec(interaction, !!!as.list(out[, c(".variable_name", indep)]), drop = TRUE)
    #
    #   # Calculate totals using aggregate function
    #   total_counts <- stats::aggregate(.count,  ~ group, data = out, FUN = sum)
    #   total_props <- stats::aggregate(.proportion ~ group, data = out, FUN = sum)
    #   total_means <- stats::aggregate(.mean ~ group, data = out, FUN = sum)
    #
    #   print(total_counts)
    #   print(total_props)
    #   print(total_means)
    #   print(out)
    #   # Combine totals into a data.frame
    #   tryCatch(expr = {
    #   totals <- tibble::tibble(
    #     .category = "Total",
    #     .variable_name = out$.variable_name,
    #     .variable_label = out$.variable_label,
    #     .count = total_counts$.count,
    #     .count_se = NA,
    #     .proportion = total_props$.proportion,
    #     .proportion_se = NA,
    #     .mean = total_means$.mean,
    #     .mean_se = NA
    #   )}, error=browser())
    #
    #   out <- rbind(out, totals)
    # }

    for (indep_var in indep) {
      attr(out[[indep_var]], "label") <- indep_labels[[indep_var]]
    }

    out <- dplyr::arrange(out, dplyr::pick(tidyselect::all_of(c(
      ".variable_name", indep, ".category", ".proportion"
    ))))

    rownames(out) <- NULL
    out
  }
