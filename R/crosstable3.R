crosstable3 <- function(x, ...) UseMethod("crosstable3", x)

crosstable3.data.frame <-
  function(data,
           cols = colnames(data),
           by = NULL,
           showNA = .saros.env$defaults$element_args$showNA,
           totals = .saros.env$defaults$element_args$totals,
           translations = .saros.env$defaults$translations,
           call = rlang::caller_env()) {

    showNA <- rlang::arg_match(showNA, values = .saros.env$defaults$element_args$showNA, error_call = call)

    # by_names <- colnames(data[, by, drop = FALSE])
    by_labels <- get_raw_labels(data, cols_pos = by)
    col_names <- colnames(data[, cols, drop = FALSE])[!(colnames(data[, cols, drop = FALSE]) %in% by)]

    if(length(col_names)==0) return()

    if(length(by) > 0 && rlang::is_true(totals)) {
      for(by_var in by) {
        data_duplicate <- data
        data_duplicate[[by_var]] <- forcats::fct_na_value_to_level(data_duplicate[[by_var]], level = translations$by_total)
        levels(data_duplicate[[by_var]]) <- rep(translations$by_total,
                                                length=length(levels(data_duplicate[[by_var]])))
        data <- dplyr::bind_rows(data, data_duplicate)
        for(i in seq_len(ncol(data))) {
          attr(data[[i]], "label") <- attr(data_duplicate[[i]], "label")
        }
      }
    }

    output <-
      lapply(stats::setNames(col_names, col_names), function(.x) {
        if(!any(by == .x)) {

      out <- data
      names(out)[names(out) == .x] <- ".category"
      col <- out$.category

      if(!rlang::is_character(col) &&
         !is.factor(col) &&
         dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
        out$.category <- factor(col)
        col <- out$.category
      }

      if(showNA == "always" ||
         (showNA == "ifany" && any(is.na(col)))) {
        out$.category <- forcats::fct_na_value_to_level(f = col, level = "NA")
      } else {
        out <- out[!is.na(out$.category), ]
      }


      for(by_var in by) {
        by_col <- out[[by_var]]

        if(showNA == "always" ||
           (showNA == "ifany" && any(is.na(by_col)))) {
          out[[by_var]] <- forcats::fct_na_value_to_level(f = by_col, level = "NA")
        } else {
          out <- out[!is.na(out[[by_var]]), ]
        }
      }



      col <- out$.category

      fct_lvls <-
        if(is.factor(col)) levels(col) else sort(unique(col))

      if(is.character(out$.category)) {
        cli::cli_warn("{.arg {.x}} is {.obj_type_friendly {out$.category}}. Taking its mean is meaningless and results in NAs.",
                      call = call)
      }
      out <- out[rlang::inject(order(!!!out[, c(by, ".category")])), ]
      summary_mean <- out
      summary_mean$.mean <- suppressWarnings(as.numeric(summary_mean$.category))
      summary_mean <- stats::aggregate(x = .mean ~ ., data = summary_mean[, c(by, ".mean"), drop = FALSE], FUN = mean, na.rm = TRUE)

      summary_prop <- out
      summary_prop$.count <- 1L
      summary_prop <- stats::aggregate(x = summary_prop$.count, by = summary_prop[, c(by, ".category"), drop = FALSE], FUN = length, simplify = TRUE)
      names(summary_prop)[names(summary_prop) == "x"] <- ".count" ## FAKE SOLUTION, A VARIABLE CALLED x will break it all
      grouped_count <- stats::aggregate(x = summary_prop$.count, by = summary_prop[, by, drop = FALSE], FUN = sum, na.rm=TRUE, simplify = TRUE)

      summary_prop <- merge(summary_prop, grouped_count, by = by)
      summary_prop$.proportion <- summary_prop$.count / summary_prop$x
      summary_prop$.category <- factor(x = summary_prop$.category,
                                       levels = fct_lvls,
                                       labels = fct_lvls)
      summary_prop$.variable_label <- unname(get_raw_labels(data, cols_pos = .x))
      # summary_prop$.mean_base <- as.integer(summary_prop$.category) * summary_prop$.count
      summary_prop$.count_se <- NA_real_
      summary_prop$.proportion_se <- NA_real_
      summary_prop$.mean_se <- NA_real_

      if(length(by) > 0) {
        out <- dplyr::left_join(summary_prop, summary_mean,
                         by = intersect(names(summary_prop), names(summary_mean)))
      } else {
        out <- cbind(summary_prop, summary_mean)
      }
      # if(length(by) > 0) {
      #   merge(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)), all.x = TRUE)
      # } else {
      #   cbind(summary_prop, summary_mean)
      # }

      out$.variable_name <- .x
      out
        }

    })
    out <- do.call(rbind, output)
    out <-
      out[#do.call(order, out[c(".variable_name", by, ".category", ".proportion")]),
        c(".variable_name", ".variable_label",
          ".category",
          ".count", ".count_se",
          ".proportion", ".proportion_se",
          ".mean", ".mean_se",
          # ".mean_base",
          by)]

    # # Add totals when 'by' is not NULL and 'totals' is TRUE
    # if(length(by)>0 && isTRUE(totals)) {
    #   # Combine the by_names and .variable_name into a single factor for aggregating
    #   out$group <- rlang::exec(interaction, !!!as.list(out[, c(".variable_name", by)]), drop = TRUE)
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

    for(by_var in by) {
      # if(is.null(out[[by_var]])) browser()
      attr(out[[by_var]], "label") <- by_labels[[by_var]]
    }

    out <- dplyr::arrange(out, dplyr::pick(tidyselect::all_of(c(
      ".variable_name", by, ".category", ".proportion"))))

    rownames(out) <- NULL
    out

  }


crosstable3.tbl_svy <-
  function(data,
           cols = colnames(data),
           by = NULL,
           showNA = .saros.env$defaults$element_args$showNA,
           totals = .saros.env$defaults$element_args$totals,
           translations = .saros.env$defaults$translations,
           call = rlang::caller_env()) {

    showNA <- rlang::arg_match(showNA, error_call = call)

    # by_names <- colnames(data[, by, drop = FALSE])
    by_labels <- get_raw_labels(data$variables, cols_pos = by)
    col_names <- colnames(data[, cols, drop = FALSE])[!(colnames(data[, cols, drop = FALSE]) %in% by)]

    # by_names <- colnames(srvyr::select(data, by))
    # col_names <- colnames(srvyr::select(data, cols)) %>% .[!. %in% by_names]

    output <- lapply(stats::setNames(col_names, col_names), function(.x) {

      out <- srvyr::rename(data, .category = tidyselect::all_of(.x))
      col <- srvyr::pull(out, .data$.category)

      if (!rlang::is_character(col) && !is.factor(col) && dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
        out <- srvyr::mutate(out, .category = factor(.data$col))
        col <- srvyr::pull(out, .data$.category)
      }

      if (showNA == "always" || (showNA == "ifany" && any(is.na(col)))) {
        out <- srvyr::mutate(out, .category = forcats::fct_na_value_to_level(f = col, level = "NA"))
      } else {
        out <- srvyr::filter(out, !is.na(.data$.category))
      }
      col <- srvyr::pull(out, .data$.category)

      fct_lvls <- if (is.factor(col)) levels(col) else sort(unique(col))

      # by_vars <- colnames(srvyr::select(data, by))

      for(by_var in by) {

        by_col <- srvyr::pull(out, .data[[by_var]])

        if (showNA == "always" || (showNA == "ifany" && any(is.na(by_col)))) {
          out <- srvyr::mutate(out, srvyr::across(.cols = tidyselect::all_of(by_var),
                                           .fns = ~forcats::fct_na_value_to_level(f = .x, level = "NA")))
        } else {
          out <- srvyr::filter(out, dplyr::if_all(.cols = tidyselect::all_of(by_var),
                                           .fns = ~!is.na(.x)))
        }
      }

      summary_mean <- srvyr::group_by(out, srvyr::across(tidyselect::all_of(by)))
      summary_mean <- srvyr::summarize(summary_mean, .mean = srvyr::survey_mean(as.numeric(.data$.category)))
      summary_mean <- srvyr::ungroup(summary_mean)
      summary_mean <- srvyr::as_tibble(summary_mean)

      summary_prop <- srvyr::group_by(out, srvyr::across(tidyselect::all_of(c(by, ".category"))))
      summary_prop <- srvyr::summarize(summary_prop, .count = srvyr::survey_total(na.rm = TRUE),
                                       .proportion = srvyr::survey_prop(proportion = TRUE))
      summary_prop <- srvyr::ungroup(summary_prop)
      summary_prop <- srvyr::as_tibble(summary_prop)
      # print(get_raw_labels(srvyr::as_tibble(data), cols_pos = .x))
      summary_prop <- dplyr::mutate(summary_prop,
                             .category = factor(x = .data$.category, levels = fct_lvls, labels = fct_lvls),
                             .variable_label = get_raw_labels(srvyr::as_tibble(data), cols_pos = .x)
                             # .mean_base = as.integer(.category) * .count
                             )

      if (length(by) > 0) {
        out <- dplyr::left_join(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)))
      } else {
        out <- cbind(summary_prop, summary_mean)
      }
      out$.variable_name <- .x
      as.data.frame(out)

    })

    out <- do.call(rbind, output)
    out <-
      out[#do.call(order, out[c(".variable_name", by, ".category", ".proportion")]),
        c(".variable_name", ".variable_label",
          ".category",
          ".count", ".count_se",
          ".proportion", ".proportion_se",
          ".mean", ".mean_se",
          # ".mean_base",
          by)]

    # # Add totals when 'by' is not NULL and 'totals' is TRUE
    # if(length(by)>0 && isTRUE(totals)) {
    #   # Combine the by_names and .variable_name into a single factor for aggregating
    #   out$group <- rlang::exec(interaction, !!!as.list(out[, c(".variable_name", by)]), drop = TRUE)
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

    for(by_var in by) {
      # if(is.null(out[[by_var]])) browser()
      attr(out[[by_var]], "label") <- by_labels[[by_var]]
    }

    out <- dplyr::arrange(out, dplyr::pick(tidyselect::all_of(c(
      ".variable_name", by, ".category", ".proportion"))))

    rownames(out) <- NULL
    out

  }
