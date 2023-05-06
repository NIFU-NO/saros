crosstable3 <- function(x, ...) UseMethod("crosstable2", x)

crosstable3.data.frame <-
  function(data,
           y_vars = colnames(data),
           x_vars = NULL,
           cols = colnames(data),
           by = NULL,
           showNA = c("ifany", "always", "never"),
           call = rlang::caller_env()) {

    showNA <- rlang::arg_match(showNA, error_call = call)

    by_names <- colnames(data[, by, drop = FALSE])
    col_names <- colnames(data[, cols, drop = FALSE])[!(colnames(data[, cols, drop = FALSE]) %in% by_names)]

    output <- lapply(stats::setNames(col_names, col_names), function(.x) {

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

      for(by_var in by_names) {
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
      summary_mean <- out
      summary_mean$.mean <- suppressWarnings(as.numeric(summary_mean$.category))
      summary_mean <- stats::aggregate(x = .mean ~ ., data = summary_mean[, c(by_names, ".mean"), drop = FALSE], FUN = mean, na.rm = TRUE)

      summary_prop <- out
      summary_prop <- stats::aggregate(x = .count ~ ., data = summary_prop[, c(by_names, ".category"), drop = FALSE], FUN = length)
      summary_prop <- summary_prop[order(summary_prop$.category), ]
      summary_prop$.proportion <- summary_prop$.count / sum(summary_prop$.count, na.rm = TRUE)
      summary_prop$.category <- factor(x = summary_prop$.category,
                                       levels = fct_lvls,
                                       labels = fct_lvls)
      summary_prop$.variable_label <- get_raw_labels(data, cols_pos = .x)
      summary_prop$.mean_base <- as.integer(summary_prop$.category) * summary_prop$.count
      summary_prop$.count_se <- NA_real_
      summary_prop$.proportion_se <- NA_real_
      summary_prop$.mean_se <- NA_real_

      if(length(by_names) > 0) {
        merge(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)), all.x = TRUE)
      } else {
        cbind(summary_prop, summary_mean)
      }

    })

    output_df <- do.call(rbind, output)
    output_df <- stats::setNames(output_df, c(".variable_name", ".variable_label",
                                       ".category",
                                       ".count", ".count_se",
                                       ".proportion", ".proportion_se",
                                       ".mean", ".mean_se",
                                       ".mean_base"))

    return(output_df)
  }


crosstable3.tbl_svy <-
  function(data,
           y_vars = colnames(data),
           x_vars = NULL,
           cols,
           by = NULL,
           showNA = c("ifany", "always", "never"),
           call = rlang::caller_env()) {

    showNA <- rlang::arg_match(showNA, error_call = call)

    by_names <- colnames(srvyr::select(data, by))
    col_names <- colnames(srvyr::select(data, cols)) %>% .[!. %in% by_names]

    output <- lapply(stats::setNames(col_names, col_names), function(.x) {

      out <- srvyr::rename(data, .category = .x)
      col <- srvyr::pull(out, .category)

      if (!rlang::is_character(col) && !is.factor(col) && dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
        out <- srvyr::mutate(out, .category = factor(col))
        col <- srvyr::pull(out, .category)
      }

      if (showNA == "always" || (showNA == "ifany" && any(is.na(col)))) {
        out <- srvyr::mutate(out, .category = forcats::fct_na_value_to_level(f = col, level = "NA"))
      } else {
        out <- srvyr::filter(out, !is.na(.category))
      }
      col <- srvyr::pull(out, .category)

      fct_lvls <- if (is.factor(col)) levels(col) else sort(unique(col))

      by_vars <- colnames(srvyr::select(data, by))

      for (by_var in by_vars) {

        by_col <- srvyr::pull(out, .data[[by_var]])

        if (showNA == "always" || (showNA == "ifany" && any(is.na(by_col)))) {
          out <- srvyr::mutate(out, across(.cols = by_var, ~forcats::fct_na_value_to_level(f = .x, level = "NA")))
        } else {
          out <- srvyr::filter(out, if_all(.cols = by_var, .fns = ~!is.na(.x)))
        }
      }

      summary_mean <- srvyr::group_by(out, across(all_of(by_vars)))
      summary_mean <- srvyr::summarize(summary_mean, .mean = survey_mean(as.numeric(.category)))
      summary_mean <- srvyr::ungroup(summary_mean)
      summary_mean <- srvyr::as_tibble(summary_mean)

      summary_prop <- srvyr::group_by(out, across(all_of(c(by_vars, ".category"))))
      summary_prop <- srvyr::summarize(summary_prop, .count = survey_total(na.rm = TRUE),
                                       .proportion = survey_prop(proportion = TRUE))
      summary_prop <- srvyr::ungroup(summary_prop)
      summary_prop <- srvyr::as_tibble(summary_prop)
      summary_prop <- mutate(summary_prop,
                             .category = factor(x = .category, levels = fct_lvls, labels = fct_lvls),
                             .variable_label = get_raw_labels(srvyr::as_tibble(data), cols_pos = .x),
                             .mean_base = as.integer(.category) * .count)

      if (length(by_vars) > 0) {
        left_join(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)))
      } else {
        cbind(summary_prop, summary_mean)
      }

    })

    output <- do.call(rbind, output)
    output <- stats::setNames(output,
                       c(".variable_name", ".variable_label", ".category",
                         ".count", ".count_se",
                         ".proportion", ".proportion_se",
                         ".mean", ".mean_se", ".mean_base"))
    output
  }
