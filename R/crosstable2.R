
crosstable2 <- function(x, ...) UseMethod("crosstable2", x)

crosstable2.data.frame <-
  function(data,
			y_vars = colnames(data),
			x_vars = NULL,
           cols = tidyselect::everything(),
           by = NULL,
           showNA = c("ifany", "always", "never"),
           call = rlang::caller_env()) {

    showNA <- rlang::arg_match(showNA, error_call = call)

    by_names <- colnames(dplyr::select(data, {{by}}))
    col_names <- colnames(dplyr::select(data, {{cols}})) %>% .[!. %in% by_names]

    purrr::map(stats::setNames(col_names, col_names), .f = ~{

        out <-
          data %>%
          dplyr::rename(.category = tidyselect::all_of(.x))
        col <- dplyr::pull(out, .data$.category)


        if(!rlang::is_character(col) &&
           !is.factor(col) &&
           dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
          out <- dplyr::mutate(out, .category = factor(.env$col))
          col <- dplyr::pull(out, .data$.category)
        }

        if(showNA == "always" ||
           (showNA == "ifany" && any(is.na(col)))) {

          out <-
            dplyr::mutate(out,
                          .category = forcats::fct_na_value_to_level(f = col, level = "NA"))

        } else {
          out <- vctrs::vec_slice(out, !is.na(out$.category))
        }

        by_vars <-
          out %>%
          dplyr::select({{by}}) %>%
          names()

        for(by_var in by_vars) {

          by_col <- dplyr::pull(out, .data[[by_var]])

          if(showNA == "always" ||
             (showNA == "ifany" && any(is.na(by_col)))) {

            out <-
              dplyr::mutate(out, dplyr::across(.cols = tidyselect::all_of(by_var),
                                               ~forcats::fct_na_value_to_level(f = .x, level = "NA")))

          } else {
            out <-
              dplyr::filter(out, dplyr::if_all(.cols = tidyselect::all_of(by_var), .fns = ~!is.na(.x)))
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
        summary_mean <- dplyr::group_by(summary_mean, dplyr::pick(tidyselect::all_of(by_vars)))
        summary_mean <- dplyr::summarize(summary_mean, .mean = mean(.mean, na.rm=TRUE))
        summary_mean <- dplyr::ungroup(summary_mean)


        summary_prop <- out
        summary_prop <- dplyr::group_by(summary_prop, dplyr::pick(tidyselect::all_of(c(by_vars, ".category"))))
        summary_prop <- dplyr::summarize(summary_prop, .count = dplyr::n())
        summary_prop <- dplyr::group_by(summary_prop, dplyr::pick(tidyselect::all_of(by_vars)))
        summary_prop <- dplyr::mutate(summary_prop, .proportion = .data$.count/sum(.data$.count, na.rm=TRUE))
        summary_prop <- dplyr::ungroup(summary_prop)
        summary_prop$.category <- factor(x = summary_prop$.category,
                               levels = fct_lvls,
                               labels = fct_lvls)
        summary_prop$.variable_label <- get_raw_labels(data, cols_pos = .x)
        summary_prop$.mean_base <- as.integer(summary_prop$.category) * summary_prop$.count
        summary_prop$.count_se <- NA_real_
        summary_prop$.proportion_se <- NA_real_
        summary_prop$.mean_se <- NA_real_

        if(length(by_vars) > 0) {
          dplyr::left_join(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)))
        } else cbind(summary_prop, summary_mean)

      }) %>%
      dplyr::bind_rows(.id = ".variable_name") %>%
      dplyr::relocate(tidyselect::all_of(c(".variable_name", ".variable_label",
                                           ".category",
                                           ".count", ".count_se",
                                           ".proportion", ".proportion_se",
                                           ".mean", ".mean_se",
                                           ".mean_base")))
      # dplyr::relocate(".count_se", .after = ".count") %>%
      # dplyr::relocate(".proportion_se", .after = ".proportion") %>%
      # dplyr::relocate(".mean", .after = ".proportion_se") %>%
      # dplyr::relocate(".mean_se", .after = ".mean") %>%
      # dplyr::relocate(".variable_label", .after = ".mean_se")

  }

################################################################################
crosstable2.tbl_svy <-
  function(data,
			y_vars = colnames(data),
			x_vars = NULL,
           cols,
           by = NULL,
           showNA = c("ifany", "always", "never"),
           call = rlang::caller_env()) {


    srvyr::select(data, {{cols}}) %>%
      colnames() %>%
      stats::setNames(nm = .) %>%
      purrr::map(.f = ~{

        out <-
          data %>%
          dplyr::rename(.category = tidyselect::all_of(.x))

        col <- srvyr::pull(out, .data$.category)

        if(!rlang::is_character(col) &&
           !is.factor(col) &&
           dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
          out <- dplyr::mutate(out, "{.x}" := factor(.env$col))
          col <- srvyr::pull(out, .data$.category)
        }

        show_na <- showNA == "always" || (showNA == "ifany" && any(is.na(col)))
        if(show_na) {

          out <-
            srvyr::mutate(out,
                          .category = forcats::fct_na_value_to_level(f = col, level = "NA"))

        } else {
          out <- srvyr::filter(out, !is.na(.data$.category))
        }
        col <- srvyr::pull(out, .data$.category)

        fct_lvls <-
          if(is.factor(col)) levels(col) else sort(unique(col))

        by_vars <-
          srvyr::select(data, {{by}}) %>%
          colnames()

        for(by_var in by_vars) {

          by_col <- srvyr::pull(out, .data[[by_var]])

          if(showNA == "always" ||
             (showNA == "ifany" && any(is.na(by_col)))) {

            # out <-
            #   srvyr::mutate(out, srvyr::across(.cols = tidyselect::all_of(by_var),
            #                                    ~forcats::fct_na_value_to_level(f = .x, level = "NA")))

          } else {
            out <-
              out %>%
              srvyr::filter(dplyr::if_all(.cols = tidyselect::all_of(by_var), .fns = ~!is.na(.x)))
          }
        }

        summary_mean <-
          out %>%
          srvyr::group_by(dplyr::across(tidyselect::all_of(by_vars))) %>%
          srvyr::summarize(.mean = srvyr::survey_mean(as.numeric(.data$.category))) %>%
          srvyr::ungroup() %>%
          srvyr::as_tibble()

        summary_prop <-
          out %>%
          srvyr::group_by(dplyr::across(tidyselect::all_of(c(by_vars, ".category")))) %>%
          srvyr::summarize(.count = srvyr::survey_total(na.rm = TRUE), #na.rm = !show_na
                           .proportion = srvyr::survey_prop(proportion = TRUE)) %>%
          srvyr::ungroup() %>%
          srvyr::as_tibble() %>%
          dplyr::mutate(
            .category = factor(x = .data$.category,
                               levels = .env$fct_lvls,
                               labels = .env$fct_lvls),
            .variable_label = get_raw_labels(srvyr::as_tibble(.env$data), cols_pos = .env$.x),
            .mean_base = as.integer(.data$.category) * .data$.count)

        if(length(by_vars) > 0) {
          dplyr::left_join(summary_prop, summary_mean, by = intersect(names(summary_prop), names(summary_mean)))
        } else cbind(summary_prop, summary_mean)

      }) %>%
      dplyr::bind_rows(.id = ".variable_name") %>%
      dplyr::relocate(tidyselect::all_of(c(".variable_name", ".variable_label",
                                           ".category",
                                           ".count", ".count_se",
                                           ".proportion", ".proportion_se",
                                           ".mean", ".mean_se",
                                           ".mean_base")))
  }

