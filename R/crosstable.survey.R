
# #' @export
# crosstable.tbl_svy <-
#   function(data,
#            dep = colnames(data),
#            indep = NULL,
#            showNA = eval(formals(makeme)$showNA),
#            totals = eval(formals(makeme)$totals),
#            translations = eval(formals(makeme)$translations),
#            ...,
#            call = rlang::caller_env()) {
#     if (inherits(data, "tbl_svy") &&
#       !requireNamespace("srvyr", quietly = TRUE)) {
#       cli::cli_abort("Needs {.pkg srvyr} to use tbl_svy objects: {.run install.packages('srvyr')}.")
#     }

#     showNA <- rlang::arg_match(showNA, values = eval(formals(makeme)$showNA), error_call = call)

#     # indep_names <- colnames(data[, indep, drop = FALSE])
#     indep_labels <- get_raw_labels(data = data$variables, col_pos = indep)
#     dep_cols <- setdiff(dep, indep)
#     crosstable_validate_columns(data = srvyr::as_tibble(data), dep = dep_cols, indep = indep)

#     output <- lapply(stats::setNames(dep_cols, dep_cols), function(.x) {
#       out <- srvyr::rename(data, .category = tidyselect::all_of(.x))
#       col <- srvyr::pull(out, .data$.category)

#       if (!is.character(col) && !is.factor(col) && dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
#         out <- srvyr::mutate(out, .category = factor(.data$col))
#         col <- srvyr::pull(out, .data$.category)
#       }

#       if (showNA == "always" || (showNA == "ifany" && any(is.na(col)))) {
#         out <- srvyr::mutate(out, .category = forcats::fct_na_value_to_level(f = col, level = "NA"))
#       } else {
#         out <- srvyr::filter(out, !is.na(.data$.category))
#       }

#       if (nrow(out) > 0) {
#         col <- srvyr::pull(out, .data$.category)

#         fct_lvls <- if (is.factor(col)) levels(col) else sort(unique(col))

#         # indep_vars <- colnames(srvyr::select(data, indep))

#         for (indep_var in indep) {
#           indep_col <- srvyr::pull(out, .data[[indep_var]])

#           if (showNA == "always" || (showNA == "ifany" && any(is.na(indep_col)))) {
#             out <- srvyr::mutate(out, srvyr::across(
#               .cols = tidyselect::all_of(indep_var),
#               .fns = ~ forcats::fct_na_value_to_level(f = .x, level = "NA")
#             ))
#           } else {
#             out <- srvyr::filter(out, dplyr::if_all(
#               .cols = tidyselect::all_of(indep_var),
#               .fns = ~ !is.na(.x)
#             ))
#           }
#         }

#         summary_mean <- srvyr::group_by(out, srvyr::across(tidyselect::all_of(indep)))
#         summary_mean <- srvyr::summarize(summary_mean,
#           .mean = srvyr::survey_mean(as.numeric(.data$.category)),
#           .count_per_dep = NA_integer_, # Not yet implemented here
#           .count_per_indep_group = srvyr::survey_total(na.rm = TRUE)
#         )
#         summary_mean <- srvyr::ungroup(summary_mean)
#         summary_mean <- srvyr::as_tibble(summary_mean)

#         summary_prop <- srvyr::group_by(out, srvyr::across(tidyselect::all_of(c(indep, ".category"))))
#         summary_prop <- srvyr::summarize(summary_prop,
#           .count = srvyr::survey_total(na.rm = TRUE),
#           .proportion = srvyr::survey_prop(proportion = TRUE)
#         )
#         summary_prop <- srvyr::ungroup(summary_prop)
#         summary_prop <- srvyr::as_tibble(summary_prop)
#         summary_prop <- dplyr::mutate(summary_prop,
#           .category = factor(x = .data$.category, levels = fct_lvls, labels = fct_lvls),
#         )

#         if (length(indep) > 0) {
#           out <- crosstable_merge_summaries(summary_prop, summary_mean, indep)
#         } else {
#           out <- cbind(summary_prop, summary_mean)
#         }
#         out <- crosstable_finalize_variable_output(out = out, dep_var = .x, data = srvyr::as_tibble(data))

#       } else {
#         out <- crosstable_empty_output(dep_var=.x, indep=indep, data=data)

#       }
#       out
#     })



#     # # Add totals when 'indep' is not NULL and 'totals' is TRUE. Not yet working.
#     # if(length(indep)>0 && isTRUE(totals)) {
#     #   # Combine the indep_names and .variable_name into a single factor for aggregating
#     #   out$group <- rlang::exec(interaction, !!!as.list(out[, c(".variable_name", indep)]), drop = TRUE)
#     #
#     #   # Calculate totals using aggregate function
#     #   total_counts <- stats::aggregate(.count,  ~ group, data = out, FUN = sum)
#     #   total_props <- stats::aggregate(.proportion ~ group, data = out, FUN = sum)
#     #   total_means <- stats::aggregate(.mean ~ group, data = out, FUN = sum)
#     #
#     #   print(total_counts)
#     #   print(total_props)
#     #   print(total_means)
#     #   print(out)
#     #   # Combine totals into a data.frame
#     #   tryCatch(expr = {
#     #   totals <- tibble::tibble(
#     #     .category = "Total",
#     #     .variable_name = out$.variable_name,
#     #     .variable_label = out$.variable_label,
#     #     .count = total_counts$.count,
#     #     .count_se = NA,
#     #     .proportion = total_props$.proportion,
#     #     .proportion_se = NA,
#     #     .mean = total_means$.mean,
#     #     .mean_se = NA
#     #   )}, error=browser())
#     #
#     #   out <- rbind(out, totals)
#     # }
#   result <- crosstable_finalize_output(output, indep, indep_labels, dep_cols)
#   return(result)
#   }


##################################

# Helper function: Prepare data for processing for tbl_svy
crosstable_prepare_data_svy <- function(data, dep_var, indep, showNA) {
  out <- data
  out <- srvyr::rename(data, .category = tidyselect::all_of(dep_var))
  col <- srvyr::pull(out, .data$.category)

  if (!is.factor(col) && dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
    out <- srvyr::mutate(out, .category = factor(.data$.category))
    col <- srvyr::pull(out, .data$.category)
  }

  if (showNA == "always" || (showNA == "ifany" && any(is.na(col)))) {
    out <- srvyr::mutate(out, .category = forcats::fct_na_value_to_level(f = col, level = "NA"))
  } else {
    out <- srvyr::filter(out, !is.na(.data$.category))
  }

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
  return(out)
}

# Helper function: Calculate mean values for tbl_svy
crosstable_calculate_means_svy <- function(data, indep) {
  summary_mean <- srvyr::group_by(data, srvyr::across(tidyselect::all_of(indep)))
  summary_mean <- srvyr::summarize(summary_mean,
    .mean = srvyr::survey_mean(suppressWarnings(as.numeric(.data$.category)), na.rm = TRUE),
    .count_per_dep = NA_integer_,  # Placeholder
    .count_per_indep_group = srvyr::survey_total(na.rm = TRUE)
  )
  summary_mean <- srvyr::ungroup(summary_mean)
  return(srvyr::as_tibble(summary_mean))
}

# Helper function: Calculate proportions for tbl_svy
crosstable_calculate_proportions_svy <- function(data, dep_var, fct_lvls, indep) {
  summary_prop <- srvyr::group_by(data, srvyr::across(tidyselect::all_of(c(indep, ".category"))))

  # Calculate total counts and proportions
  summary_prop <- srvyr::summarize(summary_prop,
    .count = srvyr::survey_total(na.rm = TRUE),
    .proportion = srvyr::survey_prop(proportion = TRUE)
  )
  summary_prop <- srvyr::ungroup(summary_prop)

  # Summaries per dep variable (e.g. b_1, b_2)
  summary_prop <- 
    cbind(summary_prop, 
          srvyr::summarize(data,
                   .count_per_dep = srvyr::survey_total(na.rm = TRUE)
  ))
  
  # Calculate .count_per_indep_group
  grouped_count <- srvyr::group_by(data, srvyr::across(tidyselect::all_of(indep)))
  grouped_count <- srvyr::summarize(grouped_count,
    .count_per_indep_group = srvyr::survey_total(na.rm = TRUE)
  )
  grouped_count <- srvyr::ungroup(grouped_count)

  # Merge .count_per_indep_group into summary_prop
#  summary_prop <- dplyr::left_join(summary_prop, grouped_count, by = indep)
  summary_prop <- merge(summary_prop, grouped_count, by = indep)
  ###
  summary_prop <- dplyr::mutate(summary_prop,
    .category = factor(x = .data$.category, levels = fct_lvls, labels = fct_lvls, exclude = character())
  )

  return(summary_prop)
}


# Helper function: Process a single dependent variable for tbl_svy
crosstable_process_dep_svy <- function(data, dep_var, indep, showNA, translations, call) {

  out <- crosstable_prepare_data_svy(data, dep_var, indep, showNA)
  if (nrow(out) == 0) return(crosstable_empty_output(dep_var, indep, data = srvyr::as_tibble(data)))
  col <- srvyr::pull(out, .data$.category)
  fct_lvls <- if (is.factor(col)) levels(col) else sort(unique(col))


  summary_mean <- crosstable_calculate_means_svy(out, indep)
  summary_prop <- crosstable_calculate_proportions_svy(out, dep_var, fct_lvls, indep)

  merged_output <- crosstable_merge_summaries(summary_prop, summary_mean, indep)
  crosstable_finalize_variable_output(merged_output, dep_var, srvyr::as_tibble(data))
}



#' @export
crosstable.tbl_svy <- function(data,
                               dep = colnames(data),
                               indep = NULL,
                               showNA = eval(formals(makeme)$showNA),
                               totals = eval(formals(makeme)$totals),
                               translations = eval(formals(makeme)$translations),
                               ...,
                               call = rlang::caller_env()) {
  # Validate inputs
  if (inherits(data, "tbl_svy") &&
      !requireNamespace("srvyr", quietly = TRUE)) {
    cli::cli_abort("Needs {.pkg srvyr} to use tbl_svy objects: {.run install.packages('srvyr')}.")
  }

  showNA <- rlang::arg_match(showNA, values = eval(formals(makeme)$showNA), error_call = call)
  
  crosstable_validate_columns(data = srvyr::as_tibble(data), dep = dep, indep = indep)

  # Prepare dependent and independent variable labels
  indep_labels <- get_raw_labels(data = srvyr::as_tibble(data), col_pos = indep)
  dep_cols <- setdiff(dep, indep)
  if(length(dep_cols) == 0) return()


  # Generate crosstables for each dependent variable
  output <- lapply(stats::setNames(dep_cols, dep_cols), function(dep_var) {
    crosstable_process_dep_svy(data, dep_var, indep, showNA, translations, call)
  })

  # Finalize and arrange output
  result <- crosstable_finalize_output(output, indep, indep_labels, dep_cols)
  return(result)
}