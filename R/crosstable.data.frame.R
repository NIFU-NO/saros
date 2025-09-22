crosstable <- function(data, ...) {
  UseMethod("crosstable")
}


# Helper function: Validate column names
crosstable_validate_columns <- function(data, dep, indep) {
  invalid_deps <- dep[!dep %in% colnames(data)]
  if (length(invalid_deps) > 0) {
    cli::cli_abort("Column{?s} {.var {invalid_deps}} {?doesn't/don't} exist.")
  }
  invalid_indeps <- indep[!indep %in% colnames(data)]
  if (length(invalid_indeps) > 0) {
    cli::cli_abort("Column{?s} {.var {invalid_indeps}} {?doesn't/don't} exist.")
  }
}

# Helper function: Add totals for independent variables
crosstable_add_totals <- function(data, indep, translations) {
  for (indep_var in indep) {
    data_duplicate <- data
    data_duplicate[[indep_var]] <- forcats::fct_na_value_to_level(
      data_duplicate[[indep_var]],
      level = translations$by_total
    )
    levels(data_duplicate[[indep_var]]) <- rep(
      translations$by_total,
      length(levels(data_duplicate[[indep_var]]))
    )

    if (is.ordered(data[[indep_var]])) {
      levels(data_duplicate[[indep_var]]) <- c(
        levels(data[[indep_var]]),
        levels(data_duplicate[[indep_var]])
      )
    }

    data <- dplyr::bind_rows(data, data_duplicate)
    for (i in seq_len(ncol(data))) {
      attr(data[[i]], "label") <- attr(data_duplicate[[i]], "label")
    }
  }
  return(data)
}


# Helper function: Prepare data for processing
crosstable_prepare_data <- function(data, dep_var, indep, showNA) {
  out <- data
  names(out)[names(out) == dep_var] <- ".category"
  col <- out$.category

  if (!is.factor(col) && dplyr::n_distinct(col, na.rm = FALSE) <= 10) {
    out$.category <- factor(col)
    col <- out$.category
  }

  if (showNA == "always" || (showNA == "ifany" && any(is.na(col)))) {
    out$.category <- forcats::fct_na_value_to_level(f = col, level = "NA")
  } else {
    out <- out[!is.na(out$.category), , drop = FALSE]
  }

  for (indep_var in indep) {
    indep_col <- out[[indep_var]]
    if (showNA == "always" || (showNA == "ifany" && any(is.na(indep_col)))) {
      out[[indep_var]] <- forcats::fct_na_value_to_level(
        f = indep_col,
        level = "NA"
      )
    } else {
      out <- vctrs::vec_slice(out, !is.na(out[[indep_var]]))
    }
  }
  return(out)
}

# Helper function: Calculate mean values
crosstable_calculate_means <- function(data, indep) {
  data$.mean <- suppressWarnings(as.numeric(data$.category))
  tryCatch(
    stats::aggregate(
      .mean ~ .,
      data = data[, c(indep, ".mean"), drop = FALSE],
      FUN = mean,
      na.rm = TRUE
    ),
    error = function(e) {
      cols <- c(indep, ".mean")
      data.frame(matrix(NA, ncol = length(cols), dimnames = list(NULL, cols)))
    }
  )
}

# Helper function: Calculate median values
crosstable_calculate_medians <- function(data, indep) {
  data$.median <- suppressWarnings(as.numeric(data$.category))
  tryCatch(
    stats::aggregate(
      .median ~ .,
      data = data[, c(indep, ".median"), drop = FALSE],
      FUN = stats::median,
      na.rm = TRUE
    ),
    error = function(e) {
      cols <- c(indep, ".median")
      data.frame(matrix(NA, ncol = length(cols), dimnames = list(NULL, cols)))
    }
  )
}

# Helper function: Calculate proportions
crosstable_calculate_proportions <- function(
  data,
  dep_var,
  fct_lvls,
  indep,
  showNA
) {
  data$.count <- 1L
  summary_prop <- tryCatch(
    stats::aggregate(
      x = data$.count,
      by = data[, c(indep, ".category"), drop = FALSE],
      FUN = length,
      simplify = TRUE
    ),
    error = function(e) {
      cols <- c(indep, ".category")
      data.frame(matrix(NA, ncol = length(cols), dimnames = list(NULL, cols)))
    }
  )
  names(summary_prop)[ncol(summary_prop)] <- ".count"

  # Summaries per dep variable (e.g. b_1, b_2)
  summary_prop[[".count_per_dep"]] <- sum(summary_prop$.count, na.rm = TRUE)

  # Summaries per indep group (e.g. males, females)
  grouped_count <- tryCatch(
    stats::aggregate(
      x = summary_prop$.count,
      by = summary_prop[, indep, drop = FALSE],
      FUN = sum,
      na.rm = TRUE,
      simplify = TRUE
    ),
    error = function(e) {
      data.frame(matrix(
        NA,
        ncol = max(c(1, length(indep))),
        dimnames = list(NULL, indep)
      ))
    }
  )
  names(grouped_count)[ncol(grouped_count)] <- ".count_per_indep_group"
  summary_prop <- merge(summary_prop, grouped_count, by = indep)
  summary_prop$.proportion <- summary_prop$.count /
    summary_prop[[".count_per_indep_group"]]

  summary_prop$.category <- factor(
    x = summary_prop$.category,
    levels = fct_lvls,
    labels = fct_lvls,
    exclude = character()
  )
  summary_prop$.count_se <- NA_real_
  summary_prop$.count_per_dep_se <- NA_real_
  summary_prop$.count_per_indep_group_se <- NA_real_
  summary_prop$.proportion_se <- NA_real_
  summary_prop$.mean_se <- NA_real_
  summary_prop
}

# Helper function: Merge summaries
crosstable_merge_summaries <- function(
  summary_prop,
  summary_mean,
  summary_median,
  indep
) {
  if (length(indep) > 0) {
    common_cols <- intersect(colnames(summary_prop), colnames(summary_mean))
    if (length(common_cols) == 0) {
      cli::cli_abort("Internal error in `crosstable_merge_summaries`")
    }
    out <- dplyr::left_join(summary_prop, summary_mean, by = common_cols)
    common_cols <- intersect(colnames(out), colnames(summary_median))
    out <- dplyr::left_join(out, summary_median, by = common_cols)
  } else {
    out <- cbind(
      summary_prop,
      summary_mean,
      summary_median[, ".median", drop = FALSE]
    )
  }
  out
}

# Helper function: Finalize variable output
crosstable_finalize_variable_output <- function(out, dep_var, data) {
  out$.variable_name <- dep_var
  out$.variable_position <- match(dep_var, colnames(data))
  out$.variable_label <- unname(get_raw_labels(data = data, col_pos = dep_var))

  out
}


# Helper function: Process a single dependent variable
crosstable_process_dep <- function(data, dep_var, indep, showNA, call) {
  out <- crosstable_prepare_data(data, dep_var, indep, showNA)
  if (nrow(out) == 0) {
    return(crosstable_empty_output(dep_var, indep, data = data))
  }

  fct_lvls <- if (is.factor(out[[".category"]])) {
    levels(out[[".category"]])
  } else {
    sort(unique(out[[".category"]]))
  }

  #out <- out[rlang::inject(order(!!!out[, c(indep, ".category"), drop = FALSE])), , drop = FALSE]
  summary_mean <- crosstable_calculate_means(data = out, indep = indep)
  summary_prop <- crosstable_calculate_proportions(
    data = out,
    dep_var = dep_var,
    fct_lvls = fct_lvls,
    indep = indep,
    showNA = showNA
  )
  summary_median <- crosstable_calculate_medians(
    data = out,
    indep = indep
  )

  merged_output <- crosstable_merge_summaries(
    summary_prop = summary_prop,
    summary_mean = summary_mean,
    summary_median = summary_median,
    indep = indep
  )
  crosstable_finalize_variable_output(
    out = merged_output,
    dep_var = dep_var,
    data = data
  )
}


# Helper function: Generate crosstables for dependent variables
crosstable_loop_over_deps <- function(data, dep_cols, indep, showNA, call) {
  lapply(stats::setNames(dep_cols, dep_cols), function(dep_var) {
    crosstable_process_dep(data, dep_var, indep, showNA, call)
  })
}

# Helper function: Return empty output
crosstable_empty_output <- function(dep_var, indep, data) {
  out <- data.frame(
    .variable_name = dep_var,
    .variable_position = match(dep_var, colnames(data)),
    .variable_label = unname(get_raw_labels(data = data, col_pos = dep_var)),
    .category = factor(NA),
    .count = NA_integer_,
    .count_se = NA_real_,
    .count_per_dep = NA_integer_,
    .count_per_dep_se = NA_real_,
    .count_per_indep_group = NA_integer_,
    .count_per_indep_group_se = NA_real_,
    .proportion = NA_real_,
    .proportion_se = NA_real_,
    .mean = NA_real_,
    .mean_se = NA_real_,
    .median = NA_real_
  )
  out[, indep] <- NA_character_
  return(out)
}

# Helper function: Finalize entire output
crosstable_finalize_output <- function(output, indep, indep_labels, dep_cols) {
  result <- do.call(rbind, output)
  for (indep_var in indep) {
    attr(result[[indep_var]], "label") <- indep_labels[[indep_var]]
  }

  result <-
    result[,
      c(
        ".variable_name",
        ".variable_position",
        ".variable_label",
        ".category",
        ".count",
        ".count_se",
        ".count_per_dep",
        ".count_per_dep_se",
        ".count_per_indep_group",
        ".count_per_indep_group_se",
        ".proportion",
        ".proportion_se",
        ".mean",
        ".mean_se",
        ".median",
        indep
      ),
      drop = FALSE
    ]
  result <- dplyr::arrange(
    result,
    dplyr::pick(tidyselect::all_of(c(".variable_name", indep, ".category")))
  )
  rownames(result) <- NULL
  return(result)
}


#' @export
crosstable.data.frame <- function(
  data,
  dep = colnames(data),
  indep = NULL,
  showNA = eval(formals(makeme)$showNA),
  totals = eval(formals(makeme)$totals),
  translations = eval(formals(makeme)$translations),
  ...,
  call = rlang::caller_env()
) {
  # Validate inputs
  showNA <- rlang::arg_match(
    showNA,
    values = eval(formals(makeme)$showNA),
    error_call = call
  )

  crosstable_validate_columns(data, dep, indep)

  # Prepare dependent and independent variable labels
  indep_labels <- get_raw_labels(data = data, col_pos = indep)
  dep_cols <- setdiff(dep, indep)
  if (length(dep_cols) == 0) {
    return()
  }

  # Handle totals for independent variables
  if (length(indep) > 0 && isTRUE(totals)) {
    data <- crosstable_add_totals(data, indep, translations)
  }

  # Generate crosstables for each dependent variable
  output <- crosstable_loop_over_deps(data, dep_cols, indep, showNA, call)

  # Combine and arrange results
  result <- crosstable_finalize_output(output, indep, indep_labels, dep_cols)
  return(result)
}


#' @export
crosstable.tbl_df <- crosstable.data.frame
