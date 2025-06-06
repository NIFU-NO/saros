# Helper function: Evaluate variable selection
evaluate_variable_selection <- function(data, dep, indep) {
    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data)
    list(dep_pos = dep_pos, indep_pos = indep_pos)
}

# Helper function: Validate and initialize arguments
initialize_arguments <- function(data, dep_pos, indep_pos, args) {
    args$data <- data
    args$dep <- names(dep_pos)
    args$indep <- names(indep_pos)
    args$showNA <- args$showNA[1]
    args$data_label <- args$data_label[1]
    args$type <- eval(args$type)[1]
    args
}

# Helper function: Process crowd settings
process_crowd_settings <- function(args) {
    args$crowd <- c(
        args$hide_for_all_crowds_if_hidden_for_crowd[args$hide_for_all_crowds_if_hidden_for_crowd %in% args$crowd],
        args$crowd[!args$crowd %in% args$hide_for_all_crowds_if_hidden_for_crowd[args$hide_for_all_crowds_if_hidden_for_crowd %in% args$crowd]]
    )
    args
}

# Helper function: Handle kept and omitted columns for crowds
handle_crowd_columns <- function(args, kept_cols_list, omitted_cols_list, kept_indep_cats_list) {
    for (crwd in names(kept_cols_list)) {
        kept_cols_tmp <- keep_cols(
            data = args$data,
            dep = args$dep,
            indep = args$indep,
            crowd = crwd,
            mesos_var = args$mesos_var,
            mesos_group = args$mesos_group,
            hide_for_crowd_if_all_na = args$hide_for_crowd_if_all_na,
            hide_for_crowd_if_valid_n_below = args$hide_for_crowd_if_valid_n_below,
            hide_for_crowd_if_category_k_below = args$hide_for_crowd_if_category_k_below,
            hide_for_crowd_if_category_n_below = args$hide_for_crowd_if_category_n_below,
            hide_for_crowd_if_cell_n_below = args$hide_for_crowd_if_cell_n_below
        )
        omitted_cols_list[[crwd]] <- kept_cols_tmp[["omitted_vars"]]
        kept_indep_cats_list[[crwd]] <- keep_indep_cats(
            data = kept_cols_tmp[["data"]],
            indep = args$indep
        )
    }
    list(omitted_cols_list = omitted_cols_list, kept_indep_cats_list = kept_indep_cats_list)
}

# Helper function: Summarize data based on type
summarize_data_by_type <- function(args, subset_data, dep_crwd, indep_crwd, ...) {
    variable_type_dep <- lapply(args$dep, function(v) class(subset_data[[v]])) |> unlist()
    if (all(variable_type_dep %in% c("integer", "numeric"))) {
        args$data_summary <- rlang::exec(summarize_int_cat_data, !!!args)
    } else if (all(variable_type_dep %in% c("factor", "ordered"))) {
        args$data_summary <- summarize_cat_cat_data(
            data = subset_data,
            dep = dep_crwd,
            indep = indep_crwd,
            ...,
            label_separator = args$label_separator,
            showNA = args$showNA,
            totals = args$totals,
            sort_by = args$sort_by,
            descend = args$descend,
            data_label = args$data_label,
            digits = args$digits,
            add_n_to_label = args$add_n_to_label,
            add_n_to_category = args$add_n_to_category,
            hide_label_if_prop_below = args$hide_label_if_prop_below,
            data_label_decimal_symbol = args$data_label_decimal_symbol,
            categories_treated_as_na = args$categories_treated_as_na,
            labels_always_at_bottom = args$labels_always_at_bottom,
            labels_always_at_top = args$labels_always_at_top,
            translations = args$translations
        )
    }
    args
}

# Helper function: Filter and process data for each crowd
process_crowd_data <- function(data, args, crwd, omitted_cols_list, kept_indep_cats_list) {
    omitted_vars_crwd <- omitted_cols_list[
        c(crwd, args$hide_for_all_crowds_if_hidden_for_crowd)
    ] |>
        lapply(FUN = function(x) if ("omitted_vars" %in% names(x)) x["omitted_vars"]) |>
        unlist() |>
        unique()

    dep_crwd <- args$dep[!args$dep %in% omitted_vars_crwd]
    if (length(dep_crwd) == 0) {
        return(NULL)
    }

    indep_crwd <- args$indep
    if (length(indep_crwd) == 0) indep_crwd <- NULL

    subset_data <- dplyr::filter(
        data[, !colnames(data) %in% omitted_vars_crwd, drop = FALSE],
        makeme_keep_rows(
            data = data,
            crwd = crwd,
            mesos_var = args$mesos_var,
            mesos_group = args$mesos_group
        )
    )

    if (isTRUE(args$hide_indep_cat_for_all_crowds_if_hidden_for_crowd)) {
        for (x in indep_crwd) {
            subset_data <- dplyr::filter(
                subset_data,
                as.character(subset_data[[x]]) %in% kept_indep_cats_list[[crwd]][[x]]
            )
        }
    }

    if (nrow(subset_data) == 0) {
        indep_msg <- if (is.character(args$indep)) paste0("indep=", cli::ansi_collapse(args$indep))
        cli::cli_warn(c("No data left to make you {.arg {args$type}} with dep={.arg {args$dep}}, {.arg {indep_msg}}, crowd={.arg {crwd}}.",
            i = "Skipping."
        ))
        return(NULL)
    }

    list(subset_data = subset_data, dep_crwd = dep_crwd, indep_crwd = indep_crwd)
}

# Helper function: Generate output for a crowd
generate_crowd_output <- function(args, subset_data, dep_crwd, indep_crwd) {
    args <- summarize_data_by_type(args, subset_data, dep_crwd, indep_crwd)
    args$main_question <- as.character(unique(args$data_summary[[".variable_label_prefix"]]))

    if (!args$type %in% c("sigtest_table_html")) {
        args$data_summary <- post_process_makeme_data(
            data = args$data_summary,
            indep = indep_crwd,
            showNA = args$showNA,
            colour_2nd_binary_cat = if (grepl(x = args$type, pattern = "docx")) args$colour_2nd_binary_cat
        )
    }

    args_crwd <- args
    args_crwd$dep <- dep_crwd
    args_crwd$indep <- indep_crwd

    suppressPackageStartupMessages(
        rlang::exec(
            make_content,
            type = args_crwd$type,
            !!!args_crwd[!names(args_crwd) %in% c("type")]
        )
    )
}

# Helper function: Rename crowd outputs
rename_crowd_outputs <- function(out, translations) {
    for (crwd in names(out)) {
        if (rlang::is_string(translations[[paste0("crowd_", crwd)]])) {
            names(out)[names(out) == crwd] <- translations[[paste0("crowd_", crwd)]]
        }
    }
    out
}
