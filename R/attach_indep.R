attach_indep2 <- function(refined_chapter_overview) {
  if(!is.null(refined_chapter_overview$.variable_role)) {

    indep_df <- refined_chapter_overview
    indep_df <- dplyr::bind_rows(indep_df, data.frame(.variable_name = NA_character_))
    indep_df <- dplyr::ungroup(indep_df)
    indep_df <- vctrs::vec_slice(indep_df, !is.na(indep_df[[".variable_role"]]) & indep_df$.variable_role == "indep")
    colnames(indep_df) <- stringi::stri_replace_all_regex(colnames(indep_df), pattern="^(.variable_.+)$", replacement = "$1_indep")

    dep_df <- vctrs::vec_slice(refined_chapter_overview,
                               !is.na(refined_chapter_overview[[".variable_role"]]) &
                                 refined_chapter_overview[[".variable_role"]] == "dep")
    colnames(dep_df) <- stringi::stri_replace_all_regex(colnames(dep_df), pattern="^(.variable_.+)$", replacement = "$1_dep")

    na_df <- vctrs::vec_slice(refined_chapter_overview, is.na(refined_chapter_overview[[".variable_role"]]))
    colnames(na_df) <- stringi::stri_replace_all_regex(colnames(na_df), pattern="^(.variable_.+)$", replacement = "$1_dep")

    join_variables <- stringi::stri_subset_regex(names(dep_df),
                                                 pattern = "^\\.variable_.+",
                                                 negate = TRUE)
    bi_out <- dplyr::full_join(x = dep_df, # TASK: SIMPLIFY INDEP IN data_overview
                            y = indep_df,
                            by = join_variables,
                            relationship = "many-to-many")

    dplyr::bind_rows(na_df, dep_df, bi_out)

  } else {
    cli::cli_warn("No column {.var .variable_role} found, no bivariates possible.")
    refined_chapter_overview
  }
}
