validate_chapter_overview <- function(chapter_overview, args) {
  if(!all(names(chapter_overview) %in% c("chapter",
                                         ".variable_role", ".variable_selection", ".variable_position",
                                         ".variable_name", ".variable_name_prefix", ".variable_name_suffix",
                                         ".variable_label_prefix", ".variable_label_suffix",
                                         ".variable_type", ".variable_group_id",
                                         ".element_name", "indep_cols_df"))) {
    chapter_overview <-
      rlang::exec(
        refine_chapter_overview,
        !!!args)
  }
  chapter_overview
}
