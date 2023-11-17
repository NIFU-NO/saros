remove_empty_col_for_mesos_group <- function(data,
                                             chapter_overview,
                                             mesos_group = NULL,
                                             mesos_var) {
  if(rlang::is_string(mesos_group)) {
    unique_vars <- unique(as.character(chapter_overview$.variable_name_dep))
    unique_vars <- unique_vars[!is.na(unique_vars)]
    for(var in unique_vars) {
      tmp <- vctrs::vec_slice(data, !is.na(data[[mesos_var]]) &
                                data[[mesos_var]] == mesos_group)
      tmp <- tmp[[var]]
      if(all(is.na(tmp)) || length(tmp)==0) {
        cli::cli_inform("In mesos_group {mesos_group}, removing empty column {var}.")
        chapter_overview <- vctrs::vec_slice(chapter_overview,
                                             as.character(chapter_overview[[".variable_name_dep"]]) != var)
      }
    }
  }

}
