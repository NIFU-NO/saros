check_duplicates_in_chapter_overview <-
  function(data, chapter_overview) {

    chapter_overview %>%
      dplyr::group_by(.data$chapter) %>%
      dplyr::group_map(.f = ~{
        if(any(.x[.x$.variable_role == "dep", ".variable_name", drop=TRUE] %in%
               .x[.x$.variable_role == "indep", ".variable_name", drop=TRUE])) {
          cli::cli_abort("Variable cannot be both dependent and independent in same chapter.")
        }
      })
}
