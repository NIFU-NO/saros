check_duplicates_in_chapter_overview <-
  function(data, chapter_overview) {

    chapter_overview %>%
      dplyr::group_by(.data$chapter) %>%
      dplyr::group_map(.f = ~{
        if(any(.x[[".variable_name_dep"]] %in% .x[[".variable_name_indep"]])) {
          cli::cli_abort("Variable cannot be both dependent and independent in same chapter.")
        }
      })
}
