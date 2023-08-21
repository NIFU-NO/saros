check_duplicates_in_data_overview <-
  function(data, data_overview) {

    data_overview %>%
      dplyr::group_by(.data$chapter) %>%
      dplyr::group_map(.f = ~{
        if(any(.x[.x$designated_role == "dep", "col_name", drop=TRUE] %in%
               .x[.x$designated_role == "indep", "col_name", drop=TRUE])) {
          cli::cli_abort("Variable cannot be both dependent and independent in same chapter.")
        }
      })
}
