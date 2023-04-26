gen_default_report_yaml <- function(yaml_path) {
  yaml_defaults <-
    ymlthis::yml_empty() %>%
    ymlthis::yml_params(
      label_separator = getOption("saros")$label_separator,
      name_separator = getOption("saros")$name_separator,
      group_by = getOption("saros")$group_by,
      sort_by = getOption("saros")$sort_by,
      descending = getOption("saros")$descending,
      glue_index_string = getOption("saros")$glue_index_string,
      ignore_if_below = getOption("saros")$element_args$ignore_if_below,
      report_ymlthis_config = getOption("saros")$report_ymlthis_config,
      chapter_ymlthis_config = getOption("saros")$chapter_ymlthis_config,
      index_filename = getOption("saros")$index_filename,
      element_names =
        getOption("saros")$element_names %>%
        .[.] %>%
        names() %>%
        stringr::str_match_all(pattern = ".*_html|.*_sigtest") %>%
        unlist(),
      translations = getOption("saros")$translations,
      element_args = getOption("saros")$element_args
      ) %>%
    yaml::as.yaml()
  cat(yaml_defaults, file = yaml_path)
  yaml::read_yaml(yaml_path)
}
