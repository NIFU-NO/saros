gen_default_report_yaml <- function(yaml_path) {
  yaml_defaults <-
    ymlthis::yml_empty() %>%
    ymlthis::yml_params(
      label_separator = options()$saros$label_separator,
      name_separator = options()$saros$name_separator,
      group_by = options()$saros$group_by,
      sort_by = options()$saros$sort_by,
      descending = options()$saros$descending,
      glue_index_string = options()$saros$glue_index_string,
      ignore_if_below = options()$saros$element_args$ignore_if_below,
      captions = options()$saros$captions,
      report_ymlthis_config = options()$saros$report_ymlthis_config,
      chapter_ymlthis_config = options()$saros$chapter_ymlthis_config,
      index_filename = options()$saros$index_filename,
      element_names =
        options()$saros$element_names %>%
        .[.] %>%
        names() %>%
        stringr::str_match_all(pattern = ".*_html|.*_sigtest") %>%
        unlist(),
      translations = options()$saros$translations,
      element_args = options()$saros$element_args
      ) %>%
    yaml::as.yaml()
  cat(yaml_defaults, file = yaml_path)
  yaml::read_yaml(yaml_path)
}
