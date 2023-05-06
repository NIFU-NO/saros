gen_tailored_reports <- function(chapter_overview,
                                 data,
                                 path = "testreport",
                                 report_generation_yaml_path = NULL,
                                 grouping_variables = "f_uni") {

  data %>%
    dplyr::group_by(dplyr::pick(tidyselect::all_of(grouping_variables))) %>%
    dplyr::group_map(.keep = TRUE, .f = function(data_part, key_part) {
      subfolder <- key_part %>% dplyr::select(tidyselect::all_of(grouping_variables)) %>% unlist()
      absolute_folder <- fs::path(path, rlang::exec(fs::path, subfolder))

      render_saros_report(chapter_overview = chapter_overview, # Currently assumes that grouping_variables are not included
                          data = data_part,
                          path = absolute_folder,
                          report_generation_yaml_path = report_generation_yaml_path)
    }) %>%
    unlist()
}
