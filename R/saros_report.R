render_saros_report <-
  function(chapter_overview,
           data,
           path = getwd(),
           yaml_path = fs::path(path, "_metadata.yml")
  ) {
    check_data_frame(chapter_overview)
    check_data_frame(data)
    check_string(yaml_path, n=1, null.ok=FALSE)
    if(!fs::is_file(yaml_path, follow = TRUE)) {
      cli::cli_abort("{.arg {yaml_path}} is not a valid file path.")
    }

    yml <- yaml::yaml.load_file(yaml_path)
    yml_element_args <- yml$element_args
    # Must add all missing entries by using defaults?

    data_overview <-
      refine_data_overview(
        data_overview = chapter_overview,
        data = data,
        !!!yml_element_args)


    elements_list <-
      rlang::exec(
      mass_lst_saros_elements,
      vec = yml$elements_vector,
      data_overview = data_overview,
      data = data,
      !!!yml_element_args)

    report_filepath <-
      rlang::exec(
      gen_qmd_report,
      data_overview = data_overview,
      elements = elements_list,
      glue_index_string = yml$glue_index_string,
      show_if_alpha_below = yml$show_if_alpha_below,
      captions = yml$captions,
      report_ymlthis_config = yml$report_ymlthis_config,
      chapter_ymlthis_config = yml$chapter_ymlthis_config,
      index_filename = yml$index_filename,
      path = path)

    if(interactive()) {
      quarto::quarto_render(report_filepath)

      utils::browseURL(url = report_filepath)
    }
    report_filepath
  }
