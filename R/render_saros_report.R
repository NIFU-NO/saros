#' Automatically Render a Saros Report from Scratch
#' @param chapter_overview A raw overview of chapters, as a data frame/tibble with one row per chapter. Must contain columns 'chapter', 'dep_cat', etc.
#' @param data A data frame (or a srvyr-object) with the columns specified in the chapter_overview 'dep_cat', etc columns.
#' @param path A path, defaults to working directory.
#' @param yaml_path Path to a yml-file specifying the
#' @importFrom rlang !!!
#'
#' @return Path to index qmd-file. If not specified in the yaml_path file, will default to index.qmd.
#' @export
#'
#' @examples
#' \dontrun{
#' render_saros_report(
#'     chapter_overview = ex_survey_ch_overview,
#'     data = ex_survey1)
#' }
render_saros_report <-
  function(chapter_overview,
           data,
           path = "testreport",
           yaml_path = fs::path(path, "report_settings.yml")
  ) {
    check_data_frame(chapter_overview)
    check_data_frame(data)
    check_string(yaml_path, n=1, null.ok=FALSE)
    fs::dir_create(path = path, recurse = TRUE)

    yml <-
      tryCatch(expr = yaml::yaml.load_file(yaml_path),
          warning = function(e) {
            cli::cli_progress_message(msg = "Creating YAML-template file...")
            cli::cli_warn("{.arg {yaml_path}} did not exist, created it with defaults.")
            gen_default_report_yaml(yaml_path = yaml_path)
            })

    yml_element_args <- yml$params$element_args

    data_overview <-
      rlang::exec(
        refine_data_overview,
        data_overview = chapter_overview,
        label_separator = yml$params$label_separator,
        name_separator = yml$params$name_separator,
        data = data)

    cli::cli_progress_message(msg = "Producing contents...")
    elements_list <-
      rlang::exec(
      mass_lst_saros_elements,
      data = data,
      data_overview = data_overview,
      element_names = yml$params$element_names,
      label_separator = yml$params$label_separator,
      !!!yml_element_args)

    cli::cli_progress_message(msg = "Creating chapter files...")
    report_filepath <-
      rlang::exec(
      gen_qmd_report,
      data_overview = data_overview,
      elements = elements_list,
      glue_index_string = yml$params$glue_index_string,
      ignore_if_below = yml$params$ignore_if_below,
      captions = yml$params$captions,
      report_ymlthis_config = yml$params$report_ymlthis_config,
      chapter_ymlthis_config = yml$params$chapter_ymlthis_config,
      index_filename = yml$params$index_filename,
      path = path)

    # if(!is.null(quarto::quarto_path())) {
    #   cli::cli_progress_message(msg = "Rendering Quarto (*.qmd) files...")
      # tryCatch(expr =
                 # quarto::quarto_render(input = report_filepath)#,
               # error = function(e) {
                 # return(list(e, report_filepath))
               # })
      # }
    if(interactive()) {
        utils::browseURL(url = report_filepath)
    }
    report_filepath
  }
