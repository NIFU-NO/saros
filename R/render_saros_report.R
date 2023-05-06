#' Automatically Render a Saros Report from Scratch
#' @param chapter_overview A raw overview of chapters, as a data frame with one row per chapter. Must contain columns 'chapter', 'dep_cat', etc.
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
           report_generation_yaml_path = fs::path(path, "_report_generation_setup.yml")
  ) {
    timestamp <- proc.time()
    check_data_frame(chapter_overview)
    check_data_frame(data)
    check_string(path, n=1, null.ok=TRUE)
    check_string(report_generation_yaml_path, n = 1, null.ok=FALSE)




    if(inherits(data, "survey")) {
      data <- srvyr::ungroup(data)
    } else data <- dplyr::ungroup(data)

    yml <- gen_default_report_yaml(yaml_path = report_generation_yaml_path,
                                   data_colnames = colnames(data))

    yml <- argument_validation_and_insertion(params = yml)

    # if(!rlang::is_null(yml$element_args$colour_na)) {
    #   data <-
    #   dplyr::mutate(data, dplyr::across(tidyselect::where(~{
    #     dplyr::n_distinct(.x, na.rm = yml$element_args$showNA == "never") == 2 &
    #       (is.factor(.x) | is.character(.x))}),
    #     .fns = ~forcats::fct_rev(.x)))
    # }

    data_overview <-
      rlang::exec(
        refine_data_overview,
        data = data,
        data_overview = chapter_overview,
        label_separator = yml$label_separator,
        name_separator = yml$name_separator,
        !!!yml$element_args) %>%
      dplyr::filter(.data$designated_role == "dep",
                    .data$designated_type != "txt",
                    .data$col_type != "chr"
      ) ## TEMPORARY FIX!!


    if(nrow(data_overview)==0) cli::cli_abort("{.var data_overview} is empty! Something is not right. Are there no factors in your data?")

      chapter_filepaths <-
        rlang::exec(
          gen_qmd_chapters,
          data_overview = data_overview,
          data = data,
          element_names = yml$element_names,
          chapter_yaml_file = yml$chapter_yaml_file,
          label_separator = yml$label_separator,
          translations = yml$translations,
          !!!yml$element_args,
          path = path)


      report_filepath <-
        gen_qmd_index(
          yaml_file = yml$report_yaml_file,
          authors = unique(data_overview[["author"]]),
          index_filepath = fs::path(path, yml$index_filename),
          chapter_filepaths = chapter_filepaths,
          call = rlang::caller_env())

    if(interactive()) {
        utils::browseURL(url = report_filepath)
    }
    cat(proc.time()-timestamp)
    cat("\n")
    report_filepath
  }
