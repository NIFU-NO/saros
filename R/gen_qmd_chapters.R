#' Generate A Quarto Survey Report
#'
#' This function generates a Quarto survey report.
#'
#' @details
#' A report consists of multiple chapters and an index file that merges them together.
#' A chapter can contain any user-defined set of dependent, independent or bivariate variable sets.
#' A chapter consists of multiple sections.
#' A section is defined as a group in the chapter_overview (ignoring the chapter grouping level) containing variables of the same type, meaning at a minimum that the variables in the section sharing the same response options, the same main question, and being of the same data type.
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @param mesos_group *Specific group to compare with*
#'
#'   `scalar<character>` // *Default:* `NULL` (`optional`)
#'
#'   Both the absolute and relative folderpaths are required, as strings.
#'
#'
#' @return A Quarto survey report generated in the specified working directory.
#' @keywords internal
#'
gen_qmd_chapters <-
  function(chapter_overview,
           data,
           mesos_group = NULL,
           ...,
           call = rlang::caller_env()
  ) {

    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)

    check_string(mesos_group, n=1, null.ok=TRUE, call = call)

    path <- fs::as_fs_path(dots$path)
    dir.create(path = path, recursive = TRUE, showWarnings = FALSE)


    grouping_structure <- dplyr::group_vars(chapter_overview)

    if(length(grouping_structure) == 0) {
      cli::cli_abort(c(
        "!"="No grouping variables found in {.arg chapter_overview}.",
        "x"="Without grouping variables, contents in {.arg elements} cannot be located.",
        "i"="Consider using {.fun refine_chapter_overview}."))
    }

    chapter_overview_chapter_groups <-
      dplyr::group_by(chapter_overview,
                      dplyr::pick(tidyselect::all_of(grouping_structure[1])))

    total_iterations <-
      dplyr::n_distinct(chapter_overview_chapter_groups[[grouping_structure[1]]])

    cli::cli_progress_bar(name = "Creating chapter files...",
                          type = "iterator",
                          format_done = "Chapter files completed!", clear = FALSE,
                          total = total_iterations)


    ## Generate each chapter. Returns paths to files, which are then used for index.qmd
    chapter_filepaths <-
      chapter_overview_chapter_groups %>% # CONVERT TO unlist(lapply(names(attr(chapter_overview_chapter, "groups"))[names(.) != ".rows"])) by taking dplyr::
      dplyr::group_map(
        .keep = TRUE,
        .f = function(chapter_overview_chapter,
                      key_chapter) {





          chapter_overview_chapter <-
            dplyr::group_by(chapter_overview_chapter,
                            dplyr::pick(tidyselect::all_of(grouping_structure)))

          # Paths


          chapter_foldername <-
            unique(chapter_overview_chapter[[grouping_structure[1]]])
          chapter_foldername <- as.character(chapter_foldername)

          chapter_number <- match(chapter_foldername,
                                  unique(chapter_overview$chapter))
          digits <- floor(log10(dplyr::n_distinct(chapter_overview_chapter_groups[[grouping_structure[1]]])))+1
          chapter_number_text <- sprintf(paste0("%0", digits, "d"), as.integer(chapter_number))

          chapter_foldername_clean <-
            filename_sanitizer(chapter_foldername, max_chars = dots$max_clean_folder_name)
          chapter_foldername_clean <-
            stringi::stri_c(chapter_number_text, "_", chapter_foldername_clean,
                            ignore_null = TRUE)

          cli::cli_progress_message(msg = "Generating for chapter {chapter_foldername}: {chapter_foldername_clean}")


          chapter_folderpath_absolute <- file.path(path, chapter_foldername_clean)
          dir.create(path = chapter_folderpath_absolute, recursive = TRUE, showWarnings = FALSE)

          chapter_filepath_relative <- stringi::stri_c(chapter_foldername_clean, ".qmd", ignore_null=TRUE)
          chapter_filepath_absolute <- file.path(path, chapter_filepath_relative)


          authors <- get_authors(data = chapter_overview_chapter, col = "authors")
          chapter_yaml <- process_yaml(yaml_file = dots$chapter_yaml_file,
                                      authors = authors,
                                      title = NULL,
                                      chapter_number = chapter_number)

          if(!all(is.na(chapter_overview_chapter$.variable_name_dep))) {

          chapter_contents <-
            rlang::exec(
              gen_qmd_structure2,
              data = data,
              chapter_overview = chapter_overview_chapter,
              mesos_group = mesos_group,
              chapter_folderpath_absolute = chapter_folderpath_absolute,
              chapter_foldername = chapter_foldername_clean,
              !!!dots#[!names(dots) %in% c("chapter_overview", "call")]
              )

          } else chapter_contents <- NULL

          qmd_start_section <-
            if(!is.null(dots$qmd_start_section_filepath)) {
              stringi::stri_c(collapse = "\n",
                              ignore_null = TRUE,
                              readLines(con = dots$qmd_start_section_filepath)
                              )
            }

          qmd_end_section <-
            if(!is.null(dots$qmd_end_section_filepath)) {
              stringi::stri_c(collapse = "\n",
                              ignore_null = TRUE,
                              readLines(con = dots$qmd_end_section_filepath)
                              )
            }

          load_dataset <-
            if(isTRUE(dots$attach_chapter_dataset)) {
              attach_chapter_dataset2(data = data,
                                     chapter_overview_chapter = chapter_overview_chapter,
                                     chapter_foldername_clean = chapter_foldername_clean,
                                     path = path,
                                     mesos_var = dots$mesos_var,
                                     auxiliary_variables = dots$auxiliary_variables,
                                     serialized_format = dots$serialized_format)
            }

          # if(chapter_foldername != "Introduction") browser()

          out <-
          stringi::stri_c(chapter_yaml,
                          stringi::stri_c("# ", chapter_foldername),
                          load_dataset,
                         qmd_start_section,
                         chapter_contents,
                         qmd_end_section,
                         sep = "\n", ignore_null=TRUE)
          out <- stringi::stri_replace_all_regex(out,
                                                 pattern = "\n{3,}",
                                                 replacement = "\n\n\n")

          cat(out, file = chapter_filepath_absolute)

          chapter_filepath_relative

        })

    chapter_filepaths <- unlist(chapter_filepaths)

    cli::cli_process_done(msg_done = "Completed report{if(is_string(mesos_group)) paste0(' for ', mesos_group)}.")


    chapter_filepaths


  }
