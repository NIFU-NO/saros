
#' Retrieve Element from Elements List
#'
#' @inheritParams gen_qmd_report
#' @param call Internal.
#'
#' @return Either NULL or a filepath to the object as a string. Returns NULL if element_contents is NULL.
get_element_path <-
  function(
    data_overview = NULL,
    elements = NULL,
    glue_index_string = NULL,
    ignore_if_below = 0,
    path,
    call = rlang::caller_env()) {


    if(rlang::is_null(elements) ||
       (length(elements) == 1 && rlang::is_null(elements[[1]]))
      ) {
      return()
    }
    if(!rlang::is_bare_list(elements)) {
      cli::cli_abort("{.arg elements} must be a list, not {.obj_type_friendly {elements}}.")
      }

    check_list(elements, n = 1, call = call)

    element_name <- names(elements)
    element_contents <- elements[[1]]
    element_contents_names <- names(element_contents)


    ### index


    check_string(glue_index_string, n = 1, null.ok = TRUE, call = call)
    check_data_frame(data_overview, call = call)


    index <-
      data_overview %>%
      dplyr::slice(1) ### BAD PRACTICE TO SLICE FIRST ROW

    if(!rlang::is_null(glue_index_string)) {
      index <-
        index %>%
        glue::glue_data(glue_index_string) %>%
        conv_to_valid_obj_name()
    } else {
      if(!stringr::str_detect(string = element_name, pattern = "^bi_.*")) {
        index <-
          index %>%
          dplyr::distinct(dplyr::pick(tidyselect::all_of(dplyr::group_vars(.)))) %>%
          list_valid_obj_name()
      } else {
        by_index <-
          index %>%
          dplyr::pull(.data$by_cols_df) %>%
          .[[1]]
        if(!rlang::is_null(by_index)) {
          by_index <- by_index$col_name
        }
        index <-
          index %>%
          dplyr::distinct(dplyr::pick(tidyselect::all_of(dplyr::group_vars(.)))) %>%
          list_valid_obj_name() %>%
          stringr::str_c(., "_BY_", by_index)
      }
    }

    if(length(index) > 1) cli::cli_abort("{.arg data_overview} contains multiple grouping variables: {.var {index}}.")

    if(length(index) == 0) cli::cli_abort("{.arg index} is empty.")



    #### paths and folders
    path <- fs::path_expand(path)
    read_element_type_path <-
      fs::path(path, "raw_elements", element_name)

    ch_folder <-
      if(!is.null(data_overview$chapter)) data_overview$chapter else ""

    if(dplyr::n_distinct(ch_folder)>1L) {
      cli::cli_abort("If not NULL, {.arg data_overview$chapter} must contain exactly 1 unique value.")
    }
    save_element_type_path_relative <-
      ch_folder %>%
      unique() %>%
      fs::path(., element_name) %>%
      fix_path_spaces()
    save_element_type_path_absolute <-
      save_element_type_path_relative %>%
      fs::path(path, .)

    fs::dir_create(path = read_element_type_path, recurse = TRUE)
    fs::dir_create(path = save_element_type_path_absolute, recurse = TRUE)

    base_filename_rds <- stringr::str_c(index, ".rds")
    base_filename_png <- stringr::str_c(index, ".png")
    base_filename_xlsx <- stringr::str_c(index, ".xlsx")
    read_filepath <- fs::path(read_element_type_path,
                              base_filename_rds)
    save_filepath_relative <- fs::path(save_element_type_path_relative,
                                       base_filename_rds)
    save_filepath_absolute <- fs::path(path, save_filepath_relative)
    save_filepath_relative_png <- fs::path(save_element_type_path_relative,
                                       base_filename_png)
    save_filepath_relative_xlsx <- fs::path(save_element_type_path_relative,
                                           base_filename_xlsx)
    save_filepath_absolute_png <- fs::path(path, save_filepath_relative_png)
    save_filepath_absolute_xlsx <- fs::path(path, save_filepath_relative_xlsx)




    ### element_contents

    check_string(element_contents_names, n = NULL, null.ok = FALSE, call = call)

    if(rlang::is_bare_list(element_contents)) {
      obj <- element_contents[[index]]


      if(!rlang::is_null(obj)) {
        if(element_name == "uni_cat_text") utils::str(obj)
        tryCatch(expr = saveRDS(object = obj, file = save_filepath_absolute),
                 error = function(e) cli::cli_abort("Unable to save {.var {element_name}} to {.file save_filepath_absolute}."))
        if(ggplot2::is.ggplot(obj)) {
          tryCatch(expr = ggplot2::ggsave(plot = obj, filename = save_filepath_absolute_png,
                          scale = 1.5, width = 20, height = 20, units = "cm", dpi = "retina"),
                   error = function(e) cli::cli_warn("Unable to save png to {.file {save_filepath_absolute_png}}. Continuing without."))
        }
        if(inherits(x = obj, "data.frame")) {
          tryCatch(expr =
          writexl::write_xlsx(x = obj, path = save_filepath_absolute_xlsx),
          error = cli::cli_warn("Unable to save {element_name} to {.path {save_filepath_absolute_xlsx}}. Continuing without.")
          )
        }
        if(inherits(x = obj, "girafe")) {
          # ggiraph::dsvg(standalone = TRUE, file = save_filepath_absolute_png, width = 20, height = 20)
          # plot(obj)
          # dev.off()
        }
        caption <- attr(obj, "saros_caption")

        out <-
          insert_obj_in_qmd(element_name = element_name,
                            index = index,
                            filepath = save_filepath_relative,
                            caption = caption,
                            call = call)
      } else out <- ""
      return(out)
    }
    if(rlang::is_character(element_contents)) {
      if(!fs::file_exists(element_contents[[index]])) {
        cli::cli_abort("Can't find file {.path {element_contents[index]}}.", call = call)
      }
      fs::file_copy(path = element_contents[[index]],
                    new_path = save_filepath_absolute, overwrite = TRUE)

      out <-
        insert_obj_in_qmd(element_name = element_name,
                          index = index,
                          filepath = save_filepath_relative,
                          call = call)
      return(out)
    }

    cli::cli_abort(c("{.arg {elements}} must be",
                     "*" = "NULL, ",
                     "*" = "a named character vector of filepaths, or ",
                     "*" = "a named list of objects,",
                     "i" = "not {.obj_type_friendly {elements}}"), call = call)
  }
