compare_many <- function(x) {
  all(purrr::map_lgl(as.list(x[-1]),
                     .f = ~identical(.x, x[[1]])))
}


#' Mass Create Elements of A Certain Type
#'
#' @param data_overview_section A (grouped) data frame containing at least the following columns
#' \describe{
#' \item{<grouping variables>}{ used to distinguish sets of columns belonging to each element.}
#' \item{"col_name"}{ name of columns.}
#' \item{"designated_type"}{ either 'cat' (categorical/ordinal/binary), 'int' (interval/continuous) or 'chr' (text).}
#' }
#' @param element_name See \code{element_list()} for options.
#' @param data Survey data.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments forwarded to the corresponding functions that create the elements.
#'
#' @return Named list of elements, where each element can .
#' @importFrom rlang !!!
gen_element_and_qmd_snippet <-
  function(data_overview_section,
           element_name = "uni_cat_prop_plot",
           data,
           summarized_data = NULL,
           element_folderpath_absolute,
           element_folderpath_relative,
           grouping_structure = NULL,
           translations = .saros.env$defaults$translations,
           ...,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)



    stopifnot(inherits(data, "data.frame") || inherits(data, "survey"))
    data_cols <- if(inherits(data, "survey")) survey:::dimnames.survey.design(data)[[2]] else colnames(data)

    fs::dir_create(element_folderpath_absolute, recurse = TRUE)

    if(dplyr::n_distinct(data_overview_section$col_type) != 1 ||
       dplyr::n_distinct(data_overview_section$designated_type) != 1 || # Later add check that all items contain the same by_cols_df
       dplyr::n_distinct(data_overview_section$label_prefix) != 1) return("")


    grouping_structure <- dplyr::group_vars(data_overview_section)
    grouping_structure2 <- grouping_structure[!grouping_structure %in% "chapter"]

    section_key <- data_overview_section
    section_key <- dplyr::ungroup(section_key)
    section_key <- dplyr::distinct(section_key, dplyr::pick(tidyselect::all_of(grouping_structure2)))
    section_key <- dplyr::group_by(section_key, dplyr::pick(tidyselect::all_of(grouping_structure2)))


    if(nrow(section_key)>1) cli::cli_warn("Something weird going on in grouping.")

    name <- list_valid_obj_name(section_key, max_width = dots$max_width_obj)


    y_col_names <- unique(data_overview_section$col_name)
    y_col_pos <- match(y_col_names, data_cols)

    variable_prefix <- if(!rlang::is_null(section_key$name_prefix) &&
                          dplyr::n_distinct(section_key$name_prefix)==1) unique(section_key$name_prefix)



    if(!stringi::stri_detect(element_name, regex="^bi_.*")) {

      filename_prefix <- name
      filepath_rel_rds <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".rds"))
      filepath_rel_png <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".png"))
      filepath_rel_xlsx <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".xlsx"))
      filepath_rel_txt <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".txt"))
      filepath_rel_docx <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".docx"))
      filepath_abs_rds <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".rds"))
      filepath_abs_png <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".png"))
      filepath_abs_xlsx <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".xlsx"))
      filepath_abs_txt <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".txt"))
      filepath_abs_docx <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".docx"))

      plot_height <-
        if(!dots$vertical) {
          max(stringi::stri_length(data_overview_section$label_prefix), na.rm=TRUE) /
            dots$x_axis_label_width *
            length(y_col_pos) *
            dots$plot_height_multiplier +
            dots$plot_height_fixed_constant
        } else 10


      ######################################################################

      if(element_name == "uni_cat_text" &&
         all(data_overview_section$designated_type == "cat")) {
        out <-
          rlang::exec(
            embed_cat_text_html,
            data = data,
            cols = y_col_pos,
            summarized_data = summarized_data,
            translations = translations,
            !!!dots)
        writeLines(text = stringr::str_c(out, collapse=""), con = filepath_abs_txt)
      }

      ######################################################################


      if(element_name == "uni_cat_prop_plot" &&
         all(data_overview_section$designated_type == "cat")) {
        out <-
          rlang::exec(
            embed_cat_prop_plot_docx,
            data = data,
            cols = y_col_pos,
            summarized_data = summarized_data,
            translations = translations,
            !!!dots)
        print(out, target = filepath_abs_docx)

        out_html <-
          rlang::exec(
            embed_cat_prop_plot,
            data = data,
            cols = y_col_pos,
            summarized_data = summarized_data,
            translations = translations,
            html_interactive = TRUE,
            !!!dots)
        ggplot2::ggsave(plot = out_html, filename = filepath_abs_png,
                        scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
                        units = "cm", dpi = "retina")
        saveRDS(out_html, file = filepath_abs_rds)

        # out_pdf <-
        #   rlang::exec(
        #     embed_cat_prop_plot,
        #     data = data,
        #     cols = y_col_pos,
        #     summarized_data = summarized_data,
        #     translations = translations,
        #     html_interactive = FALSE,
        #     !!!dots)
        # ggplot2::ggsave(plot = out_pdf, filename = filepath_abs_png,
        #                 scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
        #                 units = "cm", dpi = "retina")

        return(
          stringr::str_c(
            insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                              index = name,
                              variable_prefix = variable_prefix,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              add_text = FALSE,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              caption = attr(out_html, "saros_caption")),
            insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                              index = name,
                              variable_prefix = variable_prefix,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              caption = attr(out_html, "saros_caption")), sep="\n"))
      }
      ######################################################################

      if(element_name == "uni_cat_freq_plot" &&
         all(data_overview_section$designated_type == "cat")) {
        out_docx <-
          rlang::exec(
            embed_cat_freq_plot_docx,
            data = data,
            cols = y_col_pos,
            summarized_data = summarized_data,
            translations = translations,
            !!!dots)
        print(out_docx, target = filepath_abs_docx)

        out_html <-
          rlang::exec(
            embed_cat_freq_plot,
            data = data,
            cols = y_col_pos,
            summarized_data = summarized_data,
            translations = translations,
            html_interactive = TRUE,
            !!!dots)
        ggplot2::ggsave(plot = out_html, filename = filepath_abs_png,
                        scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
                        units = "cm", dpi = "retina")
        saveRDS(out_html, file = filepath_abs_rds)


        return(
          stringr::str_c(
            insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                              index = name,
                              variable_prefix = variable_prefix,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              add_text = FALSE,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              caption = attr(out_html, "saros_caption")),
            insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                              index = name,
                              variable_prefix = variable_prefix,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              caption = attr(out_html, "saros_caption")), sep="\n"))
      }


      ######################################################################

      ######################################################################


      # if(element_name == "uni_cat_freq_plot_html" &&
      #    all(data_overview_section$designated_type == "cat")) {
      #   out <-
      #     rlang::exec(
      #       embed_cat_freq_plot,
      #       data = data,
      #       cols = y_col_pos,
      #       summarized_data = summarized_data,
      #       translations = translations,
      #       html_interactive = TRUE,
      #       !!!dots)
      #
      # }
      # ######################################################################
      #
      #
      # if(element_name == "uni_cat_freq_plot_docx" &&
      #    all(data_overview_section$designated_type == "cat")) {
      #   out <-
      #     rlang::exec(
      #       embed_cat_freq_plot_docx,
      #       data = data,
      #       cols = y_col_pos,
      #       summarized_data = summarized_data,
      #       translations = translations,
      #       !!!dots)
      #   print(out, target = filepath_abs_docx)
      # }
      #
      # ######################################################################
      #
      #
      # if(element_name == "uni_cat_freq_plot_pdf" &&
      #    all(data_overview_section$designated_type == "cat")) {
      #   out <-
      #     rlang::exec(
      #       embed_cat_freq_plot,
      #       data = data,
      #       cols = y_col_pos,
      #       summarized_data = summarized_data,
      #       translations = translations,
      #       html_interactive = FALSE,
      #       !!!dots)
      # }

      ######################################################################


      if(element_name == "uni_cat_table" &&
         all(data_overview_section$designated_type == "cat")) {
        out <-
          rlang::exec(
            embed_cat_table,
            data = data,
            cols = y_col_pos,
            summarized_data = summarized_data,
            translations = translations,
            !!!dots)
        saveRDS(out, file = filepath_abs_rds)
        writexl::write_xlsx(x=out, path = filepath_abs_xlsx)
      }

      ######################################################################

      # if(element_name == "uni_cat_table_docx" &&
      #    all(data_overview_section$designated_type == "cat")) {
      #   out <-
      #     rlang::exec(
      #       embed_cat_table_docx,
      #       data = data,
      #       cols = y_col_pos,
      #       summarized_data = summarized_data,
      #       !!!dots)
      #   print(out, target = filepath_abs_docx)
      #
      # }

      ######################################################################

      if(stringi::stri_detect(element_name, fixed = "uni_sigtest") &&
         dplyr::n_distinct(unique(data_overview_section$designated_type)) == 1) {
        out <-
          rlang::exec(
            embed_uni_sigtest,
            data = data,
            cols = y_col_pos,
            col_type = unique(data_overview_section$designated_type),
            translations = translations,
            !!!dots)
        saveRDS(out, file = filepath_abs_rds)
        writexl::write_xlsx(x=out, path = filepath_abs_xlsx)

      }


      if(exists("out")) {
          out <-
            insert_obj_in_qmd(element_name = element_name,
                              index = name,
                              variable_prefix = variable_prefix,
                              filepath_txt = filepath_abs_rds,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              caption = attr(out, "saros_caption"))
        return(out)
      }
    }



    ######################################################################

    if(all(data_overview_section$designated_role != "indep") &&
       stringi::stri_detect(str = element_name, regex="^bi_.*") &&
       !rlang::is_null(data_overview_section$by_cols_df) &&
       !rlang::is_null(data_overview_section$by_cols_df[[1]]) &&
       nrow(data_overview_section$by_cols_df[[1]])>0 &&
       compare_many(data_overview_section$by_cols_df)) {

      by_df <- data_overview_section$by_cols_df[[1]]


      if(inherits(by_df, what = "data.frame")) {

        name_by <-
          stats::setNames(unique(by_df$col_name),
                   nm = stringr::str_c(name, "_BY_", unique(by_df$col_name)))


        if(stringi::stri_detect(str = element_name, fixed = "bi_sigtest")) {

          filename_prefix <- stringr::str_c(name, "_BY_ALL_INDEP")
          filepath_rel_rds <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".rds"))
          filepath_rel_xlsx <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".xlsx"))
          filepath_abs_rds <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".rds"))
          filepath_abs_xlsx <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".xlsx"))


        out <-
        purrr::map2(.x = name_by, .y = names(name_by), .f = ~{

          by_pos <- match(.x, data_cols)

          by_type <- vctrs::vec_slice(by_df, by_df$col_name == .x)
          by_type <- by_type$designated_type

          ##############################################################################
          if(dplyr::n_distinct(data_overview_section$designated_type) == 1 &&
             dplyr::n_distinct(by_type) == 1) {
            return(
              rlang::exec(
                embed_bi_sigtest,
                data = data,
                cols = y_col_pos,
                by = by_pos,
                col_type = unique(data_overview_section$designated_type),
                by_type = unique(by_type),
                translations = translations,
                !!!dots)
            )
          }
        }) %>%
          bind_rows()

        if(nrow(out)>0) {
          writexl::write_xlsx(x=out, path = filepath_abs_xlsx)
          saveRDS(out, file = filepath_abs_rds)
          return(
            insert_obj_in_qmd(element_name = element_name,
                              index = filename_prefix,
                              filepath_txt = filepath_abs_rds,
                              filepath = filepath_rel_rds,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              caption = attr(out, "saros_caption")))
          }
        } else {


        purrr::map2_chr(.x = name_by, .y = names(name_by), .f = ~{

          by_pos <- match(.x, data_cols)

          by_type <- vctrs::vec_slice(by_df, by_df$col_name == .x)
          by_type <- by_type$designated_type

          filename_prefix <- stringr::str_c(.y, collapse = "_")
          filepath_rel_rds <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".rds"))
          filepath_rel_png <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".png"))
          filepath_rel_xlsx <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".xlsx"))
          filepath_rel_txt <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".txt"))
          filepath_rel_docx <- fs::path(element_folderpath_relative, stringr::str_c(filename_prefix, ".docx"))
          filepath_abs_rds <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".rds"))
          filepath_abs_xlsx <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".xlsx"))
          filepath_abs_png <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".png"))
          filepath_abs_txt <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".txt"))
          filepath_abs_docx <- fs::path(element_folderpath_absolute, stringr::str_c(filename_prefix, ".docx"))


          plot_height <-
            if(!dots$vertical) {
              max(stringi::stri_length(data_overview_section$label_prefix), na.rm=TRUE) /
                dots$x_axis_label_width *
                length(y_col_pos) *
                length(by_pos) *
                dplyr::n_distinct(data[[by_pos]], na.rm = TRUE) *
                dots$plot_height_multiplier +
                dots$plot_height_fixed_constant
            } else 10


          ##############################################################


          # if(element_name == "bi_catcat_text" &&
          #    all(data_overview_section$designated_type == "cat") &&
          #    all(by_type == "cat")) {
          #   out <-
          #     rlang::exec(
          #       embed_cat_text_html,
          #       data = data,
          #       cols = y_col_pos,
          #       by = by_pos,
          #       !!!dots)
          #
          # }



          if(element_name == "bi_catcat_prop_plot" &&
             all(data_overview_section$designated_type == "cat") &&
             all(by_type == "cat")) {
            out_docx <-
              rlang::exec(
                embed_cat_prop_plot_docx,
                data = data,
                cols = y_col_pos,
                by = by_pos,
                summarized_data = summarized_data,
                translations = translations,
                !!!dots)
            print(out_docx, target = filepath_abs_docx)

            out_html <-
              rlang::exec(
                embed_cat_prop_plot,
                data = data,
                cols = y_col_pos,
                by = by_pos,
                summarized_data = summarized_data,
                translations = translations,
                html_interactive = TRUE,
                !!!dots)
            ggplot2::ggsave(plot = out_html, filename = filepath_abs_png,
                            scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
                            units = "cm", dpi = "retina")
            saveRDS(out_html, file = filepath_abs_rds)

            return(
              stringr::str_c(
                insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                                  index = filename_prefix,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  add_text = FALSE,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  caption = attr(out_html, "saros_caption")),
                insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                                  index = filename_prefix,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  caption = attr(out_html, "saros_caption")), sep="\n"))


          }

          if(element_name == "bi_catcat_freq_plot" &&
             all(data_overview_section$designated_type == "cat") &&
             all(by_type == "cat")) {
            out_docx <-
              rlang::exec(
                embed_cat_freq_plot_docx,
                data = data,
                cols = y_col_pos,
                by = by_pos,
                summarized_data = summarized_data,
                translations = translations,
                !!!dots)
            print(out_docx, target = filepath_abs_docx)

            out_html <-
              rlang::exec(
                embed_cat_freq_plot,
                data = data,
                cols = y_col_pos,
                by = by_pos,
                summarized_data = summarized_data,
                translations = translations,
                html_interactive = TRUE,
                !!!dots)
            ggplot2::ggsave(plot = out_html, filename = filepath_abs_png,
                            scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
                            units = "cm", dpi = "retina")
            saveRDS(out_html, file = filepath_abs_rds)

            return(
              stringr::str_c(
                insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                                  index = filename_prefix,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  add_text = FALSE,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  caption = attr(out_html, "saros_caption")),
                insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                                  index = filename_prefix,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  caption = attr(out_html, "saros_caption")), sep="\n"))


          }

          if(element_name == "bi_catcat_table" &&
             all(data_overview_section$designated_type == "cat") &&
             all(by_type == "cat")) {

            out <-
              rlang::exec(
                embed_cat_table,
                data = data,
                cols = y_col_pos,
                by = by_pos,
                summarized_data = summarized_data,
                translations = translations,
                !!!dots)
            writexl::write_xlsx(x=out, path = filepath_abs_xlsx)
            saveRDS(out, file = filepath_abs_rds)
          }


          if(exists("out")) {


              return(
                insert_obj_in_qmd(element_name = element_name,
                                  index = filename_prefix,
                                  filepath_txt = filepath_abs_rds,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  caption = attr(out, "saros_caption")))
          } else ""
        }) %>% stringr::str_c(collapse = "\n")
        }
      }
    }
  }

