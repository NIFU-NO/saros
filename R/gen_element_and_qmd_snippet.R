# are all elements of list x identical to each other?
compare_many <- function(x) {
  all(unlist(lapply(as.list(x[-1]),
                     FUN = function(.x) identical(.x, x[[1]])))) ||
  nrow(x[[1]])==1
}


#' Mass Create Elements of A Certain Type
#'
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#'
#' @param chapter_overview_section *Overview of chapter section*
#'
#'   `obj:<data.frame>|obj:<tbl_df>` // Required
#'
#'   Data frame (or tibble, possibly grouped). Must contain column 'dep'
#'   with similar items. See `draft_report()`.
#'
#' @param element_name *Element name*
#'
#'   `scalar<character>` // *default:* `"uni_cat_prop_plot"`
#'
#'   One of the element_names, see `draft_report()`.
#'
#' @param element_folderpath_absolute,element_folderpath_relative *Absolute and relative folder paths*
#'
#'   `scalar<character>` // Required
#'
#'   Both the absolute and relative folderpaths are required, as strings.
#'
#' @param grouping_structure *Vector of groups*
#'
#'  `vector<character>` // *default:* `NULL` (`Optional`)
#'
#'  Internal usage.
#'
#' @return Named list of elements, where each element can UNFINISHED.
#' @importFrom rlang !!!
#'
gen_element_and_qmd_snippet <-
  function(chapter_overview_section,
           element_name = "uni_cat_prop_plot",
           data,
           mesos_group = NULL,
           element_folderpath_absolute,
           element_folderpath_relative,
           grouping_structure = NULL,
           ...,
           call = rlang::caller_env()) {

    if(element_name == "hline") return("---")

    dots <- rlang::list2(...)
    dots <- utils::modifyList(x = formals(draft_report)[!names(formals(draft_report)) %in% c("data", "chapter_overview", "...")],
                              val = dots[!names(dots) %in% c("...")], keep.null = TRUE)

    stopifnot(inherits(data, "data.frame") || inherits(data, "survey"))
    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)

    fs::dir_create(element_folderpath_absolute, recurse = TRUE)

    if(dplyr::n_distinct(chapter_overview_section$.variable_type) != 1 || # Later add check that all items contain the same indep_cols_df
       dplyr::n_distinct(chapter_overview_section$.variable_label_prefix) != 1) return("")


    grouping_structure <- dplyr::group_vars(chapter_overview_section)
    grouping_structure2 <- grouping_structure[!grouping_structure %in% "chapter"]

    section_key <- chapter_overview_section
    section_key <- dplyr::ungroup(section_key)
    section_key <- dplyr::distinct(section_key, dplyr::pick(tidyselect::all_of(grouping_structure2)))
    section_key <- dplyr::group_by(section_key, dplyr::pick(tidyselect::all_of(grouping_structure2)))


    if(nrow(section_key)>1) cli::cli_warn("Something weird going on in grouping.")

    obj_name <- stringi::stri_c(ignore_null=TRUE, list_valid_obj_name(section_key, max_width = dots$max_width_obj),
                               if(rlang::is_string(mesos_group)) "_", mesos_group)


    ## Only for filenames
    # if(any(grouping_structure2 %in% ".variable_label_prefix")) {
      grouping_structure3 <- grouping_structure2
      grouping_structure3[grouping_structure3 %in%
                            c(".variable_label_prefix", ".variable_label_suffix")] <- ".variable_name"
      grouping_structure3 <- unique(grouping_structure3)
    # }
    filename_prefix <- chapter_overview_section
    filename_prefix <- dplyr::ungroup(filename_prefix)
    filename_prefix <- dplyr::distinct(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure3)))
    filename_prefix <- dplyr::arrange(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure3)))
    filename_prefix <- dplyr::group_by(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure3)))
    # filename_prefix <- dplyr::distinct(filename_prefix, dplyr::pick(tidyselect::everything()))
    filename_prefix <- glue::glue_data(filename_prefix, stringi::stri_c(ignore_null=TRUE, "{", colnames(filename_prefix), "}", collapse="_"))
    filename_prefix <- conv_to_valid_obj_name(filename_prefix, max_width = dots$max_width_obj)
    filename_prefix <- stringi::stri_c(filename_prefix, collapse = "_", ignore_null = TRUE)
    if(rlang::is_string(mesos_group)) filename_prefix <- stringi::stri_c(filename_prefix, "_", mesos_group)

    # if(nrow(section_key)>1) cli::cli_warn("Something weird going on in grouping.")

    # filename_prefix <- stringi::stri_c(ignore_null=TRUE, list_valid_obj_name(filename_prefix, max_width = dots$max_width_obj),
    #                            if(rlang::is_string(mesos_group)) "_", mesos_group)



    y_col_names <- unique(chapter_overview_section$.variable_name)
    y_col_pos <- match(y_col_names, colnames(data))



    variable_prefix <-
      if(any(names(section_key) == ".variable_name_prefix") &&
         dplyr::n_distinct(section_key$.variable_name_prefix)==1) unique(section_key$.variable_name_prefix)


    if(stringi::stri_detect(element_name, regex="^uni_.*")) {

      # filename_prefix <- obj_name
      filepath_rel_rds <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".rds"))
      filepath_rel_png <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".png"))
      filepath_rel_xlsx <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))
      filepath_rel_txt <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".txt"))
      filepath_rel_docx <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".docx"))
      filepath_abs_rds <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".rds"))
      filepath_abs_png <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".png"))
      filepath_abs_xlsx <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))
      filepath_abs_txt <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".txt"))
      filepath_abs_docx <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".docx"))

      plot_height <- estimate_plot_height(y_col_pos = y_col_pos,
                                          vertical = dots$vertical,
                                          .variable_label_prefix = chapter_overview_section$.variable_label_prefix,
                                          x_axis_label_width = dots$x_axis_label_width,
                                          data = data,
                                          showNA = dots$showNA,
                                          plot_height_multiplier = dots$plot_height_multiplier,
                                          plot_height_fixed_constant = dots$plot_height_fixed_constant,
                                          plot_height_max = dots$plot_height_max,
                                          plot_height_min = dots$plot_height_min,
                                          vertical_height = dots$vertical_height)



      ######################################################################

      if(element_name == "uni_cat_text" &&
         all(chapter_overview_section$.variable_type == "fct")) {
        out <-
          rlang::exec(
            embed_cat_text_html,
            data = data,
            dep = y_col_pos,
            mesos_group = mesos_group,
            !!!dots)
        saveRDS(out, file = filepath_abs_rds)
        writeLines(text = stringi::stri_c(ignore_null=TRUE, out, collapse=""),
                   con = filepath_abs_txt)
      }

      ######################################################################

      if(element_name == "uni_chr_table" &&
         all(chapter_overview_section$.variable_type == "chr") &&
         length(y_col_pos) == 1) {
        out <-
          rlang::exec(
            embed_chr_table_html,
            data = data,
            dep = y_col_pos,
            mesos_group = mesos_group,
            !!!dots)
        saveRDS(out, file = filepath_abs_rds)
        writexl::write_xlsx(x=out, path = filepath_abs_xlsx)
      }

      ######################################################################


      if(element_name == "uni_cat_prop_plot" &&
         all(chapter_overview_section$.variable_type == "fct")) {
        out_docx <-
          rlang::exec(
            embed_cat_prop_plot_docx,
            data = data,
            dep = y_col_pos,
            mesos_group = mesos_group,
            !!!dots)
        print(out_docx, target = filepath_abs_docx)

        out_html <-
          rlang::exec(
            embed_cat_prop_plot,
            data = data,
            dep = y_col_pos,
            mesos_group = mesos_group,
            html_interactive = TRUE,
            !!!dots)
        ggplot2::ggsave(plot = out_html,
                        filename = filepath_abs_png,
                        scale = dots$png_scale,
                        width = dots$png_width,
                        height = dots$png_height,
                        units = "cm", dpi = "retina")
        writexl::write_xlsx(x = out_html$data, filepath_abs_xlsx)
        saveRDS(out_html, file = filepath_abs_rds)

        # out_pdf <-
        #   rlang::exec(
        #     embed_cat_prop_plot,
        #     data = data,
        #     dep = y_col_pos,
        #     translations = dots$translations,
        #     html_interactive = FALSE,
        #     !!!dots)
        # ggplot2::ggsave(plot = out_pdf, filename = filepath_abs_png,
        #                 scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
        #                 units = "cm", dpi = "retina")

        return(
          stringi::stri_c(ignore_null=TRUE,
            insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                              index = obj_name,
                              variable_prefix = variable_prefix,
                              mesos_group = mesos_group,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              add_text = FALSE,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              translations = dots$translations,
                              caption = attr(out_html, "saros_caption")),
            insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                              index = obj_name,
                              variable_prefix = variable_prefix,
                              mesos_group = mesos_group,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              translations = dots$translations,
                              caption = attr(out_html, "saros_caption")), sep="\n"))
      }
      ######################################################################

      if(element_name == "uni_cat_freq_plot" &&
         all(chapter_overview_section$.variable_type == "fct")) {
        out_docx <-
          rlang::exec(
            embed_cat_freq_plot_docx,
            data = data,
            dep = y_col_pos,
            mesos_group = mesos_group,
            !!!dots)
        print(out_docx, target = filepath_abs_docx)

        out_html <-
          rlang::exec(
            embed_cat_freq_plot,
            data = data,
            dep = y_col_pos,
            mesos_group = mesos_group,
            html_interactive = TRUE,
            !!!dots)
        ggplot2::ggsave(plot = out_html,
                        filename = filepath_abs_png,
                        scale = dots$png_scale,
                        width = dots$png_width,
                        height = dots$png_height,
                        units = "cm", dpi = "retina")
        saveRDS(out_html, file = filepath_abs_rds)


        return(
          stringi::stri_c(ignore_null=TRUE,
            insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                              index = obj_name,
                              variable_prefix = variable_prefix,
                              mesos_group = mesos_group,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              add_text = FALSE,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              translations = dots$translations,
                              caption = attr(out_html, "saros_caption")),
            insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                              index = obj_name,
                              variable_prefix = variable_prefix,
                              mesos_group = mesos_group,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              translations = dots$translations,
                              caption = attr(out_html, "saros_caption")), sep="\n"))
      }


      ######################################################################


      if(element_name == "uni_cat_table" &&
         all(chapter_overview_section$.variable_type == "fct")) {
        out <-
          rlang::exec(
            embed_cat_table,
            data = data,
            dep = y_col_pos,
            mesos_group = mesos_group,
            !!!dots)
        saveRDS(out, file = filepath_abs_rds)
        writexl::write_xlsx(x=out, path = filepath_abs_xlsx)
      }


      if(stringi::stri_detect(element_name, fixed = "uni_sigtest") &&
         dplyr::n_distinct(unique(chapter_overview_section$.variable_type)) == 1) {
        out <-
          rlang::exec(
            embed_uni_sigtest,
            data = data,
            dep = y_col_pos,
            .variable_type = unique(chapter_overview_section$.variable_type),
            mesos_group = mesos_group,
            !!!dots)
        saveRDS(out, file = filepath_abs_rds)
        writexl::write_xlsx(x=out, path = filepath_abs_xlsx)

      }


      if(exists("out")) {
          out <-
            insert_obj_in_qmd(element_name = element_name,
                              index = obj_name,
                              variable_prefix = variable_prefix,
                              mesos_group = mesos_group,
                              filepath_txt = filepath_abs_rds,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              translations = dots$translations,
                              caption = attr(out, "saros_caption"))
        return(out)
      }
    }



    ######################################################################

    if(all(chapter_overview_section$.variable_role != "indep") &&
       stringi::stri_detect(str = element_name, regex="^bi_.*") &&
       !rlang::is_null(chapter_overview_section$indep_cols_df) &&
       !rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) &&
       nrow(chapter_overview_section$indep_cols_df[[1]])>0 &&
       compare_many(chapter_overview_section$indep_cols_df)) {

      indep_df <- chapter_overview_section$indep_cols_df[[1]]

      if(inherits(indep_df, what = "data.frame")) {

        name_indep <-
          stats::setNames(unique(indep_df$.variable_name),
                   nm = stringi::stri_c(ignore_null=TRUE, obj_name, "_BY_", unique(indep_df$.variable_name)))


        if(stringi::stri_detect(str = element_name, fixed = "bi_sigtest")) {

          filename_prefix <- stringi::stri_c(ignore_null=TRUE, filename_prefix, "_BY_ALL_INDEP")
          filepath_rel_rds <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".rds"))
          filepath_rel_xlsx <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))
          filepath_abs_rds <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".rds"))
          filepath_abs_xlsx <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))


        out <-
        lapply(X = seq_along(name_indep), FUN = function(i) {
          .x <- name_indep[[i]]
          .y <- names(name_indep)[[i]]

          # Early check whether x and y are the same, which saros cannot handle
          if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return("")

          indep_pos <- match(.x, data_cols)

          indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
          indep_type <- indep_type$.variable_type

          ##############################################################################
          if(dplyr::n_distinct(chapter_overview_section$.variable_type) == 1 &&
             dplyr::n_distinct(indep_type) == 1) {
            return(
              rlang::exec(
                embed_bi_sigtest,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                .variable_type = unique(chapter_overview_section$.variable_type),
                indep_type = unique(indep_type),
                mesos_group = mesos_group,
                !!!dots)
            )
          }
        }) %>%
          dplyr::bind_rows()

        if(nrow(out)>0) {
          writexl::write_xlsx(x=out, path = filepath_abs_xlsx)
          saveRDS(out, file = filepath_abs_rds)
          return(
            insert_obj_in_qmd(element_name = element_name,
                              index = filename_prefix,
                              mesos_group = mesos_group,
                              filepath_txt = filepath_abs_rds,
                              filepath = filepath_rel_rds,
                              figure_height = plot_height,
                              max_width_obj = dots$max_width_obj,
                              max_width_file = dots$max_width_file,
                              translations = dots$translations,
                              caption = attr(out, "saros_caption")))
          }
        } else {


          unlist(
          lapply(X = seq_along(name_indep),
                 FUN = function(i) {
            .x <- name_indep[[i]]
            .y <- names(name_indep)[[i]]
          if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return("")

          indep_pos <- match(.x, data_cols)

          indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
          indep_type <- indep_type$.variable_type

          filename_prefix <- stringi::stri_c(ignore_null=TRUE, .y, collapse = "_")
          filepath_rel_rds <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".rds"))
          filepath_rel_png <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".png"))
          filepath_rel_xlsx <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))
          filepath_rel_txt <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".txt"))
          filepath_rel_docx <- fs::path(element_folderpath_relative, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".docx"))
          filepath_abs_rds <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".rds"))
          filepath_abs_xlsx <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))
          filepath_abs_png <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".png"))
          filepath_abs_txt <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".txt"))
          filepath_abs_docx <- fs::path(element_folderpath_absolute, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".docx"))

          plot_height <- estimate_plot_height(y_col_pos = y_col_pos,
                                              x_cols = indep_pos,
                                              vertical = dots$vertical,
                                              .variable_label_prefix = chapter_overview_section$.variable_label_prefix,
                                              x_axis_label_width = dots$x_axis_label_width,
                                              data = data,
                                              showNA = dots$showNA,
                                              plot_height_multiplier = dots$plot_height_multiplier,
                                              plot_height_fixed_constant = dots$plot_height_fixed_constant,
                                              plot_height_max = dots$plot_height_max,
                                              plot_height_min = dots$plot_height_min,
                                              vertical_height = dots$vertical_height)




          ##############################################################


          # if(element_name == "bi_catcat_text" &&
          #    all(chapter_overview_section$.variable_type == "fct") &&
          #    all(indep_type == "fct")) {
          #   out <-
          #     rlang::exec(
          #       embed_cat_text_html,
          #       data = data,
          #       dep = y_col_pos,
          #       indep = indep_pos,
          #       !!!dots)
          #
          # }



          if(element_name == "bi_catcat_prop_plot" &&
             all(chapter_overview_section$.variable_type == "fct") &&
             all(indep_type == "fct")) {

            out_docx <-
              rlang::exec(
                embed_cat_prop_plot_docx,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                !!!dots)
            print(out_docx, target = filepath_abs_docx)

            out_html <-
              rlang::exec(
                embed_cat_prop_plot,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                html_interactive = TRUE,
                !!!dots)
            ggplot2::ggsave(plot = out_html,
                            filename = filepath_abs_png,
                            scale = dots$png_scale,
                            width = dots$png_width,
                            height = dots$png_height,
                            units = "cm", dpi = "retina")
            saveRDS(out_html, file = filepath_abs_rds)

            return(
              stringi::stri_c(ignore_null=TRUE,
                insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                                  index = filename_prefix,
                                  mesos_group = mesos_group,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  add_text = FALSE,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")),
                insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                                  index = filename_prefix,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")), sep="\n"))


          }

          if(element_name == "bi_catcat_freq_plot" &&
             all(chapter_overview_section$.variable_type == "fct") &&
             all(indep_type == "fct")) {
            out_docx <-
              rlang::exec(
                embed_cat_freq_plot_docx,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                !!!dots)
            print(out_docx, target = filepath_abs_docx)

            out_html <-
              rlang::exec(
                embed_cat_freq_plot,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                html_interactive = TRUE,
                !!!dots)
            ggplot2::ggsave(plot = out_html,
                            filename = filepath_abs_png,
                            scale = dots$png_scale,
                            width = dots$png_width,
                            height = dots$png_height,
                            units = "cm", dpi = "retina")
            saveRDS(out_html, file = filepath_abs_rds)

            return(
              stringi::stri_c(ignore_null=TRUE,
                insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                                  index = filename_prefix,
                                  mesos_group = mesos_group,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  add_text = FALSE,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")),
                insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                                  index = filename_prefix,
                                  mesos_group = mesos_group,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")), sep="\n"))
          }



          if(element_name == "bi_catcat_prop_plot2" &&
             all(chapter_overview_section$.variable_type == "fct") &&
             all(indep_type == "fct")) {

            out_docx <-
              rlang::exec(
                embed_cat_prop_plot_docx,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                inverse = TRUE,
                !!!dots)
            print(out_docx, target = filepath_abs_docx)

            out_html <-
              rlang::exec(
                embed_cat_prop_plot,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                html_interactive = TRUE,
                inverse = TRUE,
                !!!dots)
            ggplot2::ggsave(plot = out_html,
                            filename = filepath_abs_png,
                            scale = dots$png_scale,
                            width = dots$png_width,
                            height = dots$png_height,
                            units = "cm", dpi = "retina")
            saveRDS(out_html, file = filepath_abs_rds)

            return(
              stringi::stri_c(ignore_null=TRUE,
                insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                                  index = filename_prefix,
                                  mesos_group = mesos_group,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  add_text = FALSE,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")),
                insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                                  index = filename_prefix,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")), sep="\n"))


          }

          if(element_name == "bi_catcat_freq_plot2" &&
             all(chapter_overview_section$.variable_type == "fct") &&
             all(indep_type == "fct")) {
            out_docx <-
              rlang::exec(
                embed_cat_freq_plot_docx,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                inverse = TRUE,
                !!!dots)
            print(out_docx, target = filepath_abs_docx)

            out_html <-
              rlang::exec(
                embed_cat_freq_plot,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                html_interactive = TRUE,
                inverse = TRUE,
                !!!dots)
            ggplot2::ggsave(plot = out_html,
                            filename = filepath_abs_png,
                            scale = dots$png_scale,
                            width = dots$png_width,
                            height = dots$png_height,
                            units = "cm", dpi = "retina")
            saveRDS(out_html, file = filepath_abs_rds)

            return(
              stringi::stri_c(ignore_null=TRUE,
                insert_obj_in_qmd(element_name = paste0(element_name, "_html"),
                                  index = filename_prefix,
                                  mesos_group = mesos_group,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  add_text = FALSE,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")),
                insert_obj_in_qmd(element_name = paste0(element_name, "_pdf"),
                                  index = filename_prefix,
                                  mesos_group = mesos_group,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out_html, "saros_caption")), sep="\n"))
          }


          if(element_name == "bi_catcat_table" &&
             all(chapter_overview_section$.variable_type == "fct") &&
             all(indep_type == "fct")) {

            out <-
              rlang::exec(
                embed_cat_table,
                data = data,
                dep = y_col_pos,
                indep = indep_pos,
                mesos_group = mesos_group,
                !!!dots)
            writexl::write_xlsx(x=out, path = filepath_abs_xlsx)
            saveRDS(out, file = filepath_abs_rds)
          }


          if(exists("out")) {


              return(
                insert_obj_in_qmd(element_name = element_name,
                                  index = filename_prefix,
                                  mesos_group = mesos_group,
                                  filepath_txt = filepath_abs_rds,
                                  filepath = filepath_rel_rds,
                                  figure_height = plot_height,
                                  max_width_obj = dots$max_width_obj,
                                  max_width_file = dots$max_width_file,
                                  translations = dots$translations,
                                  caption = attr(out, "saros_caption")))
          } else ""
        })) %>% stringi::stri_c(ignore_null=TRUE, collapse = "\n")
        }
      }
    }
  }

