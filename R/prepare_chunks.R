
prepare_chunk <- function(element_name, ...) {
  class(element_name) <- element_name
  UseMethod("prepare_chunk", element_name)
}

prepare_chunk.hline <- function(...) {
  "-----"
}

prepare_chunk.uni_cat_text <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord"))) return()

    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      rlang::exec(
        embed_cat_text_html,
        data = data,
        dep = y_col_pos,
        mesos_group = mesos_group,
        !!!dots)
    saveRDS(out, file = filepaths$abs$rds)
    writeLines(text = stringi::stri_c(out, ignore_null=TRUE, collapse=""),
               con = filepaths$abs$txt)
    insert_obj_in_qmd(element_name = "uni_cat_text",
                      index = obj_name,
                      variable_prefix = variable_prefix,
                      mesos_group = mesos_group,
                      filepath_txt = filepaths$abs$rds,
                      filepath = filepaths$rel$rds,
                      figure_height = plot_height,
                      max_width_obj = dots$max_width_obj,
                      max_width_file = dots$max_width_file,
                      translations = dots$translations,
                      caption = attr(out, "saros_caption"))

  }


prepare_chunk.uni_chr_table <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    if(!all(chapter_overview_section$.variable_type %in% c("chr")) ||
       length(y_col_pos) != 1) return()

    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      rlang::exec(
        embed_chr_table_html,
        data = data,
        dep = y_col_pos,
        mesos_group = mesos_group,
        !!!dots)
    saveRDS(out, file = filepaths$abs$rds)
    writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
    insert_obj_in_qmd(element_name = "uni_chr_table",
                      index = obj_name,
                      variable_prefix = variable_prefix,
                      mesos_group = mesos_group,
                      filepath_txt = filepaths$abs$rds,
                      filepath = filepaths$rel$rds,
                      figure_height = plot_height,
                      max_width_obj = dots$max_width_obj,
                      max_width_file = dots$max_width_file,
                      translations = dots$translations,
                      caption = attr(out, "saros_caption"))


  }


prepare_chunk.uni_cat_table <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord"))) return()

    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      rlang::exec(
        embed_cat_table,
        data = data,
        dep = y_col_pos,
        mesos_group = mesos_group,
        !!!dots)
    saveRDS(out, file = filepaths$abs$rds)
    writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
    insert_obj_in_qmd(element_name = "uni_cat_table",
                      index = obj_name,
                      variable_prefix = variable_prefix,
                      mesos_group = mesos_group,
                      filepath_txt = filepaths$abs$rds,
                      filepath = filepaths$rel$rds,
                      figure_height = plot_height,
                      max_width_obj = dots$max_width_obj,
                      max_width_file = dots$max_width_file,
                      translations = dots$translations,
                      caption = attr(out, "saros_caption"))


  }


prepare_chunk.uni_sigtest <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
       dplyr::n_distinct(unique(chapter_overview_section$.variable_type)) != 1) return()

    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      rlang::exec(
        embed_uni_sigtest,
        data = data,
        dep = y_col_pos,
        .variable_type = unique(chapter_overview_section$.variable_type),
        mesos_group = mesos_group,
        !!!dots)
    saveRDS(out, file = filepaths$abs$rds)
    writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
    insert_obj_in_qmd(element_name = "uni_sigtest",
                      index = obj_name,
                      variable_prefix = variable_prefix,
                      mesos_group = mesos_group,
                      filepath_txt = filepaths$abs$rds,
                      filepath = filepaths$rel$rds,
                      figure_height = plot_height,
                      max_width_obj = dots$max_width_obj,
                      max_width_file = dots$max_width_file,
                      translations = dots$translations,
                      caption = attr(out, "saros_caption"))

  }




prepare_chunk.uni_cat_prop_plot <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()


    if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord"))) return()

    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    common_data_type <- get_common_data_type(data, col_names = y_col_names)
    common_levels <- get_common_levels(data, col_names = y_col_names)

    colour_palette <- get_colour_set(
      x = common_levels,
      common_data_type = common_data_type,
      colour_palette_nominal = dots$colour_palette_nominal,
      colour_palette_ordinal = dots$colour_palette_ordinal,
      colour_na = dots$colour_na,
      colour_2nd_binary_cat = dots$colour_2nd_binary_cat)

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

    out_docx <-
      rlang::exec(
        embed_cat_prop_plot_docx,
        data = data,
        dep = y_col_pos,
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        !!!dots)
    print(out_docx, target = filepaths$abs$docx)

    out_html <-
      rlang::exec(
        embed_cat_prop_plot,
        data = data,
        dep = y_col_pos,
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        html_interactive = TRUE,
        !!!dots)
    ggplot2::ggsave(plot = out_html,
                    filename = filepaths$abs$png,
                    scale = dots$png_scale,
                    width = dots$png_width,
                    height = dots$png_height,
                    units = "cm", dpi = "retina")
    writexl::write_xlsx(x = out_html$data, filepaths$abs$xlsx)
    saveRDS(out_html, file = filepaths$abs$rds)

    # out_pdf <-
    #   rlang::exec(
    #     embed_cat_prop_plot,
    #     data = data,
    #     dep = y_col_pos,
    #     translations = dots$translations,
    #     html_interactive = FALSE,
    #     !!!dots)
    # ggplot2::ggsave(plot = out_pdf, filename = filepaths$abs$png,
    #                 scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
    #                 units = "cm", dpi = "retina")

    stringi::stri_c(
      insert_obj_in_qmd(element_name = "uni_cat_prop_plot_html",
                        index = obj_name,
                        variable_prefix = variable_prefix,
                        mesos_group = mesos_group,
                        filepath = filepaths$rel$rds,
                        figure_height = plot_height,
                        add_text = FALSE,
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        translations = dots$translations,
                        caption = attr(out_html, "saros_caption")),
      insert_obj_in_qmd(element_name = "uni_cat_prop_plot_pdf",
                        index = obj_name,
                        variable_prefix = variable_prefix,
                        mesos_group = mesos_group,
                        filepath = filepaths$rel$rds,
                        figure_height = plot_height,
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        translations = dots$translations,
                        caption = attr(out_html, "saros_caption")),
      sep="\n", ignore_null=TRUE)

  }



prepare_chunk.uni_cat_freq_plot <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord"))) return()

    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    common_data_type <- get_common_data_type(data, col_names = y_col_names)
    common_levels <- get_common_levels(data, col_names = y_col_names)

    colour_palette <- get_colour_set(
      x = common_levels,
      common_data_type = common_data_type,
      colour_palette_nominal = dots$colour_palette_nominal,
      colour_palette_ordinal = dots$colour_palette_ordinal,
      colour_na = dots$colour_na,
      colour_2nd_binary_cat = dots$colour_2nd_binary_cat)

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

    out_docx <-
      rlang::exec(
        embed_cat_freq_plot_docx,
        data = data,
        dep = y_col_pos,
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        !!!dots)
    print(out_docx, target = filepaths$abs$docx)

    out_html <-
      rlang::exec(
        embed_cat_freq_plot,
        data = data,
        dep = y_col_pos,
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        html_interactive = TRUE,
        !!!dots)
    ggplot2::ggsave(plot = out_html,
                    filename = filepaths$abs$png,
                    scale = dots$png_scale,
                    width = dots$png_width,
                    height = dots$png_height,
                    units = "cm", dpi = "retina")
    writexl::write_xlsx(x = out_html$data, filepaths$abs$xlsx)
    saveRDS(out_html, file = filepaths$abs$rds)

    # out_pdf <-
    #   rlang::exec(
    #     embed_cat_freq_plot,
    #     data = data,
    #     dep = y_col_pos,
    #     translations = dots$translations,
    #     html_interactive = FALSE,
    #     !!!dots)
    # ggplot2::ggsave(plot = out_pdf, filename = filepaths$abs$png,
    #                 scale = dots$png_scale, width = dots$png_width, height = dots$png_height,
    #                 units = "cm", dpi = "retina")

    stringi::stri_c(
      insert_obj_in_qmd(element_name = "uni_cat_freq_plot_html",
                        index = obj_name,
                        variable_prefix = variable_prefix,
                        mesos_group = mesos_group,
                        filepath = filepaths$rel$rds,
                        figure_height = plot_height,
                        add_text = FALSE,
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        translations = dots$translations,
                        caption = attr(out_html, "saros_caption")),
      insert_obj_in_qmd(element_name = "uni_cat_freq_plot_pdf",
                        index = obj_name,
                        variable_prefix = variable_prefix,
                        mesos_group = mesos_group,
                        filepath = filepaths$rel$rds,
                        figure_height = plot_height,
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        translations = dots$translations,
                        caption = attr(out_html, "saros_caption")),
      sep="\n", ignore_null=TRUE)

  }





############ BIVARIATE ##############


prepare_chunk.bi_sigtest <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           y_col_names,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)


    if(any(chapter_overview_section$.variable_role == "indep") ||
       rlang::is_null(chapter_overview_section$indep_cols_df) ||
       rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) ||
       nrow(chapter_overview_section$indep_cols_df[[1]]) == 0 ||
       !compare_many(chapter_overview_section$indep_cols_df) ||
       !inherits(chapter_overview_section$indep_cols_df[[1]], what = "data.frame")) return()

    indep_df <- chapter_overview_section$indep_cols_df[[1]]


    name_indep <-
      stats::setNames(unique(indep_df$.variable_name),
                      nm = stringi::stri_c(obj_name, "_BY_", unique(indep_df$.variable_name), ignore_null=TRUE))


    filename_prefix <- stringi::stri_c(filename_prefix, "_BY_ALL_INDEP", ignore_null=TRUE)
    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)


    out <-
      lapply(X = seq_along(name_indep), FUN = function(i) {
        .x <- name_indep[[i]]
        .y <- names(name_indep)[[i]]

        # Early check whether x and y are the same, which saros cannot handle
        if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return(data.frame())

        indep_pos <- match(.x, data_cols)

        indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
        indep_type <- indep_type$.variable_type

        ##############################################################################
        if(dplyr::n_distinct(chapter_overview_section$.variable_type) == 1 &&
           dplyr::n_distinct(indep_type) == 1) {

          rlang::exec(
            embed_bi_sigtest,
            data = data,
            dep = y_col_pos,
            indep = indep_pos,
            .variable_type = unique(chapter_overview_section$.variable_type),
            indep_type = unique(indep_type),
            mesos_group = mesos_group,
            !!!dots)
        }
      })

    out <- dplyr::bind_rows(out)

    if(nrow(out)>0) {
      writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
      saveRDS(out, file = filepaths$abs$rds)
      return(
        insert_obj_in_qmd(element_name = "bi_sigtest",
                          index = filename_prefix,
                          mesos_group = mesos_group,
                          filepath_txt = filepaths$abs$rds,
                          filepath = filepaths$rel$rds,
                          max_width_obj = dots$max_width_obj,
                          max_width_file = dots$max_width_file,
                          translations = dots$translations,
                          caption = attr(out, "saros_caption")))
    }
  }



##### BIVARIATE PLOTS ########
prepare_chunk.bi_catcat_prop_plot <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           y_col_names,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)

    if(any(chapter_overview_section$.variable_role == "indep") ||
       rlang::is_null(chapter_overview_section$indep_cols_df) ||
       rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) ||
       nrow(chapter_overview_section$indep_cols_df[[1]]) == 0 ||
       !compare_many(chapter_overview_section$indep_cols_df) ||
       !inherits(chapter_overview_section$indep_cols_df[[1]], what = "data.frame")) return()

    indep_df <- chapter_overview_section$indep_cols_df[[1]]


    name_indep <-
      stats::setNames(unique(indep_df$.variable_name),
                      nm = stringi::stri_c(obj_name, "_BY_", unique(indep_df$.variable_name), ignore_null=TRUE))


    filename_prefix <- stringi::stri_c(filename_prefix, "_BY_ALL_INDEP", ignore_null=TRUE)
    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      lapply(X = seq_along(name_indep),
             FUN = function(i) {
               .x <- name_indep[[i]]
               .y <- names(name_indep)[[i]]
               # print(name_indep[i])

               # If dep and indep are the same, or empty, return early.
               if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return()

               indep_pos <- match(.x, data_cols)

               indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
               indep_type <- indep_type$.variable_type

               if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
                  !all(indep_type %in% c("fct", "ord"))) return()

               filename_prefix <- stringi::stri_c(filename_prefix, "BY", .x, ignore_null=TRUE, sep = "_")
               filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                                element_folderpath_absolute = element_folderpath_absolute,
                                                filename_prefix = filename_prefix)

               common_data_type <- get_common_data_type(data, col_names = y_col_names)
               common_levels <- get_common_levels(data, col_names = y_col_names)

               colour_palette <- get_colour_set(
                 x = common_levels,
                 common_data_type = common_data_type,
                 colour_palette_nominal = dots$colour_palette_nominal,
                 colour_palette_ordinal = dots$colour_palette_ordinal,
                 colour_na = dots$colour_na,
                 colour_2nd_binary_cat = dots$colour_2nd_binary_cat)

               plot_height <-
                 estimate_plot_height(y_col_pos = y_col_pos,
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


               out_docx <-
                 rlang::exec(
                   embed_cat_prop_plot_docx,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   !!!dots)
               print(out_docx, target = filepaths$abs$docx)

               out_html <-
                 rlang::exec(
                   embed_cat_prop_plot,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   html_interactive = TRUE,
                   !!!dots)
               ggplot2::ggsave(plot = out_html,
                               filename = filepaths$abs$png,
                               scale = dots$png_scale,
                               width = dots$png_width,
                               height = dots$png_height,
                               units = "cm", dpi = "retina")
               writexl::write_xlsx(x = out_html$data, path = filepaths$abs$xlsx)

               saveRDS(out_html, file = filepaths$abs$rds)

               out <-
                 c(insert_obj_in_qmd(element_name = "bi_catcat_prop_plot_html",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = FALSE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")),
                   insert_obj_in_qmd(element_name = "bi_catcat_prop_plot_pdf",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = TRUE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")))

               stringi::stri_c(out, sep="\n", ignore_null=TRUE)

             })
    out <- unlist(out)
    stringi::stri_c(out, ignore_null=TRUE, collapse = "\n")
  }


prepare_chunk.bi_catcat_freq_plot <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           y_col_names,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)

    if(any(chapter_overview_section$.variable_role == "indep") ||
       rlang::is_null(chapter_overview_section$indep_cols_df) ||
       rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) ||
       nrow(chapter_overview_section$indep_cols_df[[1]]) == 0 ||
       !compare_many(chapter_overview_section$indep_cols_df) ||
       !inherits(chapter_overview_section$indep_cols_df[[1]], what = "data.frame")) return()

    indep_df <- chapter_overview_section$indep_cols_df[[1]]


    name_indep <-
      stats::setNames(unique(indep_df$.variable_name),
                      nm = stringi::stri_c(obj_name, "_BY_", unique(indep_df$.variable_name), ignore_null=TRUE))


    filename_prefix <- stringi::stri_c(filename_prefix, "_BY_ALL_INDEP", ignore_null=TRUE)
    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      lapply(X = seq_along(name_indep),
             FUN = function(i) {
               .x <- name_indep[[i]]
               .y <- names(name_indep)[[i]]
               # print(name_indep[i])

               # If dep and indep are the same, or empty, return early.
               if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return()

               indep_pos <- match(.x, data_cols)

               indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
               indep_type <- indep_type$.variable_type

               if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
                  !all(indep_type %in% c("fct", "ord"))) return()

               filename_prefix <- stringi::stri_c(filename_prefix, "BY", .x, ignore_null=TRUE, sep = "_")
               filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                                element_folderpath_absolute = element_folderpath_absolute,
                                                filename_prefix = filename_prefix)

               common_data_type <- get_common_data_type(data, col_names = y_col_names)
               common_levels <- get_common_levels(data, col_names = y_col_names)

               colour_palette <- get_colour_set(
                 x = common_levels,
                 common_data_type = common_data_type,
                 colour_palette_nominal = dots$colour_palette_nominal,
                 colour_palette_ordinal = dots$colour_palette_ordinal,
                 colour_na = dots$colour_na,
                 colour_2nd_binary_cat = dots$colour_2nd_binary_cat)

               plot_height <-
                 estimate_plot_height(y_col_pos = y_col_pos,
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


               out_docx <-
                 rlang::exec(
                   embed_cat_freq_plot_docx,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   !!!dots)
               print(out_docx, target = filepaths$abs$docx)

               out_html <-
                 rlang::exec(
                   embed_cat_freq_plot,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   html_interactive = TRUE,
                   !!!dots)
               ggplot2::ggsave(plot = out_html,
                               filename = filepaths$abs$png,
                               scale = dots$png_scale,
                               width = dots$png_width,
                               height = dots$png_height,
                               units = "cm", dpi = "retina")
               writexl::write_xlsx(x = out_html$data, path = filepaths$abs$xlsx)

               saveRDS(out_html, file = filepaths$abs$rds)

               out <-
                 c(insert_obj_in_qmd(element_name = "bi_catcat_freq_plot_html",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = FALSE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")),
                   insert_obj_in_qmd(element_name = "bi_catcat_freq_plot_pdf",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = TRUE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")))

               stringi::stri_c(out, sep="\n", ignore_null=TRUE)

             })
    out <- unlist(out)
    stringi::stri_c(out, ignore_null=TRUE, collapse = "\n")
  }


##### BIVARIATE PLOTS ########
prepare_chunk.bi_catcat_prop_plot2 <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           y_col_names,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)

    if(any(chapter_overview_section$.variable_role == "indep") ||
       rlang::is_null(chapter_overview_section$indep_cols_df) ||
       rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) ||
       nrow(chapter_overview_section$indep_cols_df[[1]]) == 0 ||
       !compare_many(chapter_overview_section$indep_cols_df) ||
       !inherits(chapter_overview_section$indep_cols_df[[1]], what = "data.frame")) return()

    indep_df <- chapter_overview_section$indep_cols_df[[1]]


    name_indep <-
      stats::setNames(unique(indep_df$.variable_name),
                      nm = stringi::stri_c(obj_name, "_BY_", unique(indep_df$.variable_name), ignore_null=TRUE))


    filename_prefix <- stringi::stri_c(filename_prefix, "_BY_ALL_INDEP", ignore_null=TRUE)
    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      lapply(X = seq_along(name_indep),
             FUN = function(i) {
               .x <- name_indep[[i]]
               .y <- names(name_indep)[[i]]
               # print(name_indep[i])

               # If dep and indep are the same, or empty, return early.
               if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return()

               indep_pos <- match(.x, data_cols)

               indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
               indep_type <- indep_type$.variable_type

               if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
                  !all(indep_type %in% c("fct", "ord"))) return()

               filename_prefix <- stringi::stri_c(filename_prefix, "BY", .x, ignore_null=TRUE, sep = "_")
               filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                                element_folderpath_absolute = element_folderpath_absolute,
                                                filename_prefix = filename_prefix)

               common_data_type <- get_common_data_type(data, col_names = y_col_names)
               common_levels <- get_common_levels(data, col_names = y_col_names)

               colour_palette <- get_colour_set(
                 x = common_levels,
                 common_data_type = common_data_type,
                 colour_palette_nominal = dots$colour_palette_nominal,
                 colour_palette_ordinal = dots$colour_palette_ordinal,
                 colour_na = dots$colour_na,
                 colour_2nd_binary_cat = dots$colour_2nd_binary_cat)

               plot_height <-
                 estimate_plot_height(y_col_pos = y_col_pos,
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


               out_docx <-
                 rlang::exec(
                   embed_cat_prop_plot_docx,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   inverse = TRUE,
                   !!!dots)
               print(out_docx, target = filepaths$abs$docx)

               out_html <-
                 rlang::exec(
                   embed_cat_prop_plot,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   html_interactive = TRUE,
                   inverse = TRUE,
                   !!!dots)
               ggplot2::ggsave(plot = out_html,
                               filename = filepaths$abs$png,
                               scale = dots$png_scale,
                               width = dots$png_width,
                               height = dots$png_height,
                               units = "cm", dpi = "retina")
               writexl::write_xlsx(x = out_html$data, path = filepaths$abs$xlsx)

               saveRDS(out_html, file = filepaths$abs$rds)

               out <-
                 c(insert_obj_in_qmd(element_name = "bi_catcat_prop_plot_html",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = FALSE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")),
                   insert_obj_in_qmd(element_name = "bi_catcat_prop_plot_pdf",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = TRUE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")))

               stringi::stri_c(out, sep="\n", ignore_null=TRUE)

             })
    out <- unlist(out)
    stringi::stri_c(out, ignore_null=TRUE, collapse = "\n")
  }


#########
prepare_chunk.bi_catcat_freq_plot2 <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           y_col_names,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)

    if(any(chapter_overview_section$.variable_role == "indep") ||
       rlang::is_null(chapter_overview_section$indep_cols_df) ||
       rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) ||
       nrow(chapter_overview_section$indep_cols_df[[1]]) == 0 ||
       !compare_many(chapter_overview_section$indep_cols_df) ||
       !inherits(chapter_overview_section$indep_cols_df[[1]], what = "data.frame")) return()

    indep_df <- chapter_overview_section$indep_cols_df[[1]]


    name_indep <-
      stats::setNames(unique(indep_df$.variable_name),
                      nm = stringi::stri_c(obj_name, "_BY_", unique(indep_df$.variable_name), ignore_null=TRUE))


    filename_prefix <- stringi::stri_c(filename_prefix, "_BY_ALL_INDEP", ignore_null=TRUE)
    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      lapply(X = seq_along(name_indep),
             FUN = function(i) {
               .x <- name_indep[[i]]
               .y <- names(name_indep)[[i]]
               # print(name_indep[i])

               # If dep and indep are the same, or empty, return early.
               if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return()

               indep_pos <- match(.x, data_cols)

               indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
               indep_type <- indep_type$.variable_type

               if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
                  !all(indep_type %in% c("fct", "ord"))) return()

               filename_prefix <- stringi::stri_c(filename_prefix, "BY", .x, ignore_null=TRUE, sep = "_")
               filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                                element_folderpath_absolute = element_folderpath_absolute,
                                                filename_prefix = filename_prefix)

               common_data_type <- get_common_data_type(data, col_names = y_col_names)
               common_levels <- get_common_levels(data, col_names = y_col_names)

               colour_palette <- get_colour_set(
                 x = common_levels,
                 common_data_type = common_data_type,
                 colour_palette_nominal = dots$colour_palette_nominal,
                 colour_palette_ordinal = dots$colour_palette_ordinal,
                 colour_na = dots$colour_na,
                 colour_2nd_binary_cat = dots$colour_2nd_binary_cat)

               plot_height <-
                 estimate_plot_height(y_col_pos = y_col_pos,
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


               out_docx <-
                 rlang::exec(
                   embed_cat_freq_plot_docx,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   inverse = TRUE,
                   !!!dots)
               print(out_docx, target = filepaths$abs$docx)

               out_html <-
                 rlang::exec(
                   embed_cat_freq_plot,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   colour_palette = colour_palette,
                   mesos_group = mesos_group,
                   html_interactive = TRUE,
                   inverse = TRUE,
                   !!!dots)
               ggplot2::ggsave(plot = out_html,
                               filename = filepaths$abs$png,
                               scale = dots$png_scale,
                               width = dots$png_width,
                               height = dots$png_height,
                               units = "cm", dpi = "retina")
               writexl::write_xlsx(x = out_html$data, path = filepaths$abs$xlsx)

               saveRDS(out_html, file = filepaths$abs$rds)

               out <-
                 c(insert_obj_in_qmd(element_name = "bi_catcat_freq_plot_html",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = FALSE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")),
                   insert_obj_in_qmd(element_name = "bi_catcat_freq_plot_pdf",
                                     index = filename_prefix,
                                     mesos_group = mesos_group,
                                     filepath = filepaths$rel$rds,
                                     figure_height = plot_height,
                                     add_text = TRUE,
                                     max_width_obj = dots$max_width_obj,
                                     max_width_file = dots$max_width_file,
                                     translations = dots$translations,
                                     caption = attr(out_html, "saros_caption")))

               stringi::stri_c(out, sep="\n", ignore_null=TRUE)

             })
    out <- unlist(out)
    stringi::stri_c(out, ignore_null=TRUE, collapse = "\n")
  }

#########
prepare_chunk.bi_catcat_table <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           y_col_names,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)

    if(any(chapter_overview_section$.variable_role == "indep") ||
       rlang::is_null(chapter_overview_section$indep_cols_df) ||
       rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) ||
       nrow(chapter_overview_section$indep_cols_df[[1]]) == 0 ||
       !compare_many(chapter_overview_section$indep_cols_df) ||
       !inherits(chapter_overview_section$indep_cols_df[[1]], what = "data.frame")) return()

    indep_df <- chapter_overview_section$indep_cols_df[[1]]


    name_indep <-
      stats::setNames(unique(indep_df$.variable_name),
                      nm = stringi::stri_c(obj_name, "_BY_", unique(indep_df$.variable_name), ignore_null=TRUE))


    filename_prefix <- stringi::stri_c(filename_prefix, "_BY_ALL_INDEP", ignore_null=TRUE)
    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)

    out <-
      lapply(X = seq_along(name_indep),
             FUN = function(i) {
               .x <- name_indep[[i]]
               .y <- names(name_indep)[[i]]
               # print(name_indep[i])

               # If dep and indep are the same, or empty, return early.
               if(is.null(y_col_names) || is.null(.x) || any(y_col_names == .x)) return()

               indep_pos <- match(.x, data_cols)

               indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
               indep_type <- indep_type$.variable_type

               if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
                  !all(indep_type %in% c("fct", "ord"))) return()

               filename_prefix <- stringi::stri_c(filename_prefix, "BY", .x, ignore_null=TRUE, sep = "_")
               filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                                element_folderpath_absolute = element_folderpath_absolute,
                                                filename_prefix = filename_prefix)

               common_data_type <- get_common_data_type(data, col_names = y_col_names)
               common_levels <- get_common_levels(data, col_names = y_col_names)

               out <-
                 rlang::exec(
                   embed_cat_table,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   mesos_group = mesos_group,
                   !!!dots)
               writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
               saveRDS(out, file = filepaths$abs$rds)



               out <-
                 insert_obj_in_qmd(element_name = element_name,
                                   index = filename_prefix,
                                   mesos_group = mesos_group,
                                   filepath_txt = filepaths$abs$rds,
                                   filepath = filepaths$rel$rds,
                                   figure_height = plot_height,
                                   max_width_obj = dots$max_width_obj,
                                   max_width_file = dots$max_width_file,
                                   translations = dots$translations,
                                   caption = attr(out, "saros_caption"))


             })
    out <- unlist(out)
    stringi::stri_c(out, ignore_null=TRUE, collapse = "\n")
  }

