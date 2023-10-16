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

    common_data_type <- get_common_data_type(data, col_pos = y_col_pos)
    common_levels <- get_common_levels(data, col_pos = y_col_pos)

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
    qs::qsave(out_html, file = filepaths$abs$rds)


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
