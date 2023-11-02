prepare_chunk.bi_catcat_prop_plot2 <-
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
               indep_pos <- match(.x, data_cols)
               .y <- names(name_indep)[[i]]
               # print(name_indep[i])

               # If dep and indep are the same, or empty, return early.
               if(is.null(y_col_pos) || is.null(.x) || any(y_col_pos == indep_pos)) return()



               indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
               indep_type <- indep_type$.variable_type

               if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
                  !all(indep_type %in% c("fct", "ord"))) return()

               filename_prefix <- stringi::stri_c(filename_prefix, "BY", .x, ignore_null=TRUE, sep = "_")
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
                 colour_2nd_binary_cat = dots$colour_2nd_binary_cat,
                 categories_treated_as_na = dots$categories_treated_as_na[dots$categories_treated_as_na %in% y_col_names])

               plot_height <-
                 estimate_plot_height(y_col_pos = y_col_pos,
                                      x_cols = indep_pos,
                                      vertical = dots$vertical,
                                      label_separator = dots$label_separator,
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

               qs::qsave(out_html, file = filepaths$abs$rds)

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
