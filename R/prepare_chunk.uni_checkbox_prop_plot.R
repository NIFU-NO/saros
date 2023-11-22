prepare_chunk.uni_checkbox_prop_plot <-
  function(chapter_overview_section,
           data,
           mesos_group=NULL,
           filepaths,
           obj_name,
           colour_palette,
           plot_height=15,
           ...) {

    dots <- rlang::list2(...)

    # Early quit
    if(!all(chapter_overview_section$.variable_type_dep %in% c("fct", "ord")) ||
       !all(is.na(as.character(chapter_overview_section$.variable_name_indep))) ||
       !is_colour(dots$colour_2nd_binary_cat) ||
       length(unique(unlist(data[, unique(as.character(chapter_overview_section$.variable_name_dep)), drop=FALSE]))) > 3) return()

    if(all(chapter_overview_section$.element_name == "uni_checkbox_prop_plot")) {
        embed_checkbox_plot_docx <- embed_checkbox_prop_plot_docx
        embed_checkbox_plot <- embed_checkbox_prop_plot
        element_name_snippet <- "uni_checkbox_prop_plot_html"
    } else {
      embed_checkbox_plot_docx <- embed_checkbox_freq_plot_docx
      embed_checkbox_plot <- embed_checkbox_freq_plot
      element_name_snippet <- "uni_checkbox_freq_plot_html"

      }

    out_docx <-
      rlang::exec(
        embed_checkbox_plot_docx,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        plot_height = plot_height,
        !!!dots)
    print(out_docx, target = filepaths$abs$docx)

    out_html <-
      rlang::exec(
        embed_checkbox_plot,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
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


    out <-
      c(insert_obj_in_qmd(element_name = element_name_snippet,
                        index = obj_name,
                        mesos_group = mesos_group,
                        filepath = filepaths$rel$rds,
                        figure_height = plot_height,
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        translations = dots$translations,
                        caption = attr(out_html, "saros_caption")))
    stringi::stri_c(out, collapse="\n", ignore_null=TRUE)

  }

prepare_chunk.uni_checkbox_freq_plot <- prepare_chunk.uni_checkbox_prop_plot

