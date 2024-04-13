#' @export
prepare_chunk.uni_cat_prop_plot <-
  function(element_name,
           chapter_overview_section,
           data,
           mesos_group=NULL,
           filepaths,
           obj_name,
           colour_palette,
           plot_height=15,
           ...) {

    dots <- rlang::list2(...)

    if(!all(chapter_overview_section$.variable_type_dep %in% c("fct", "ord")) ||
       !all(is.na(as.character(chapter_overview_section$.variable_name_indep))) ||
       (is_colour(dots$colour_2nd_binary_cat) && # To avoid both types of plots
        length(unique(unlist(data[, unique(as.character(chapter_overview_section$.variable_name_dep)), drop=FALSE]))) <= 3)) return()

    if(all(chapter_overview_section$.element_name == "uni_cat_prop_plot")) {
        embed_cat_plot_docx <- embed_cat_prop_plot_docx
        embed_cat_plot <- embed_cat_prop_plot
        element_name_snippet <- "uni_cat_prop_plot_html"
    } else {
      embed_cat_plot_docx <- embed_cat_freq_plot_docx
      embed_cat_plot <- embed_cat_freq_plot
      element_name_snippet <- "uni_cat_freq_plot_html"

      }

    out_docx <-
      rlang::exec(
        embed_cat_plot_docx,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        plot_height = plot_height,
        !!!dots)
    print(out_docx, target = filepaths$abs$docx)

    out_html <-
      rlang::exec(
        embed_cat_plot,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        html_interactive = TRUE,
        !!!dots)
    # ggplot2::ggsave(plot = out_html,
    #                 filename = filepaths$abs$png,
    #                 scale = dots$png_scale,
    #                 width = dots$png_width,
    #                 height = dots$png_height,
    #                 units = "cm", dpi = "retina")
    tabular_write(object = out_html$data, path = filepaths$abs[[dots$tabular_format]], format = dots$tabular_format)
    serialize_write(out_html, path = filepaths$abs[[dots$serialized_format]], format = dots$serialized_format)


    out <-
      c(create_code_cell(element_name = element_name_snippet,
                        index = obj_name,
                        mesos_group = mesos_group,
                        filepath = filepaths$rel[[dots$serialized_format]],
                        figure_height = plot_height,
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        serialized_format = dots$serialized_format,
                        tabular_format = dots$tabular_format,
                        function_call_prefix = 'ggiraph::girafe(ggobj = ',
                        function_call_suffix = ')',
                        translations = dots$translations,
                        caption = attr(out_html, "saros_caption")))
    stringi::stri_c(out, collapse="\n", ignore_null=TRUE)

  }

#' @export
prepare_chunk.uni_cat_freq_plot <- prepare_chunk.uni_cat_prop_plot

