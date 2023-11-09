prepare_chunk.bi_catcat_prop_plot <-
  function(chapter_overview_section,
           data,
           mesos_group=NULL,
           filepaths,
           obj_name,
           colour_palette,
           plot_height=15,
           ...) {

    dots <- rlang::list2(...)

    if(!all(chapter_overview_section$.variable_type_dep %in% c("fct", "ord")) ||
       !all(chapter_overview_section$.variable_type_indep %in% c("fct", "ord")) ||
       any(is.na(as.character(chapter_overview_section$.variable_name_indep))) ||
       nrow(chapter_overview_section) == 0) return()

    # if(!compare_many(chapter_overview_section$indep_cols_df)) return()

    if(all(stringi::stri_detect_regex(chapter_overview_section$.element_name, "bi_catcat_prop2*_plot"))) {
      embed_cat_plot_docx <- embed_cat_prop_plot_docx
      embed_cat_plot <- embed_cat_prop_plot
      element_name <- "bi_catcat_prop_plot_html"

    } else if(all(stringi::stri_detect_regex(chapter_overview_section$.element_name, "bi_catcat_freq2*_plot"))) {
      embed_cat_plot_docx <- embed_cat_freq_plot_docx
      embed_cat_plot <- embed_cat_freq_plot
      element_name <- "bi_catcat_freq_plot_html"
    }
    if(all(as.character(chapter_overview_section$.element_name) %in% c("bi_catcat_prop2_plot", "bi_catcat_freq2_plot"))) {
      inverse <- TRUE
    } else inverse <- FALSE


    out_docx <-
      rlang::exec(
        embed_cat_plot_docx,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        indep = unique(as.character(chapter_overview_section$.variable_name_indep)),
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        inverse = inverse,
        !!!dots)
    print(out_docx, target = filepaths$abs$docx)

    out_html <-
      rlang::exec(
        embed_cat_plot,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        indep = unique(as.character(chapter_overview_section$.variable_name_indep)),
        colour_palette = colour_palette,
        mesos_group = mesos_group,
        inverse = inverse,
        html_interactive = TRUE,
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
      c(insert_obj_in_qmd(element_name = element_name,
                          index = obj_name,
                          mesos_group = mesos_group,
                          filepath = filepaths$rel$rds,
                          figure_height = plot_height,
                          add_text = TRUE,
                          max_width_obj = dots$max_width_obj,
                          max_width_file = dots$max_width_file,
                          translations = dots$translations,
                          caption = attr(out_html, "saros_caption")))

    stringi::stri_c(out, collapse="\n", ignore_null=TRUE)
  }


prepare_chunk.bi_catcat_freq_plot <- prepare_chunk.bi_catcat_prop_plot
prepare_chunk.bi_catcat_prop_plot2 <- prepare_chunk.bi_catcat_prop_plot
prepare_chunk.bi_catcat_freq_plot2 <- prepare_chunk.bi_catcat_prop_plot
