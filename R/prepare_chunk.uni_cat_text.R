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
    qs::qsave(out, file = filepaths$abs$rds)
    writeLines(text = stringi::stri_c(out, ignore_null=TRUE, collapse=""),
               con = filepaths$abs$txt)
    insert_obj_in_qmd(element_name = "uni_cat_text",
                      index = obj_name,
                      variable_prefix = variable_prefix,
                      mesos_group = mesos_group,
                      filepath_txt = filepaths$abs$rds,
                      filepath = filepaths$rel$rds,
                      max_width_obj = dots$max_width_obj,
                      max_width_file = dots$max_width_file,
                      translations = dots$translations,
                      caption = attr(out, "saros_caption"))

  }
