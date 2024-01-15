#' @export
prepare_chunk.uni_cat_text <-
  function(chapter_overview_section,
           data,
           mesos_group,
           filepaths,
           obj_name,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2(...)

    if(!all(chapter_overview_section$.variable_type_dep %in% c("fct", "ord")) ||
       !all(is.na(as.character(chapter_overview_section$.variable_name_indep)))) return()

    out <-
      rlang::exec(
        embed_cat_text_html,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        mesos_group = mesos_group,
        !!!dots)
    out <- unlist(out)
    qs::qsave(out, file = filepaths$abs$rds)
    writeLines(text = out, con = filepaths$abs$txt)
    insert_obj_in_qmd(element_name = "uni_cat_text",
                      index = obj_name,
                      mesos_group = mesos_group,
                      filepath_txt = filepaths$abs$rds,
                      filepath = filepaths$rel$rds,
                      max_width_obj = dots$max_width_obj,
                      max_width_file = dots$max_width_file,
                      translations = dots$translations,
                      caption = attr(out, "saros_caption"))

  }
