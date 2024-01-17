prepare_chunk.uni_cat_table <-
  function(chapter_overview_section,
           data,
           mesos_group=NULL,
           filepaths,
           obj_name,
           ...) {

    dots <- rlang::list2(...)

    if(!all(chapter_overview_section$.variable_type_dep %in% c("fct", "ord")) ||
       !all(is.na(as.character(chapter_overview_section$.variable_name_indep)))) return()

    out <-
      rlang::exec(
        embed_cat_table,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        mesos_group = mesos_group,
        !!!dots)
    serialize_write(out, path = filepaths$abs[[serialized_format]], format = dots$serialized_format)
    writexl::write_xlsx(x = out, path = filepaths$abs$xlsx)
    insert_obj_in_qmd(element_name = "uni_cat_table",
                      index = obj_name,
                      mesos_group = mesos_group,
                      filepath_txt = filepaths$abs$rds,
                      filepath = filepaths$rel$rds,
                      max_width_obj = dots$max_width_obj,
                      max_width_file = dots$max_width_file,
                      serialized_format = dots$serialized_format,
                      translations = dots$translations,
                      caption = attr(out, "saros_caption"))
  }
