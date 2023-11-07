
prepare_chunk.uni_chr_table <-
  function(chapter_overview_section,
           data,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           ...) {

    dots <- rlang::list2(...)

    if(dplyr::n_distinct(chapter_overview_section$.variable_name_dep, na.rm=TRUE) != 1 ||
       !all(chapter_overview_section$.variable_type_dep %in% c("chr")) ||
       !all(is.na(chapter_overview_section$.variable_name_indep))) return()

    out <-
      rlang::exec(
        embed_chr_table_html,
        data = data,
        dep = chapter_overview_section$.variable_name_dep,
        mesos_group = mesos_group,
        !!!dots)
    qs::qsave(out, file = filepaths$abs$rds)
    writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
    insert_obj_in_qmd(element_name = "uni_chr_table",
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
