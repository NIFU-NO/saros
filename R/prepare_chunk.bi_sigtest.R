#' @export
prepare_chunk.bi_sigtest <-
  function(element_name,
           chapter_overview_section,
           data,
           mesos_group=NULL,
           filepaths,
           obj_name,
           ...) {

    dots <- rlang::list2(...)

    if(!all(chapter_overview_section$.variable_type_dep %in% c("fct", "ord", "int", "dbl")) ||
       !all(chapter_overview_section$.variable_type_indep %in% c("fct", "ord", "int", "dbl")) ||
       any(is.na(as.character(chapter_overview_section$.variable_name_indep)))) return()

    out <-
      rlang::exec(
        embed_sigtest,
        data = data,
        chapter_overview = chapter_overview_section,
        mesos_group = mesos_group,
        !!!dots)

    if(nrow(out)>0) {
      tabular_write(object = out, path = filepaths$abs[[dots$tabular_format]], format = dots$tabular_format)
      serialize_write(out, path = filepaths$abs[[dots$serialized_format]], format = dots$serialized_format)
      insert_obj_in_qmd(element_name = "bi_sigtest",
                        index = obj_name,
                        mesos_group = mesos_group,
                        filepath_txt = filepaths$abs$rds,
                        filepath = filepaths$rel$rds,
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        serialized_format = dots$serialized_format,
                        tabular_format = dots$tabular_format,
                        translations = dots$translations,
                        caption = attr(out, "saros_caption"))
    } else "\n<!--# NO SIGTABLE TO SHOW -->\n"
  }
