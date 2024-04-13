#' @export
prepare_chunk.bi_catcat_table <-
  function(element_name,
           chapter_overview_section,
           data,
           mesos_group,
           filepaths,
           obj_name,
           ...) {

    dots <- rlang::list2(...)

    if(!all(chapter_overview_section$.variable_type_dep %in% c("fct", "ord")) ||
       !all(chapter_overview_section$.variable_type_indep %in% c("fct", "ord")) ||
       any(is.na(as.character(chapter_overview_section$.variable_name_indep)))) return()

    out <-
      rlang::exec(
        embed_cat_table,
        data = data,
        dep = unique(as.character(chapter_overview_section$.variable_name_dep)),
        indep = unique(as.character(chapter_overview_section$.variable_name_indep)),
        mesos_group = mesos_group,
        !!!dots)
    tabular_write(object = out, path = filepaths$abs[[dots$tabular_format]], format = dots$tabular_format)
    serialize_write(out, path = filepaths$abs[[dots$serialized_format]], format = dots$serialized_format)



    out <-
      create_code_cell(element_name = "bi_catcat_table",
                        index = obj_name,
                        mesos_group = mesos_group,
                        filepath_txt = filepaths$abs[[dots$serialized_format]],
                        filepath = filepaths$rel[[dots$serialized_format]],
                        max_width_obj = dots$max_width_obj,
                        max_width_file = dots$max_width_file,
                        serialized_format = dots$serialized_format,
                        tabular_format = dots$tabular_format,
                        translations = dots$translations,
                        caption = attr(out, "saros_caption"))
    # stringi::stri_c(out, ignore_null=TRUE, collapse = "\n")
  }
