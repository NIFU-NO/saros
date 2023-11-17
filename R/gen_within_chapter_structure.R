#' @keywords internal
#' @return String
#'
#'

# Heading added if not at innermost-level, or if inner-most level and rlang::is_string(output)
# dots$hide_chr_for_others if mesos_group and chr
#
gen_within_chapter_structure <-
  function(chapter_overview,
           data,
           mesos_group = NULL,
           chapter_folderpath_absolute,
           chapter_foldername,
           ...,
           call = rlang::caller_env()) {


    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)


    grouping_structure <- colnames(attr(chapter_overview, "groups"))
    grouping_structure <- grouping_structure[grouping_structure != ".rows"]

    heading_registry <- as.list(grouping_structure)
    created_headings <- lapply(seq_along(grouping_structure), function(x) list())
    out <- NULL

    for(group_index in seq_len(max(dplyr::n_groups(chapter_overview)))) {


      chapter_overview_section <-
        chapter_overview[dplyr::group_indices(chapter_overview) == group_index, , drop=FALSE]
      chapter_overview_section <- droplevels(chapter_overview_section)

      new_headings <- NULL

      for(level in seq_along(grouping_structure)) {
        unique_values <- unique(chapter_overview_section[[grouping_structure[level]]])

        for(value in unique_values) {
          heading_key <- paste(grouping_structure[1:level], collapse = "_")

        }
        if(all(heading_registry[[grouping_structure[level]]] != cur_section)) {

          heading_registry[[grouping_structure[level]]] <- c(heading_registry[[grouping_structure[level]]], cur_section)

          heading <- gen_heading_line(group=grouping_structure[level],
                                      cur_section = cur_section,
                                      chapter_overview_section = chapter_overview_section)

          new_headings <- c(new_headings, setNames(cur_section, nm = grouping_structure[level]))
        } # If already in registry, continue checking lower section level
      }



      if(rlang::is_true(dots$mesos_report) &&
         rlang::is_string(dots$mesos_var) &&
         rlang::is_string(mesos_group)) {

        data_for_all <-
          vctrs::vec_slice(data,
                           # is.na(data[[dots$mesos_var]]) |
                           data[[dots$mesos_var]] != mesos_group)

        data_for_mesos <-
          vctrs::vec_slice(data,
                           !is.na(data[[dots$mesos_var]]) &
                             data[[dots$mesos_var]] == mesos_group)

      } else {
        data_for_all <- data
        data_for_mesos <- data.frame()
      }


      if(nrow(chapter_overview_section) > 0 && nrow(data_for_all) > 0) {
        qmd_snippet_all <-
        rlang::exec(
          gen_element_and_qmd_snippet2,
          chapter_overview_section = chapter_overview_section,
          data = data_for_all,
          mesos_group = if(rlang::is_string(dots$mesos_var)) dots$translations$mesos_label_all_others,
          grouping_structure = grouping_structure,
          chapter_folderpath_absolute = chapter_folderpath_absolute,
          chapter_foldername = chapter_foldername,
          !!!dots
        )
      } else qmd_snippet_all <- NULL

      if(nrow(chapter_overview_section) > 0 && nrow(data_for_mesos) > 0) {
          qmd_snippet_mesos <-
            rlang::exec(
              gen_element_and_qmd_snippet2,
              chapter_overview_section = chapter_overview_section,
              data = data_for_mesos,
              mesos_var = dots$mesos_var,
              mesos_group = mesos_group,
              grouping_structure = grouping_structure,
              chapter_folderpath_absolute = chapter_folderpath_absolute,
              chapter_foldername = chapter_foldername,
              !!!dots
            )
        } else qmd_snippet_mesos <- NULL

      added <-
        insert_qmd_tablet_mesos_order(element_name = unique(chapter_overview_section$.element_name),
                                      qmd_snippet = qmd_snippet,
                                      qmd_snippet_mesos = qmd_snippet_mesos,
                                      mesos_report = dots$mesos_report,
                                      mesos_var = dots$mesos_var,
                                      mesos_group = mesos_group,
                                      panel_tabset_mesos = dots$panel_tabset_mesos,
                                      mesos_first = dots$mesos_first,
                                      translations = dots$translations)
      headings <- gen_heading_line(new_headings = new_headings,
                                   added = added)
      out <- c(out, added)

    }
}



