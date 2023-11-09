make_filename_prefix <- function(
    grouping_structure,
    chapter_overview_section,
    max_width_obj,
    mesos_group,
    indep_sep_string = "_BY_",
    mesos_sep_string = "_FOR_"
) {

  # browser()
  grouping_structure[grouping_structure %in%
                        c(".variable_label_prefix_dep", ".variable_label_suffix_dep")] <- ".variable_name_dep"
  grouping_structure <- unique(grouping_structure)
  # grouping_structure <- grouping_structure[!grouping_structure %in% c(".element_name")]

  filename_prefix <- chapter_overview_section
  filename_prefix <- dplyr::ungroup(filename_prefix)
  filename_prefix <- dplyr::distinct(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  filename_prefix <- dplyr::arrange(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  filename_prefix <- dplyr::group_by(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  filename_prefix <- lapply(filename_prefix, function(x) get_common_name(x=unique(as.character(x))))
  filename_prefix <- filename_prefix[order(lengths(filename_prefix))]
  filename_prefix <- unlist(filename_prefix)
  filename_prefix <- unname(filename_prefix)
  filename_prefix <- stringi::stri_remove_empty_na(filename_prefix)
  filename_prefix <- stringi::stri_sub(str = filename_prefix, from = 1, to = max_width_obj,
                                       use_matrix = FALSE, ignore_negative_length = TRUE)
  filename_prefix <- stringi::stri_c(filename_prefix, collapse = "_", ignore_null = TRUE)


  obj_name_mesos <-
  if(rlang::is_string(mesos_group) && !is.na(mesos_group)) {
    stringi::stri_c(mesos_sep_string, mesos_group, ignore_null = TRUE)
  }


  random_id <- stringi::stri_c(sample(0:9, size=2, replace=TRUE), collapse="")
  ####
  out <-
    stringi::stri_c(filename_prefix, #obj_name_indep,
                  obj_name_mesos,
                  random_id,
                  ignore_null=TRUE)
  if(is.na(out)) browser()

  filename_sanitizer(out)
}
