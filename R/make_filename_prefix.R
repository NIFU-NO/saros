make_filename_prefix <- function(
    grouping_structure,
    chapter_overview_section,
    max_width_obj,
    mesos_group,
    indep_sep_string = "_BY_",
    mesos_sep_string = "_FOR_"
) {

  grouping_structure[grouping_structure %in%
                        c(".variable_label_prefix_dep", ".variable_label_suffix_dep")] <- ".variable_name_dep"
  grouping_structure[grouping_structure %in%
                       c(".variable_label_prefix_indep", ".variable_label_suffix_indep")] <- ".variable_name_indep"
  grouping_structure <- unique(grouping_structure)

  prefix <- chapter_overview_section
  prefix <- dplyr::ungroup(prefix)
  prefix <- dplyr::distinct(prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  prefix <- dplyr::arrange(prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  prefix <- dplyr::group_by(prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  prefix <- lapply(prefix, function(x) get_common_name(x=unique(as.character(x))))
  prefix <- prefix[order(lengths(prefix))]
  prefix <- unlist(prefix)
  prefix <- unname(prefix)
  prefix <- stringi::stri_remove_empty_na(prefix)
  prefix <- stringi::stri_sub(str = prefix, from = 1, to = max_width_obj,
                                       use_matrix = FALSE, ignore_negative_length = TRUE)
  prefix <- stringi::stri_c(prefix, collapse = "_", ignore_null = TRUE)
  prefix <- stringi::stri_replace_first_regex(prefix, pattern = "^[[:punct:]]", replacement = "")


  obj_name_mesos <-
  if(rlang::is_string(mesos_group) && !is.na(mesos_group)) {
    stringi::stri_c(mesos_sep_string, mesos_group, ignore_null = TRUE)
  }


  random_id <- stringi::stri_c(sample(0:9, size=2, replace=TRUE), collapse="")
  ####
  out <-
    stringi::stri_c(prefix, #obj_name_indep,
                  obj_name_mesos,
                  random_id,
                  ignore_null=TRUE)
  if(is.na(out)) browser()

  filename_sanitizer(out) # No max_chars shortening
}
