make_filename_prefix <- function(
    grouping_structure,
    chapter_overview_section,
    max_width_obj,
    mesos_group
) {

  grouping_structure[grouping_structure %in%
                        c(".variable_label_prefix", ".variable_label_suffix")] <- ".variable_name"
  grouping_structure <- unique(grouping_structure)
  grouping_structure <- grouping_structure[!grouping_structure %in% c(".element_name")]


  # if(any(stringi::stri_detect_fixed(chapter_overview_section$.variable_name, "s_407_"))) browser()
  filename_prefix <- chapter_overview_section
  filename_prefix <- dplyr::ungroup(filename_prefix)
  filename_prefix <- dplyr::distinct(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  filename_prefix <- dplyr::arrange(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  filename_prefix <- dplyr::group_by(filename_prefix, dplyr::pick(tidyselect::all_of(grouping_structure)))
  filename_prefix <- lapply(filename_prefix, function(x) get_common_name(x=unique(x)))
  filename_prefix <- filename_prefix[order(lengths(filename_prefix))]
  filename_prefix <- unlist(filename_prefix)
  filename_prefix <- unname(filename_prefix)
  filename_prefix <-
  filename_prefix <- stringi::stri_sub(str = filename_prefix, from = 1, to = max_width_obj)


  filename_prefix <- stringi::stri_c(filename_prefix, collapse = "_", ignore_null = TRUE)
  if(rlang::is_string(mesos_group)) filename_prefix <- stringi::stri_c(filename_prefix, "_", mesos_group)

  filename_prefix
}
