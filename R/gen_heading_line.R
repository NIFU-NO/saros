gen_heading_line <- function(group,
                             cur_section,
                             chapter_overview_section = NULL,
                             added = NULL,
                             ignore_heading_for_group = NULL,
                             replace_heading_for_group = NULL) {




  if(!any(ignore_heading_for_group == group)) {

    # Make exception to heading construction to ensure always pretty heading names
    if(is.data.frame(chapter_overview_section)) {
      for(replace_i in seq_along(replace_heading_for_group)) {
        if(unname(replace_heading_for_group)[replace_i] == group) {
          row_selection <- chapter_overview_section[[unname(replace_heading_for_group)[replace_i]]] == cur_section
          cur_section <-
            chapter_overview_section[row_selection,
                                     names(replace_heading_for_group)[replace_i]][[1]]
        }
      }
    }
    level <- match(group, dplyr::group_vars(chapter_overview_section))

    stringi::stri_c(strrep("#", times = level), " ", cur_section,
                    "{#sec-", conv_to_valid_obj_name(cur_section), "_",
                    stringi::stri_c(sample(0:9, size=1, replace = TRUE), ignore_null=TRUE, collapse=""),
                    "}\n",
                    ignore_null=TRUE)
  }

}
