add_section_heading_line <- function(grouped_data,
                                     level,
                                     chapter_overview,
                                     value) {

  # Make exception to heading construction to ensure always pretty heading names
  if(names(grouped_data)[level] == ".variable_name_dep") {
    heading <- chapter_overview[chapter_overview[[".variable_name_dep"]] == value,
                                ".variable_label_suffix_dep"][[1]]
  } else heading <- value

  stringi::stri_c(strrep("#", times = level), " ", heading,
                    "{#sec-", conv_to_valid_obj_name(value), "_",
                    stringi::stri_c(sample(0:9, size=2, replace = TRUE), ignore_null=TRUE, collapse=""),
                    "}\n",
                    ignore_null=TRUE)

}
