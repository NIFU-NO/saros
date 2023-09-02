attach_new_output_to_output <- function(new_out,
                                        output,
                                        level,
                                        grouped_data,
                                        heading_line) {
  new_out <- unlist(new_out)
  new_out <- new_out[!stringi::stri_isempty(new_out)]
  new_out <- stringi::stri_c(new_out, collapse = "\n\n", ignore_null=TRUE) # Space between elements

  stringi::stri_c(output,
                            if(level == ncol(grouped_data) &&
                               names(grouped_data)[level] != ".element_name" &&
                               nchar(new_out) > 4) heading_line,
                            new_out,
                            sep = "\n", ignore_null=TRUE) # Space between heading and first element

}