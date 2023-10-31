#
# rcode_to_quarto <- function(code, call = rlang::caller_env()) {
#   check_string(code, n=1, null.ok=FALSE, call = call)
#   stringi::stri_c(ignore_null=TRUE,
#                   "```{r}",
#                   code,
#                   "``` \n",
#                   sep = "\n")
# }

# fix_path_spaces <- function(path) {
#   if(!rlang::is_null(quarto::quarto_path()) &&
#      quarto::quarto_version() < 1.3) {
#     stringi::stri_replace_all(str = path, regex = "[[:space:]]", replacement = "_")
#   } else path
# }

conv_to_valid_obj_name <- function(x, max_width = 48) {
  stringi::stri_replace_all(str = x,
                           regex = "[[:space:][:punct:]]",
                           replacement = "_") %>%
    stringi::stri_replace(regex = "^([0-9])", replacement = "x$1") %>%
    stringi::stri_sub(str = ., from = 1, to = max_width,
                      use_matrix = FALSE, ignore_negative_length = TRUE)

}

list_valid_obj_name <- function(data, max_width = 48) {

  data %>%
    dplyr::distinct(dplyr::pick(tidyselect::everything())) %>%
    glue::glue_data(stringi::stri_c(ignore_null=TRUE, "{", colnames(.), "}", collapse="_")) %>%
    get_common_name() %>%
    conv_to_valid_obj_name(max_width = max_width)
}

create_heading <- function(x, level = NULL,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {

  check_string(x, n = NULL, null.ok = TRUE, arg = arg, call = call)
  if(rlang::is_null(level)) level <- names(x)[1]
  stringi::stri_c(ignore_null=TRUE,
    strrep("#", times=level), " ",
    x[level])
}


