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

# list_valid_obj_name <- function(data, max_width = 48) {
#
#   data %>%
#     dplyr::distinct(dplyr::pick(tidyselect::everything())) %>%
#     glue::glue_data(stringi::stri_c(ignore_null=TRUE, "{", colnames(.), "}", collapse="_")) %>%
#     get_common_name() %>%
#     conv_to_valid_obj_name(max_width = max_width)
# }

# create_obj_name <- function(grouping_structure,
#                             section_key,
#                             max_width,
#                             mesos_group,
#                             indep_sep_string = "_BY_",
#                             mesos_sep_string = "_FOR_") {
#
#   grouping_structure[grouping_structure %in%
#                        c(".variable_label_prefix_dep", ".variable_label_suffix_dep")] <- ".variable_name_dep"
#
#   obj_name_dep <- section_key
#   # obj_name_dep <- obj_name_dep[!colnames(obj_name_dep) %in% c(".variable_name_indep")]
#   obj_name_dep <- glue::glue_data(obj_name_dep, stringi::stri_c("{", colnames(obj_name_dep), "}", collapse="_", ignore_null=TRUE))
#   obj_name_dep <- get_common_name(obj_name_dep)
#   obj_name_dep <- stringi::stri_remove_empty_na(obj_name_dep)
#   obj_name_dep <- stringi::stri_sub(obj_name_dep, from = 1, to = max_width,
#                                     use_matrix = FALSE, ignore_negative_length = TRUE)
#   obj_name_dep <- stringi::stri_c(obj_name_dep, collapse = "_", ignore_null = TRUE)
#
#   ####
#   # obj_name_indep <-
#   #   if(dplyr::n_distinct(section_key$.variable_name_indep, na.rm = TRUE) == 1) {
#   #     stringi::stri_c(indep_sep_string, unique(indep_vars))
#   #   }
#   # if(!is.null(obj_name_indep) && any(is.na(obj_name_indep))) obj_name_indep <- NULL
#
#   ####
#   obj_name_mesos <-
#     if(rlang::is_string(mesos_group)) stringi::stri_c(mesos_sep_string, mesos_group)
#
#   ####
#   out <-
#     stringi::stri_c(obj_name_dep, #obj_name_indep,
#                   obj_name_mesos,
#                   ignore_null=TRUE)
#   out <- stringi::stri_replace(out, regex = "^([0-9])", replacement = "x$1")
#   filename_sanitizer(out)
# }




create_heading <- function(x, level = NULL,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {

  check_string(x, n = NULL, null.ok = TRUE, arg = arg, call = call)
  if(rlang::is_null(level)) level <- names(x)[1]
  stringi::stri_c(ignore_null=TRUE,
    strrep("#", times=level), " ",
    x[level])
}


