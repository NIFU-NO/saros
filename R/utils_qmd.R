
rcode_to_quarto <- function(code, call = rlang::caller_env()) {
  check_string(code, n=1, null.ok=FALSE, call = call)
  paste("```{r}",
         code,
         "``` \n\n",
         sep = "\n")
}

fix_path_spaces <- function(path) {
  if(!rlang::is_null(quarto::quarto_path()) &&
     quarto::quarto_version() < 1.3) {
    gsub("[[:space:]]", "_", path)
  } else path
}

conv_to_valid_obj_name <- function(x) {
  stringr::str_replace_all(string = x,
                           pattern = "[[:space:][:punct:]]",
                           replacement = "_") %>%
    stringr::str_replace(pattern = "^([0-9])", replacement = "x\\1")

}

list_valid_obj_name <- function(data) {
  data %>%
    dplyr::distinct(dplyr::pick(tidyselect::everything())) %>%
    glue::glue_data(stringr::str_c("{", colnames(.), "}", collapse="_")) %>%
    conv_to_valid_obj_name()
}

create_heading <- function(x, level = NULL,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {

  check_string(x, n = NULL, null.ok = TRUE, arg = arg, call = call)
  if(rlang::is_null(level)) level <- names(x)[1]
  stringr::str_c(
    stringr::str_dup("#", level), " ",
    x[level])
}


