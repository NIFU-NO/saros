#' Recodes Multiple Sets of Factor Variables of Checkbox Items
#'
#' Qualtrics produces datasets where a matrix set of checkbox questions 
#' have value labels (factor labels) representing the actual question. When presenting
#' these questions in a figure or table, however, it is often useful to recode 
#' the value labels to something common, e.g. "Selected" and "Not selected".
#' 
#' Furthermore, this function recodes all of the variables within a set to missing
#' if all of the variables are missing - under the assumption that the respondent
#' has then omitted them all. In other cases, e.g. one checkbox is checked, then
#' the other variables can be assumed to be consciously "Not selected".
#'
#' @param data Data frame or tibble.
#' @param var_patterns Character vector of regex search patterns that uniquely identify
#' variables belonging to sets of batteries. Same as used in tidyselect's matches().
#' @param keep_tmp_var Flag, whether the temporary variables created should 
#' @param input_check_value,input_uncheck_value Factor/integer values indicating checked and unchecked responses.
#' @param output_check_value,output_uncheck_value Recoded values for checked and unchecked responses.
#' @param output_all_unchecked_value If respondent has not checked any checkboxes, what should they be recoded as? Set to NULL to not recode.
#' @param output_check_label,output_uncheck_label,output_all_unchecked_label 
#'   Factor labels.
#' @param keep_tmp_var Whether to keep temporary variables for output_all_unchecked_value (mostly for debugging). Defaults to FALSE.
#' @return Data frame/tibble.
#' @export
#'
#' @examples
#' n <- 100 
#' tmp <-
#' data.frame(a_1 = factor(sample(c("Car", NA), size = n, replace = TRUE)),
#'            a_2 = factor(sample(c("Train", NA), size = n, replace = TRUE)),
#'            a_3 = factor(sample(c("Bus", NA), size = n, replace = TRUE)),
#'            a_4 = factor(sample(c("Flight", NA), size = n, replace = TRUE)),
#'            b_1 = factor(sample(c("Apple", NA), size = n, replace = TRUE)),
#'            b_2 = factor(sample(c("Orange", NA), size = n, replace = TRUE)),
#'            b_3 = factor(sample(c("Banana", NA), size = n, replace = TRUE)),
#'            b_4 = factor(sample(c("Grapes", NA), size = n, replace = TRUE)))
#' tmp2 <- recode_checkbox_sets(tmp, var_patterns = c("^a_", "^b_[[:digit:]]$"))
#' cbind(tmp, tmp2)
#'                      
recode_checkbox_sets <- 
  function(data, 
           var_patterns = NULL, 
           input_check_value = 1L,
           input_uncheck_value = NA_integer_,
           output_check_value = 2L,
           output_uncheck_value = 1L,
           output_all_unchecked_value = NA_integer_,
           output_check_label = "Selected",
           output_uncheck_label = "Not selected",
           output_all_unchecked_label = NULL,
           convert_to_factor = TRUE,
           keep_tmp_var = FALSE) {
    
    if(!inherits(data, "data.frame")) cli::cli_abort("{.arg data} must be a data frame or tibble.")

    checker <- function(arg) {
      if(!is.character(arg) || !length(arg) %in% c(1L, length(var_patterns))) {
        cli::cli_abort("{.arg {arg}} must be a character of length 1 or same length as {.arg var_patterns} ({length(var_patterns)}), not {length(arg)}.")
      }
    }
    for(arg in c(input_check_value, input_uncheck_value,
                 output_check_value, output_uncheck_value, output_all_unchecked_value,
                 output_check_label, output_uncheck_label, output_all_unchecked_label)) {
      checker(arg = arg)
    }
    
    
    data_out <- data
    for(var_pattern in var_patterns) {
      tmp_var <- stringi::stri_replace_all_regex(str = var_pattern, 
                                                 pattern = "[^[:alnum:]]+", replacement = "_")
      tmp_var <- paste0("tmp_", tmp_var, "_tmp")
      data_out <-
        data_out |>
        dplyr::rowwise() |>
        dplyr::mutate("{tmp_var}" := sum(as.integer(dplyr::c_across(tidyselect::matches(var_pattern))), na.rm=TRUE)) |>
        dplyr::ungroup() |>
        dplyr::mutate(dplyr::across(tidyselect::matches(var_pattern), 
                                    ~dplyr::case_when(if(!is.null(output_all_unchecked_value)) .data[[tmp_var]] == 0 ~ output_all_unchecked_value, 
                                                      as.integer(.x) %in% input_uncheck_value ~ output_uncheck_value,
                                                      .default = output_check_value))) |>
        dplyr::ungroup()
      if(isFALSE(keep_tmp_var)) {
        data_out[[tmp_var]] <- NULL
      }
      
      if(isTRUE(convert_to_factor)) {
        data_out <-
          data_out |>
          dplyr::mutate(dplyr::across(tidyselect::matches(var_pattern),
                                      ~factor(.x, 
                                              levels = c(output_uncheck_value, output_check_value, output_all_unchecked_value), 
                                              labels = c(output_uncheck_label, output_check_label, output_all_unchecked_label))))
      }
    }
    data_out <- dplyr::ungroup(data_out)
    data_out <- labelled::copy_labels_from(to = data_out, from=data, .strict = FALSE)
    data_out
  }