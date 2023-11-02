#' Remove regex pattern from variable labels
#'
#' @param data Data.frame
#' @param pattern String, regex pattern.
#' @param data_type_criterion Can be a function that specifies what kind of variables to select, e.g. is.factor
#'
#' @return Data
#' @export
#'
#' @examples remove_label_parts(ex_survey1, pattern = " living in")
remove_label_parts <- function(data,
                               pattern = NULL, data_type_criterion = NULL) {
  if(is.null(data_type_criterion) || !is.function(data_type_criterion)) data_type_criterion <- function(x) !is.null(x)

  if(rlang::is_string(pattern)) {
    for(var in colnames(data)) {
      # if(var == "campus2") browser()
      if(data_type_criterion(data[[var]])) {
        label <- attr(data[[var]], "label")
        label <- stringr::str_replace(label, pattern = pattern, replacement = "")

        if(length(label)>0 && rlang::is_string(label)) {

          attr(data[[var]], "label") <- label
        }
      }
    }
  }
  data
}
