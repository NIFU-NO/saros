#' Remove regex pattern from variable labels
#'
#' @param data Data.frame
#' @param pattern String, regex pattern.
#' @param replacement String, replacement for pattern.
#' @param data_type_criterion Can be a function that specifies what kind of variables to select, e.g. is.factor
#'
#' @return Data
#' @export
#'
#' @examples
#' remove_label_parts(ex_survey, pattern = " living in")
remove_label_parts <- function(data,
                               pattern = NULL,
                               replacement = "",
                               data_type_criterion = NULL) {
  if(is.null(data_type_criterion) || !is.function(data_type_criterion)) data_type_criterion <- function(x) !is.null(x)

  if(is_string(pattern)) {
    for(var in colnames(data)) {

      if(data_type_criterion(data[[var]])) {
        label <- attr(data[[var]], "label")
        label <- stringr::str_replace(label, pattern = pattern, replacement = replacement)

        if(length(label)>0 && is_string(label)) {

          attr(data[[var]], "label") <- label
        }
      }
    }
  }
  data
}
