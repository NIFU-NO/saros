
#' Mass Create Elements of A Certain Type
#'
#' @param data_overview A (grouped) data frame containing at least the following columns
#' \describe{
#' \item{<grouping variables>}{ used to distinguish sets of columns belonging to each element.}
#' \item{"col_name"}{ name of columns.}
#' \item{"designated_type"}{ either 'cat' (categorical/ordinal/binary), 'int' (interval/continuous) or 'chr' (text).}
#' }
#' @param element_name See \code{element_list()} for options.
#' @param data Survey data.
#' @param ... arguments forwarded to the corresponding functions that create the elements.
#'
#' @return Named list of elements, where each element can .
#' @export
#'
#' @examples
lst_saros_elements <-
  function(data_overview,
           element_name = "uni_cat_plot_html",
           data = NULL,
           ...) {

    # args <- rlang::list2(...)

    check_element_name(x = element_name, n = 1, null.ok = FALSE)

    ## Save some memory by reducing data set to those variables which have been specified in data_overview
    if(!is.null(data)) {
      data <- dplyr::select(.data = data,
                          tidyselect::all_of(unique(data_overview$col_name)))
      data_overview <- dplyr::select(.data = data_overview,
                                     !tidyselect::all_of("col_pos")) # These will be invalid and thus removed.
    }


    list_names <-
      data_overview %>%
      list_valid_obj_name() # Currently ignores the fact that some might return NULL...

    data_overview %>%
      dplyr::group_map(
        .keep = TRUE,
        .f = function(section_df, section_key) {

          name <- list_valid_obj_name(section_key)

          if(element_name == "uni_cat_plot_html" &&
             all(section_df$designated_type == "cat")) {
            out <-
              embed_chart_categorical_ggplot(
                data = data,
                cols = tidyselect::all_of(unique(section_df$col_name)),
                ...
              )

            return(rlang::set_names(list(out), nm = name))
          }


          if(element_name == "uni_cat_plot_docx" &&
             all(section_df$designated_type == "cat")) {
            out <-
              embed_chart_categorical_office(
                data = data,
                cols = tidyselect::all_of(unique(section_df$col_name)),
                ...
              )
            return(rlang::set_names(list(out), nm = name))
          }


          if(element_name == "uni_cat_plot_docx" &&
             all(section_df$designated_type == "cat")) {
            out <-
              embed_chart_categorical_office(
                data = data,
                cols = tidyselect::all_of(unique(section_df$col_name)),
                ...
              )
            return(rlang::set_names(list(out), nm = name))
          }

    }) %>%
      unlist(recursive = FALSE)
  }



