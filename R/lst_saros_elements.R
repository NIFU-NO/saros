
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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments forwarded to the corresponding functions that create the elements.
#'
#' @return Named list of elements, where each element can .
#' @importFrom rlang !!!
#' @export
#'
#' @examples
#' library(dplyr)
#'  ex_survey_ch_overview %>%
#'  refine_data_overview(label_separator = " - ",
#'                        name_separator = "_") %>%
#' lst_saros_elements(data_overview=.,
#'                    data= ex_survey1)
lst_saros_elements <-
  function(data_overview,
           element_name = "uni_cat_plot_html",
           data = NULL,
           ...) {

    dots <- rlang::list2(...)


    check_element_name(x = element_name, n = 1, null.ok = FALSE)

    compare_many <- function(x) {
      all(purrr::map_lgl(as.list(x[-1]),
                         .f = ~identical(.x, x[[1]])))
    }

    data_overview %>%
      dplyr::group_map(
        .keep = TRUE,
        .f = function(section_df, section_key) {

          name <- list_valid_obj_name(section_key)

          y_col_names <- unique(section_df$col_name)

          if(element_name == "uni_cat_plot_html" &&
             all(section_df$designated_type == "cat")) {
            out <-
              embed_chart_categorical_ggplot(
                data = data,
                cols = tidyselect::all_of(y_col_names),
                ...
              )

            return(rlang::set_names(list(out), nm = name))
          }
          ######################################################################


          if(element_name == "uni_cat_plot_docx" &&
             all(section_df$designated_type == "cat")) {
            out <-
              embed_chart_categorical_office(
                data = data,
                cols = tidyselect::all_of(y_col_names),
                ...
              )
            return(rlang::set_names(list(out), nm = name))
          }
          ######################################################################


          if(element_name == "uni_cat_plot_docx" &&
             all(section_df$designated_type == "cat")) {
            out <-
              embed_chart_categorical_office(
                data = data,
                cols = tidyselect::all_of(y_col_names),
                ...
              )
            return(rlang::set_names(list(out), nm = name))
          }
          ######################################################################


          if(all(section_df$designated_role != "indep") &&
             stringr::str_detect(element_name, "^bi_.*") &&
             !rlang::is_null(section_df$by_cols_df) &&
             compare_many(section_df$by_cols_df)) {

            by_df <- dplyr::pull(section_df, .data$by_cols_df)[[1]]

            if(inherits(by_df, what = "data.frame")) {

              by_df %>%
                dplyr::pull(.data$col_name) %>%
                stringr::str_unique() %>%
                rlang::set_names(nm = stringr::str_c(name, "_BY_", .)) %>%
                purrr::map(.f = ~{

                  by_type <-
                    by_df %>%
                    dplyr::filter(.data$col_name == .x) %>%
                    dplyr::pull(.data$designated_type)


                  if(element_name == "bi_catcat_plot_html" &&
                     all(section_df$designated_type == "cat") &&
                     all(by_type == "cat")) {


                    return(
                      embed_chart_categorical_ggplot(
                        data = data,
                        cols = tidyselect::all_of(y_col_names),
                        by = tidyselect::all_of(.x),
                        ...
                      )
                    )
                  }

                  if(element_name == "bi_catcat_plot_docx" &&
                     all(section_df$designated_type == "cat") &&
                     all(by_type == "cat")) {
                    return(
                      embed_chart_categorical_office(
                        data = data,
                        cols = tidyselect::all_of(y_col_names),
                        by = tidyselect::all_of(.x),
                        ...
                      )
                    )
                  }
                })
            }
          }



    }) %>%
      unlist(recursive = FALSE)
  }


#' Title
#'
#' @param element_names Character vector of element names. Defaults to all available.
#' @inheritParams lst_saros_elements
#'
#' @return List of elements that can be directly used in gen_qmd_report()
#' @export
#'
#' @examples
# #' library(dplyr)
# #' ex_survey_elements_list <-
# #' mass_lst_saros_elements(element_names = saros::list_available_element_types(),
# #'                         data_overview =
# #'                           saros::ex_survey_ch_overview %>%
# #'                           saros::refine_data_overview(data = saros::ex_survey1,
# #'                                                       label_separator = " - ",
# #'                                                       name_separator = "_"),
# #'                         data = saros::ex_survey1,
# #'                         label_separator = " - ")
# mass_lst_saros_elements <-
#   function(element_names = list_available_element_types(),
#            data_overview,
#            data,
#            ...) {
#     element_names %>%
#     rlang::set_names() %>%
#       purrr::imap(.f = ~{
#
#         lst_saros_elements(data_overview = data_overview,
#                            element_name = rlang::set_names(.x, .y),
#                            data = data,
#                            ...)
#       })
#   }
