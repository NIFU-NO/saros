
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

          data_cols <-
            data %>%
            {if(inherits(., what="srvyr")) srvyr::as_tibble(.) else .} %>%
            colnames()

          y_col_pos <- unique(section_df$col_name)
          y_col_pos <- match(y_col_pos, data_cols)

          ######################################################################


          if(element_name == "uni_cat_plot_html" &&
             all(section_df$designated_type == "cat")) {
            out <-
              rlang::exec(
                embed_plot_cat_html,

                  data = data,
                  cols = y_col_pos,
                  !!!dots)

            return(rlang::set_names(list(out), nm = name))
          }
          ######################################################################


          if(element_name == "uni_cat_plot_docx" &&
             all(section_df$designated_type == "cat")) {
            out <-
              rlang::exec(
                embed_plot_cat_docx,
                data = data,
                cols = y_col_pos,
                !!!dots)

            return(rlang::set_names(list(out), nm = name))
          }

          ######################################################################


          if(element_name == "uni_cat_table_html" &&
             all(section_df$designated_type == "cat")) {
            out <-
              rlang::exec(
                embed_table_cat_html,
                data = data,
                cols = y_col_pos,
                !!!dots)

            return(rlang::set_names(list(out), nm = name))
          }

          ######################################################################

          if(element_name == "uni_cat_table_docx" &&
             all(section_df$designated_type == "cat")) {
            # out <-
            #   rlang::exec(
            #     embed_table_cat_docx,
            #     data = data,
            #     cols = y_col_pos,
            #     !!!dots)
            #
            # return(rlang::set_names(list(out), nm = name))
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

                  by_pos <-
                    match(.x, data_cols)

                  by_type <-
                    by_df %>%
                    dplyr::filter(.data$col_name == .x) %>%
                    dplyr::pull(.data$designated_type)


                  if(element_name == "bi_catcat_plot_html" &&
                     all(section_df$designated_type == "cat") &&
                     all(by_type == "cat")) {


                    return(
                      rlang::exec(
                        embed_plot_cat_html,
                        data = data,
                        cols = y_col_pos,
                        by = by_pos,
                        !!!dots)
                    )
                  }

                  if(element_name == "bi_catcat_plot_docx" &&
                     all(section_df$designated_type == "cat") &&
                     all(by_type == "cat")) {
                    return(
                      rlang::exec(
                        embed_plot_cat_docx,
                        data = data,
                        cols = y_col_pos,
                        by = by_pos,
                        !!!dots)
                    )
                  }
                })
            }
          }



    }) %>%
      unlist(recursive = FALSE)
  }



#' Mass Generate Elements
#'
#' @param element_names Character vector of element names. Defaults to all available.
#' @inheritParams lst_saros_elements
#'
#' @return List of elements that can be directly used in gen_qmd_report()
#' @importFrom rlang !!!
#' @export
#'
#' @examples
#' library(dplyr)
#' ex_survey_elements_list <-
#' mass_lst_saros_elements(data_overview =
#'                           ex_survey_ch_overview %>%
#'                           refine_data_overview(data = ex_survey1,
#'                                                       label_separator = " - ",
#'                                                       name_separator = "_"),
#'                         data = ex_survey1,
#'                         label_separator = " - ")
mass_lst_saros_elements <-
  function(element_names = list_available_element_types(),
           data_overview,
           data,
           ...) {
    dots <- rlang::list2(...)

    cli::cli_progress_bar(name = "Producing elements", total = length(element_names))
    element_names <-
      element_names %>%
      rlang::set_names()
    out <- element_names %>% as.list()

    for(i in element_names) {

        cli::cli_progress_update()
      out[[i]] <-
        rlang::exec(
          lst_saros_elements,
          data_overview = data_overview,
          element_name = element_names[i],
          data = data,
          !!!dots)

      }
    cli::cli_progress_done()


    out
  }
