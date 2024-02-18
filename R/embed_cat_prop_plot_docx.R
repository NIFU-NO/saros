
#' Create Word Report with Univariates for Categorical Columns Sharing Same Categories
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#' @inheritParams embed_cat_prop_plot
#' @param plot_height Fixed height of the plot in cm. Defaults to 15.
#'
#' @importFrom tidyselect everything eval_select
#' @importFrom officer read_docx docx_dim block_caption body_add_caption
#' @importFrom mschart body_add_chart
#' @importFrom rlang enquo is_bare_character
#' @importFrom cli cli_abort
#' @importFrom stats ave
#' @return rdocx object, which can be saved with print() after loading the officer-package
#' @export
#'
#' @examples
#' library(officer) # To save the rdocx object to disk
#'
#' test_docx_b13 <-
#'    ex_survey |>
#'    embed_cat_prop_plot_docx(dep = b_1:b_3,
#'               showNA = "never",
#'               descend = TRUE,
#'               return_raw = FALSE,
#'               hide_label_if_prop_below=0,
#'               data_label = "percentage_bare",
#'               data_label_decimal_symbol = ",",
#'               digits = 1,
#'               label_font_size = 12,
#'               main_font_size = 12,
#'              plot_height_multiplier = .3,
#'              plot_height_fixed_constant = 1,
#'               vertical = FALSE,
#'               font_family = "sans")
#' print(test_docx_b13, target = tempfile(fileext = ".docx"))
embed_cat_prop_plot_docx <-
  function(data,
           ...,
           dep = tidyselect::everything(),
           indep = NULL,
           colour_palette = NULL,
           mesos_group = NULL,
           plot_height = 15,
           inverse = FALSE) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "cat_prop_plot")

    check_multiple_indep(data, indep = {{indep}})

    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data)

    check_category_pairs(data = data, cols_pos = c(dep_pos))

    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        dep = names(dep_pos),
        indep = names(indep_pos),
        !!!dots)

    if(length(indep_pos)==0) {
      data_out$.variable_label <- forcats::fct_rev(data_out$.variable_label)
    }

    if(dplyr::n_distinct(data_out$.category, na.rm = dots$showNA == "never") == 2 &&
       !rlang::is_null(dots$colour_2nd_binary_cat)) {
      data_out$.category <- forcats::fct_rev(data_out$.category)
    }

    chart <-
      rlang::exec(
        prep_cat_prop_plot_docx,
        data = data_out,
        inverse = inverse,
        colour_palette = colour_palette,
        call = rlang::caller_env(),
        !!!dots)

    if(!rlang::is_null(dots$label_separator)) {
      indep_label <- unname(get_raw_labels(data = data, col_pos = indep_pos))
      caption <- get_raw_labels(data = data, col_pos = dep_pos)
      caption <- get_main_question2(caption, label_separator = dots$label_separator)
      caption <- create_caption(caption,
                                data_out = data_out,
                                indep_pos = indep_label,
                                mesos_group = mesos_group,
                                filepath = NULL,
                                translations = dots$translations)
      attr(chart, "saros_caption") <- caption
    } else caption <- NULL

    if(FALSE) {
      chart
    } else {


      ## Consider moving all the below into prep_cat_prop_plot_docx
      ## so that embed_chart becomes one function
      docx_file <- use_docx(docx_template = dots$docx_template)

      docx_dims <-
        get_docx_dims(docx_file)


      docx_file <-
        mschart::body_add_chart(
          x = docx_file,
          chart = chart,
          pos = "after",
          width = docx_dims[["w"]],
          height = plot_height)
      docx_file

    }
  }


