
#' Embed Reactable Table
#'
#' @inheritParams summarize_data
#' @param information Which pre-computed information for each variable-category to display.
#' @param ... Further arguments passed to reactable()
#'
#' @return
#' @export
#'
#' @examples
embed_table_reactable <-
  function(data,
           cols = tidyselect::everything(),
           by = NULL,
           showNA = c("ifany", "always", "no"),
           percentage = TRUE,
           digits = 1,
           percent_sign = TRUE,
           sort_by = c(".variable_name", ".variable_label", ".category"),
           ignore_if_below = 1,
           information =
             c(".variable_label", #".variable_name",
               ".category",
               ".count", ".count_se",
               ".proportion", ".proportion_se",
               ".mean", ".mean_se", ".mean_base",
                ".data_label", ".comb_categories", ".sum_value"),
           label_separator = NULL,
           ...) {
  showNA <- rlang::arg_match(showNA, multiple = FALSE)
  check_data_frame(data)
  check_string(label_separator, null.ok=TRUE)

  data <- dplyr::select(data, {{cols}}, {{by}})


  cols_enq <- rlang::enquo(arg = cols)
  cols_pos <- tidyselect::eval_select(cols_enq, data = data)
  by_enq <- rlang::enquo(arg = by)
  by_pos <- tidyselect::eval_select(by_enq, data = data)

  check_category_pairs(data = data, cols_pos = c(cols_pos))


  data_out <-
    summarize_data(
      data = data,
      cols = {{cols}},
      by = {{by}},
      percentage = percentage,
      showNA = showNA,
      digits = digits,
      percent_sign = percent_sign,
      sort_by = sort_by,
      desc = desc,
      ignore_if_below = ignore_if_below,
      label_separator = label_separator,
      call = rlang::caller_env())

  selected_cols <-
    rlang::set_names(vector(mode = "list", length = length(information)),
                     nm = information)

  reactable::reactable(data = data_out, columns = selected_cols)
}






#' Title
#'
#' @param x Crosstable
#' @param label_separator [\code{character(1)}]\cr If not NULL (default), will split labels into main- and sub-questions and create figure caption.
#' @param docx_template  [\code{character(1) || officer::read_docx()}]\cr
#' @param caption_style [\code{character(1)}]\cr Word template style to be used for formatting chart. Defaults to "Normal".
#' @param caption_autonum Object obtained from \link[officer]{run_autonum}.
#' @param body_style [\code{character(1)}]\cr Word style for table body cells.
#' @param table_header_style [\code{character(1)}]\cr Word style for table header cells.
#' @param footer_style [\code{character(1)}]\cr Word style for table footer.
#' @param return_docx [\code{logical(1)}]\cr If FALSE, will return the table
#' rather than a rdocx object with the table inside it. Set to TRUE (default)
#' if piping multiple tables and charts together. Set to FALSE if you want to
#' continue modify the table.
#' @param topcaption [\code{logical(1)}]\cr Place caption above (TRUE, default) or below table.
#'
#' @importFrom flextable border_inner_h border_remove hline_bottom font fontsize add_footer_lines set_caption body_add_flextable
#' @importFrom officer fp_border fp_par body_add_fpar
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @importFrom rlang %||%
#' @importFrom crosstable as_flextable
#' @return rdocx object, which can be saved with print() after loading the officer-package
#' @export
#'
#' @examples
#' library(officer)
#' crosstable_to_apa(x=crosstable::crosstable(ex_survey1,
#' cols = b_1:b_3, percent_pattern = "{p_col}"),
#'  label_separator=" - ")
crosstable_to_apa <- function(x,
                              label_separator = NULL,
                              docx_template = NULL,
                              caption_style = NULL,
                              body_style = NULL,
                              table_header_style = NULL,
                              footer_style = NULL,
                              caption_autonum = NULL,
                              topcaption = TRUE,
                              return_docx = TRUE) {

  docx_file <- use_docx(docx_template = docx_template)
  caption_style <- caption_style %||% "Normal"
  body_style <- body_style %||% "Normal"
  table_header_style <- table_header_style %||% "Normal"
  footer_style <- footer_style %||% "Normal"


  if(!is.null(label_separator)) {
    main_question <- get_main_question2(x$label, label_separator = label_separator)


    sep_pat <- paste0("^(.*)", label_separator, "(.*)$")
    x <- dplyr::mutate(.data = x,
                       label = stringr::str_replace(string = .data$label,
                                                    pattern = .env$sep_pat,
                                                    replacement = "\\2"))
  }

  y <- crosstable::as_flextable(x)
  if(!is.null(label_separator)) {
    y <- flextable::set_caption(x = y,
                                align_with_table = TRUE,
                                caption = main_question,
                                autonum = caption_autonum,
                                word_stylename = caption_style)
  }
  y <- flextable::style(x = y, pr_p = officer::fp_par(word_style = body_style), part = "body")
  y <- flextable::style(x = y, pr_p = officer::fp_par(word_style = table_header_style), part = "header")
  y <- flextable::border_remove(y)
  y <- flextable::hline(x = y, border = officer::fp_border(), part = "all")
  y <- flextable::hline_top(x = y, border = officer::fp_border(), part = "all")
  y <- flextable::hline_bottom(x = y, border = officer::fp_border(), part = "body")

  docx_file <-
    flextable::body_add_flextable(x = docx_file, value = y,
                                align = "left",
                                split = FALSE,
                                topcaption = topcaption)

  docx_file <-
    officer::body_add_par(x = docx_file,
                         value = paste0("Note. N="), style = footer_style)

  if(return_docx) docx_file else y
}


#' Tabulate categorical variables with proportions
#'
#' @param data data.frame with only variables you want included.
#' @param yvars Dependent variable, as string.
#' @param xvars Optional independent variable, as string.
#' @param var_labels Integer, 0=variable name, 1=variable name - variable label,
#' 2=variable label.
#' @param val_labels Logical, whether to use value labels as column names in output.
#' @param prop Logical, whether to compute proportions (default), or n_observations.
#' @param prefix Prefix to frequency variables in output.
#' @param na.rm.yvar Remove missing in dependent variable? Defaults to false.
#' @param na.rm.xvar Remove missing in independent variable? Defaults to false.
#'
#' @return data.frame with some fixed columns
#' @importFrom dplyr count all_of mutate
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom rlang set_names abort
#' @export
#'
#' @examples tab_cat_prop(ex_survey1[,paste0("b_", 1:3)])
#' tab_cat_prop(ex_survey1, xvars = "x1_sex", yvars = paste0("b_", 1:3))

tab_cat_prop <- function(data,
                         yvars=NULL,
                         xvars=NULL,
                         var_labels=2,
                         val_labels=FALSE,
                         prop=TRUE,
                         prefix="v",
                         na.rm.yvar=FALSE,
                         na.rm.xvar=FALSE) {


  if(!var_labels %in% 0:2) rlang::abort("var_labels must be either 0, 1 or 2")
  if(!is.logical(prop)) rlang::abort("prop must be logical, TRUE or FALSE")
  if(!is.logical(val_labels)) rlang::abort("val_labels must be logical, TRUE or FALSE")
  if(!is.logical(na.rm.yvar)) rlang::abort("na.rm must be logical, TRUE or FALSE")
  if(!is.logical(na.rm.xvar)) rlang::abort("na.rm must be logical, TRUE or FALSE")
  if(is.null(yvars) && !is.null(xvars)) rlang::abort("yvars must be specified if xvars is specified.")
  if(is.null(yvars)) yvars <- names(data)

  yvars <- rlang::set_names(yvars)

  out <-
    purrr::map_dfr(
      .x = yvars,
      .id = "yvar",
      .f = function(dep) {

        if(is.null(data[[dep]])) rlang::abort(c(i="Unable to find yvars variable", x=dep))

        if(!is.null(xvars)) {
          xvars <- rlang::set_names(xvars)
          purrr::map_dfr(xvars, .id = "xvar", .f = function(indep) {
            if(is.null(data[[indep]])) rlang::abort(c(i="Unable to find xvars variable", x=indep))

            out <-
              dplyr::count(data,
                           yvar_cat = .data[[dep]],
                           xvar_cat = .data[[indep]],
                           name = ".n")
            if(na.rm.xvar) out <- out[!is.na(out$xvar_cat), ]
            if(na.rm.yvar) out <- out[!is.na(out$yvar_cat), ]
            if(prop) {
              out <- dplyr::group_by(out, .data$xvar_cat)
              out <- dplyr::mutate(out, .n=.data$.n/sum(.data$.n, na.rm = TRUE))
              out <- dplyr::ungroup(out)
            }
            y_label <- attr(data[[dep]], "label")
            x_label <- attr(data[[indep]], "label")
            out$yvar_label <-
              if(!is.null(y_label)) c(dep,
                                      paste0(dep, " - ", y_label),
                                      y_label)[var_labels+1] else dep
            out$xvar_label <-
              if(!is.null(x_label)) c(indep,
                                      paste0(indep, " - ", x_label),
                                      x_label)[var_labels+1] else indep

            if(val_labels) {
              .w <- attr(data[[dep]], "labels")
              .w <- data.frame(yvar_cat=unname(.w),
                               yvar_val_label=names(.w))
              out <- merge.data.frame(x = out, y = .w, by="yvar_cat")
            }
            out
          })

        } else {


          out <- dplyr::count(data,
                              yvar_cat=.data[[dep]],
                              name = ".n")
          if(na.rm.yvar) out <- out[!is.na(out$yvar_cat), ]
          if(prop) out$.n <- out$.n/sum(out$.n, na.rm = TRUE)
          y_label <- attr(data[[dep]], "label")
          out$yvar_label <-
            if(!is.null(y_label)) c(dep,
                                    paste0(dep, " - ", y_label),
                                    y_label)[var_labels+1] else dep
          if(val_labels) {
            .w <- attr(data[[dep]], "labels")
            .w <- data.frame(yvar_cat=unname(.w),
                             yvar_val_label=names(.w))
            out <- merge.data.frame(x = out, y = .w, by="yvar_cat")
          }
          out
        }
      })

  if(val_labels) {
    out$yvar_cat <- out$yvar_val_label
    out$yvar_val_label <- NULL
  }
  out <- tidyr::separate(out, col = dplyr::all_of("yvar_label"),
                         into=c("yvar_group_label", "yvar_part_label"),
                         sep=" - ")
  out <- tidyr::separate(out, col = dplyr::all_of("yvar"),
                         into=c("yvar_group", "yvar_part"),
                         sep="_", remove = F)
  if(!is.null(xvars)) {
    out <- tidyr::separate(out, col = dplyr::all_of("xvar_label"),
                           into=c("xvar_group_label", "xvar_label"),
                           sep=" - ", remove = F)
  }
  out
}

# tab_cat_prop <- function(data,
# 						 yvars=NULL,
# 						 xvars=NULL,
# 						 var_labels=2,
# 						 val_labels=FALSE,
# 						 prop=TRUE,
# 						 prefix="v",
# 						 na.rm.yvar=FALSE,
# 						 na.rm.xvar=FALSE) {
#
#
# 	if(!var_labels %in% 0:2) rlang::abort("var_labels must be either 0, 1 or 2")
# 	if(!is.logical(prop)) rlang::abort("prop must be logical, TRUE or FALSE")
# 	if(!is.logical(val_labels)) rlang::abort("val_labels must be logical, TRUE or FALSE")
# 	if(!is.logical(na.rm.yvar)) rlang::abort("na.rm must be logical, TRUE or FALSE")
# 	if(!is.logical(na.rm.xvar)) rlang::abort("na.rm must be logical, TRUE or FALSE")
# 	if(is.null(yvars) && !is.null(xvars)) rlang::abort("yvars must be specified if xvars is specified.")
# 	if(is.null(yvars)) yvars <- names(data)
#
# 	yvars <- rlang::set_names(yvars)
#
# 	out <-
# 		purrr::map_dfr(
# 			.x = yvars,
# 			.id = "yvar",
# 			.f = function(dep) {
#
# 				if(is.null(data[[dep]])) rlang::abort(c(i="Unable to find yvars variable", x=dep))
#
# 				if(!is.null(xvars)) {
# 					xvars <- rlang::set_names(xvars)
# 					purrr::map_dfr(xvars, .id = "xvar", .f = function(indep) {
# 						if(is.null(data[[indep]])) rlang::abort(c(i="Unable to find xvars variable", x=indep))
#
# 						out <-
# 							dplyr::count(data,
# 										 yvar_cat = .data[[dep]],
# 										 xvar_cat = .data[[indep]],
# 										 name = ".n")
# 						if(na.rm.xvar) out <- out[!is.na(out$xvar_cat), ]
# 						if(na.rm.yvar) out <- out[!is.na(out$yvar_cat), ]
# 						if(prop) {
# 							out <- dplyr::group_by(out, .data$xvar_cat)
# 							out <- dplyr::mutate(out, .n=.data$.n/sum(.data$.n, na.rm = TRUE))
# 							out <- dplyr::ungroup(out)
# 						}
# 						y_label <- attr(data[[dep]], "label")
# 						x_label <- attr(data[[indep]], "label")
# 						out$yvar_label <-
# 							if(!is.null(y_label)) c(dep,
# 													paste0(dep, " - ", y_label),
# 													y_label)[var_labels+1] else dep
# 						out$xvar_label <-
# 							if(!is.null(x_label)) c(indep,
# 													paste0(indep, " - ", x_label),
# 													x_label)[var_labels+1] else indep
#
# 						if(val_labels) {
# 							.w <- attr(data[[dep]], "labels")
# 							.w <- data.frame(yvar_cat=unname(.w),
# 											 yvar_val_label=names(.w))
# 							out <- merge.data.frame(x = out, y = .w, by="yvar_cat")
# 						}
# 						out
# 					})
#
# 				} else {
#
#
# 					out <- dplyr::count(data,
# 										yvar_cat=.data[[dep]],
# 										name = ".n")
# 					if(na.rm.yvar) out <- out[!is.na(out$yvar_cat), ]
# 					if(prop) out$.n <- out$.n/sum(out$.n, na.rm = TRUE)
# 					y_label <- attr(data[[dep]], "label")
# 					out$yvar_label <-
# 						if(!is.null(y_label)) c(dep,
# 												paste0(dep, " - ", y_label),
# 												y_label)[var_labels+1] else dep
# 					if(val_labels) {
# 						.w <- attr(data[[dep]], "labels")
# 						.w <- data.frame(yvar_cat=unname(.w),
# 										 yvar_val_label=names(.w))
# 						out <- merge.data.frame(x = out, y = .w, by="yvar_cat")
# 					}
# 					out
# 				}
# 			})
#
# 	if(val_labels) {
# 		out$yvar_cat <- out$yvar_val_label
# 		out$yvar_val_label <- NULL
# 	}
# 	out <- tidyr::separate(out, col = dplyr::all_of("yvar_label"),
# 						   into=c("yvar_group_label", "yvar_part_label"),
# 						   sep=" - ")
# 	out <- tidyr::separate(out, col = dplyr::all_of("yvar"),
# 						   into=c("yvar_group", "yvar_part"),
# 						   sep="_", remove = F)
# 	if(!is.null(xvars)) {
# 	out <- tidyr::separate(out, col = dplyr::all_of("xvar_label"),
# 						   into=c("xvar_group_label", "xvar_label"),
# 						   sep=" - ", remove = F)
# 	}
# 	out
# }



#' Turn tibble-result from tab_cat_prop into a prettier table
#'
#' Does what it says.
#' @param data data.frame from tab_cat_prop
#' @param prefix string as prefix for column names
#' @importFrom dplyr all_of
#' @importFrom tidyr pivot_wider
#' @return data.frame
#' @export
#'
#' @examples tab_cat_prop_to_table(tab_cat_prop(ex_survey1[,paste0("b_", 1:3)]))
tab_cat_prop_to_table <- function(data, prefix="") {
  tidyr::pivot_wider(data = data,
                     # id_cols = dplyr::all_of(c("yvar", "yvar_label",
                     # 						  if(!is.null(data$xvars)) c("xvar", "xvar_label",  "xvar_cat"))),
                     names_from = dplyr::all_of("yvar_cat"),
                     names_prefix = prefix,
                     values_from = dplyr::all_of(".n"))
}


