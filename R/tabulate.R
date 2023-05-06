

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
#' @export
#'
#' @examples tab_cat_prop(ex_survey1[,stringr::str_c("b_", 1:3)])
#' tab_cat_prop(ex_survey1, xvars = "x1_sex", yvars = stringr::str_c("b_", 1:3))

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

  yvars <- stats::setNames(yvars, nm=yvars)

  out <-
    purrr::map_dfr(
      .x = yvars,
      .id = "yvar",
      .f = function(dep) {

        if(is.null(data[[dep]])) rlang::abort(c(i="Unable to find yvars variable", x=dep))

        if(!is.null(xvars)) {
          xvars <- stats::setNames(xvars, xvars)
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
                                      stringr::str_c(dep, " - ", y_label),
                                      y_label)[var_labels+1] else dep
            out$xvar_label <-
              if(!is.null(x_label)) c(indep,
                                      stringr::str_c(indep, " - ", x_label),
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
                                    stringr::str_c(dep, " - ", y_label),
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
# 	yvars <- stats::setNames(yvars, yvars)
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
# 					xvars <- stats::setNames(xvars, xvars)
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
# 													stringr::str_c(dep, " - ", y_label),
# 													y_label)[var_labels+1] else dep
# 						out$xvar_label <-
# 							if(!is.null(x_label)) c(indep,
# 													stringr::str_c(indep, " - ", x_label),
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
# 												stringr::str_c(dep, " - ", y_label),
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



#' Turn data frame-result from tab_cat_prop into a prettier table
#'
#' Does what it says.
#' @param data data.frame from tab_cat_prop
#' @param prefix string as prefix for column names
#' @importFrom dplyr all_of
#' @importFrom tidyr pivot_wider
#' @return data.frame
#' @export
#'
#' @examples tab_cat_prop_to_table(tab_cat_prop(ex_survey1[,stringr::str_c("b_", 1:3)]))
tab_cat_prop_to_table <- function(data, prefix="") {
  tidyr::pivot_wider(data = data,
                     # id_cols = dplyr::all_of(c("yvar", "yvar_label",
                     # 						  if(!is.null(data$xvars)) c("xvar", "xvar_label",  "xvar_cat"))),
                     names_from = dplyr::all_of("yvar_cat"),
                     names_prefix = prefix,
                     values_from = dplyr::all_of(".n"))
}


