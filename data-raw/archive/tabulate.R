
#' Fix to the janitor::tabyl function to make it work seamlessly with labelled variables.
#'
#' @param dat a data.frame containing the variables you wish to count. Or, a vector you want to tabulate.
#' @param ... the arguments to tabyl (here just for the sake of documentation compliance, as all arguments are listed with the vector- and data.frame-specific methods)
#' @param show_na should counts of NA values be displayed? In a one-way tabyl, the presence of NA values triggers an additional column showing valid percentages(calculated excluding NA values).
#' @param show_missing_levels should counts of missing levels of factors be displayed? These will be rows and/or columns of zeroes. Useful for keeping consistent output dimensions even when certain factor levels may not be present in the data.
#' @importFrom dplyr mutate across everything
#' @return Returns a data.frame with frequencies and percentages of the tabulated variable(s). A 3-way tabulation returns a list of data.frames.
#' @export
#'
#' @examples tabyl_lab(mtcars, cyl)
tabyl_lab <- function(dat, ..., show_na=TRUE, show_missing_levels=TRUE) {
  if(any(class(dat) %in% c("data.frame", "tbl"))) {
    dat <- mutate(dat, across(everything(), .fns=~labelled::to_factor(.x)))
    janitor::tabyl(dat, ..., show_na = show_na, show_missing_levels = show_missing_levels)
  } else {
    dat <- labelled::to_factor(dat)
    janitor::tabyl(dat, show_na = show_na, show_missing_levels = show_missing_levels, ...)
  }
}

#' Generate multiple 1-way frequency tables that share the same response scale.
#'
#' @param dat a data.frame containing the variables you wish to count. Or, a vector you want to tabulate.
#' @param show_na the arguments to tabyl (here just for the sake of documentation compliance, as all arguments are listed with the vector- and data.frame-specific methods)
#' @param show_missing_levels should counts of NA values be displayed? In a one-way tabyl, the presence of NA values triggers an additional column showing valid percentages(calculated excluding NA values).
#' @param var_label Defaults to both variable name and variable label, alternatively, TRUE for only label, FALSE for only name.
#' @return Returns a data.frame with frequencies and percentages of the tabulated variable(s).
#' @importFrom rlang abort warn set_names
#' @importFrom labelled var_label val_labels
#' @importFrom purrr map2_dfr
#' @importFrom tidyr pivot_wider
#' @importFrom janitor as_tabyl
#' @export
#'
#' @examples tabyl_multi(ex_survey1[,paste0("a_", 1:9)])

tabyl_multi <- function(dat,
                        show_na=TRUE, show_missing_levels=TRUE, var_label=NULL) {
  if(!any(class(dat) %in% c("data.frame", "tbl"))) rlang::abort("dat must be a data.frame or tibble for this trick to work.")
  var_names <- names(dat)
  var_labels <- labelled::var_label(dat, unlist = TRUE)
  val_labels <- labelled::val_labels(dat)

  check_dissimilarity1 <- length(unique(gsub("^(.*) - .*$", "\\1", var_labels)))>1L
  if(check_dissimilarity1) rlang::warn("Seems you have more than one type of label.")

  identicalValue <- function(x,y) if (identical(x,y)) x else rlang::abort("Seems you have different sets of labels.")
  Reduce(identicalValue,val_labels)

  x <- if(!is.null(var_label) && var_label) {
    rlang::set_names(dat, nm = var_labels)
  } else if(!is.null(var_label) && !var_label) {
    dat
  } else if(is.null(var_label)) {
    rlang::set_names(dat, nm = paste(var_names, "-", var_labels))
  }

  x <- purrr::map2_dfr(.x = x, .y=names(x), .id = "var",
                       .f = ~tabyl_lab(dat = .x, show_na = show_na, show_missing_levels = show_missing_levels))
  x <- tidyr::pivot_wider(data = x, id_cols = .data$var, names_from = .data$dat, values_from = .data$n)
  janitor::as_tabyl(x)
}


crosstable_list <- function(data, col, by, showNA = "ifany") {
  data |>
    dplyr::select({{col}}, {{by}}) |>
    tidyr::pivot_wider(
      names_from = {{by}},
      values_from = {{col}},
      values_fn = ~list(.x)) |>
    purrr::map(.f = ~{as.data.frame(table(.x[[1]], useNA = showNA))}) |>
    dplyr::bind_rows(.id = "label") |>
    dplyr::rename(variable = "Var1", value = "Freq") |>
    dplyr::mutate(.id = .data$label)
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
#' library(crosstable)
#' library(dplyr)
#' library(officer)
#' crosstable::crosstable(ex_survey1, b_1:b_3, percent_pattern = "{p_col}") %>%
#'   crosstable_to_apa(label_separator=" - ")
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
    y <- dplyr::mutate(.data = x,
                       label = stringr::str_replace(string = .data$label,
                                                    pattern = .env$sep_pat,
                                                    replacement = "\\2"))
  } else y <- x

  y <- crosstable::as_flextable(y)
  # y <- flextable::add_footer_lines(x = y,
  #                                  values = paste0("Note. N="),
  #                                  top = FALSE)
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
  # y <- flextable::hline(x = y, border = officer::fp_border(), part = "all")
  y <- flextable::hline_top(x = y, border = officer::fp_border(), part = "all")
  y <- flextable::hline_bottom(x = y, border = officer::fp_border(), part = "body")


  flextable::body_add_flextable(x = docx_file, value = y, align = "left",
                                split = FALSE, topcaption = topcaption)

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


#' Create barplot
#'
#' @param data data.frame
#' @param y_string_wrap_width Wrap y-labels if longer than this. Default=40.
#' @param reverse_y Reverse order of y-labels (defaults to FALSE).
#' @param base_font_size Base font size. Default=12.
#' @param percent Logical, percentage vs proportion (default).
#' @param decimals Number of decimals for labels.
#' @importFrom dplyr mutate na_if
#' @import ggplot2
#' @importFrom scales label_percent label_number
#' @importFrom ggfittext geom_bar_text
#' @return ggplot2-object
#' @export
#'
#' @examples tab_cat_prop_to_barplot(
#' tab_cat_prop(ex_survey1[,paste0("b_", 1:3)]))
# tab_cat_prop_to_barplot <-
# 	function(data,
# 			 y_string_wrap_width=40, reverse_y=FALSE, base_font_size=12,
# 			 percent=FALSE, decimals=3) {
#
# 		construct <- unique(data$yvar_group)
# 		construct_label <- unique(data$yvar_group_label)
# 		caption_string <-
# 			paste0(if(length(construct_label)==1L) construct_label,
# 				   " (", construct, ")")
#
#
# 		print(c(construct, construct_label))
# 		if(length(construct)==1L) {
# 			orig_fact <- unique(data$yvar_cat)
# 			p <-
# 				data %>%
# 				dplyr::mutate(labelled::to_character(.data$yvar_cat),
# 					yvar_cat = factor(.data$yvar_cat, levels=rev(orig_fact)),
# 							  yvar_cat = dplyr::na_if(.data$yvar_cat, "NA"),
# 					   yvar_part_label = center_string(.data$yvar_part_label,
# 					   								maxwidth=.env$y_string_wrap_width),
# 					bar_label = round(.data$.n, decimals),
# 					   bar_label = if(percent) paste0(.data$bar_label*100, "%") else .data$bar_label)
# 			p %>%
# 				ggplot2::ggplot(mapping = ggplot2::aes(x = .data$.n,
# 													   y = .data$yvar_part_label,
# 													   fill = .data$yvar_cat,
# 													   label = .data$bar_label)) +
# 				ggplot2::geom_col(position = "stack") +
# 				ggfittext::geom_bar_text(position = "stack", place = "center", na.rm = FALSE) +
# 				ggplot2::scale_x_continuous(expand = c(0, 0),
# 											limits = c(0, 1.05),
# 											labels = if(percent) scales::label_percent() else scales::label_number()
# 											) +
# 				ggplot2::scale_y_discrete(limits=if(reverse_y) rev) +
# 				ggplot2::scale_fill_brewer(type = "seq", direction = -1, na.value="gray90",
# 										   guide=ggplot2::guide_legend(reverse = F)) +
# 				ggplot2::guides(fill=ggplot2::guide_legend(nrow = 1, reverse = T)) +
# 				ggplot2::theme_classic(base_size = base_font_size) +
# 				ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal") +
# 				ggplot2::labs(x=NULL, y=NULL, title=NULL, fill=NULL,
# 							  caption = caption_string)
#
# 		}
# 	}


#' Turn a list of tabulated data frames into plots to be saved.
#'
#' @param l List of data frames from tab_cat_prop()
#' @param y_string_width y_var strng wrap width
#' @param y_reverse Reverse order of y_var? Defaults to true.
#' @param output_dir String.
#' @param output_file_prefix String
#' @param output_ext Format
#' @param plot_width Plot width
#' @param plot_height Plot height
#' @param plot_scale Plot scale
#' @param plot_units units. defaults to cm
#' @param base_font_size Base font size. Defaults to 12
#' @importFrom cowplot align_plots
#' @importFrom rlang set_names
#' @importFrom purrr map map_chr walk2
#' @importFrom tidyr separate
#' @importFrom dplyr group_by group_map pull
#' @importFrom ggplot2 ggsave
#' @return List of plots
#' @export
#'
#' @examples
# tab_cat_list_to_plots <- function(l, y_string_width=40, y_reverse=TRUE,
# 								  output_dir = getwd(), output_file_prefix="", output_ext="png",
# 								  plot_width=33.8, plot_height=19, plot_scale=1, plot_units="cm",
# 								  base_font_size=12) {
# 	p_list <-
# 		l %>%
# 		purrr::map(.f= ~.x %>%
# 			   	tidyr::separate(col = .data$.variable, into=c(".var_group", ".var_part"), sep = "_", remove = FALSE) %>%
# 			   	dplyr::group_by(.data$.var_group) %>%
# 			   	dplyr::group_map(.keep = T, .f = function(.x, ...) .x)) %>%
# 		unlist(recursive = F) %>%
# 		rlang::set_names(nm = purrr::map_chr(.x = ., .f=~dplyr::pull(.x, .data$.var_group) %>% unique())) %>%
# 		purrr::map(.f=~barplot_tab_cat_percent(.data = .x, y_string_wrap_width = y_string_width, reverse_y = y_reverse)) %>%
# 		cowplot::align_plots(plotlist = ., align = "hv", axis = "lrbt")
# 	purrr::walk2(p_list, .y = names(p_list), .f=function(p, construct) {
# 			ggplot2::ggsave(
# 				filename = paste0(output_dir, "/", output_file_prefix, construct, ".", output_ext),
# 				plot = p, scale = plot_scale, width = plot_width, height = plot_height, units = plot_units)
# 		})
# 		p_list
# }




