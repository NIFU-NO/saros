#' Generate Report From Template and Design Frame
#'
#' Generates a report from infoframe object. If specifics have not been set per
#' row in obj$design_frame, will then use global arguments in this function as
#' defaults. Can optionally be pre-processed with add_percentages(),
#' add_analyses(), and add_text().
#'
#' @param obj A list containing df, var_frame and design_frame, as created by
#'   create_infoframe().
#' @param section_divider1,section_divider2 Variable in design_frame that
#'   specifies under which section (level2) and subsection (level3) in the
#'   docx-document that the results will be reported.
#' @param sorting Sorting order for results within a section2
#' @param y_colour_set Character vector with hex colours for the set of y_var.
#' @param drop_na_y,drop_na_x Logical, defaults to TRUE. Whether to drop missing
#'   as a category.
#' @param prefix_number Whether to add the underlying number index for the
#'   categories as a prefix in the printed chart and table labels. Useful for
#'   troubleshooting.
#' @param remove_empty_y_var Logical, defaults to FALSE. As `reporter()` cannot
#'   handle rows in design_frame with missing y_var, will drop these rows if
#'   FALSE (NOT YET IMPLEMENTED). Otherwise aborts.
#' @param drop_duplicates Logical, defaults to FALSE. If there are duplicate
#'   rows in design_frame, drop duplicates silently?
#' @param hide_label_if_less_than Not yet working.
#' @param font_size Font size. Defaults to 8.
#' @param round_digits Integer, defaults to 1. Number of decimals in charts and
#'   tables. Currently not working for charts.
#' @param show_n_chart Logical, defaults to "caption". Should number of cases in
#'   a category/group be added? One of "axis" (part of the chart axis text),
#'   "caption" (summarized and added to the caption), "none" (off) or "auto"
#'   (caption if no variation, otherwise in axis).
#' @param show_n_table Same as show_n_chart, but for the tables.
#' @param show_p_table Show percentage symbol (%) in table header ("axis") or in
#'   the caption ("caption").
#' @param create_barplot1 Logical, defaults to TRUE. Percents in each category.
#' @param create_barplot2 Logical, defaults to TRUE. Percents in each category.
#' @param create_table1 Logical, defaults to FALSE. Percents in each category.
#' @param create_table2 Logical, defaults to TRUE. Means for each category.
#' @param docx Logical, defaults to TRUE. Whether to make Word document.
#' @param pptx Logical, defaults to FALSE Whether to make PowerPoint document.
#' @param xlsx Logical, defaults to FALSE Whether to make Excel document.
#' @param str_pos,str_neg,str_not,str_blank String specifying how relations
#'   between y and x are to be presented in captions for positive, negative, no
#'   relations, and for univariates (str_blank).
#' @param str_table_tag,str_figure_tag String specifying what "Table " and
#'   "Figure " should be displayed as in captions.
#' @param path Directory to save file(s).
#' @param file_prefix Filename prefix (before file extension).
#' @param docx_template_path,pptx_template_path,xlsx_template_path File path
#'    to docx/pptx/xlsx templates.
#' @param template_styles Either a data frame or filepath with two columns:
#'   `surveyreport_style` (must be as is), and `template_style` (must fit styles
#'   in the documents, see NIFUmal_stiler.xlsx for all available).
#'
#' @importFrom dplyr select all_of case_when filter rename across n group_by
#'   ungroup summarize mutate if_else count add_count n_distinct arrange
#'   rename_with %>%
#' @importFrom rlang abort warn .data .env  arg_match set_names
#' @importFrom purrr map_chr map2 map pmap map_lgl
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer unite pivot_wider
#' @importFrom labelled set_variable_labels var_label to_character val_label
#' @importFrom haven as_factor
#' @importFrom mschart body_add_chart chart_labels_text
#' @importFrom officer body_add_par body_add_table add_slide table_stylenames
#'   ph_with ph_location_fullsize ph_location_type fp_text add_sheet

#' @import officer
#' @return design_frame
#' @export
#'
#' @examples
reporter <-
	function(obj,
			 section_divider1 = NULL,
			 section_divider2 = NULL,
			 sorting = NULL,
			 y_colour_set=NULL, drop_na_y=TRUE, drop_na_x=TRUE, prefix_number=FALSE, remove_empty_y_var=FALSE, drop_duplicates=TRUE,
			 hide_label_if_less_than=0, font_size=8, round_digits=1,
			 show_n_chart=c("caption", "axis", "auto", "none"),
			 show_n_table=c("caption", "axis", "auto", "none"),
			 show_p_table = c("caption", "axis", "auto", "none"),
			 create_barplot1=TRUE, create_barplot2=FALSE, create_table1=TRUE, create_table2=FALSE,

			 docx=TRUE, pptx=FALSE, xlsx=FALSE,
			 str_pos = " <+> ", str_neg = " <-> ", str_not = " <0> ", str_blank = 0,

			 str_table_tag="Tabell ", str_figure_tag="Figur ",

			 path = getwd(), file_prefix="uni",
			 docx_template_path = "template/NIFUmal_tom.docx",
			 pptx_template_path = "template/NIFUmal_tom.pptx",
			 xlsx_template_path = "template/NIFUmal_tom.xlsx",
			 template_styles = "template/NIFUmal_stiler.xlsx") {



		s <- get_template_styles(template_styles=template_styles)

		section_options <- c('y_group_label', 'y_group', 'y_var', 'y_label', 'y_type',
							 'x_group_label', 'x_group', 'x_var', 'x_label', 'x_type', 'section1', 'section2')
		sorting_options <- c(section_options, "y_pos", "x_pos",
							 "top1", "average", "sum_high_cat", "sum_low_cat", "est", "pval")
		show_n_options <- c("caption", "axis", "auto", "none")
		show_n_chart <- rlang::arg_match(show_n_chart)
		show_n_table <- rlang::arg_match(show_n_table)
		show_p_table <- rlang::arg_match(show_p_table)

		added_parts <- c()

		# Early abort, warnings and messages.
		obj <- assert_valid_infoframe(obj=obj)
		if(drop_duplicates) {
			design_frame <- unique(design_frame)
		}

		if(is.null(design_frame[["y_label"]])) {
			design_frame[["y_label"]] <- design_frame[["y_var"]]
			added_parts <- c(added_parts, "y_label")
		}
		if(is.null(design_frame[["y_group"]])) {
			design_frame[["y_group"]] <- design_frame[["y_var"]] %>% purrr::map(.x = ., .f = ~paste0(.,collapse=","))
			added_parts <- c(added_parts, "y_group")
		}
		if(is.null(design_frame[["y_group_label"]])) {
			design_frame[["y_group_label"]] <- design_frame[["y_var"]] %>% purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
			added_parts <- c(added_parts, "y_group_label")
		}
		if(is.null(design_frame[["x_var"]])) {
			design_frame[["x_var"]] <- NA_character_
			added_parts <- c(added_parts, "x_var")
		}
		if(is.null(design_frame[["x_label"]])) {
			design_frame[["x_label"]] <- design_frame[["x_var"]]
			added_parts <- c(added_parts, "x_label")
		}
		if(is.null(design_frame[["x_group"]])) {
			design_frame[["x_group"]] <- design_frame[["x_var"]] %>%
				purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
			added_parts <- c(added_parts, "x_group")
		}
		if(is.null(design_frame[["x_group_label"]])) {
			design_frame[["x_group_label"]] <- design_frame[["x_var"]] %>%
				purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
			added_parts <- c(added_parts, "x_group_label")
		}

		if(is.null(design_frame[["section1"]])) {
			if(!is.null(section_divider1) && (length(section_divider1) != 1L || !section_divider1 %in% section_options)) {
				rlang::abort(c(i="Global argument `section_divider1` must be a string length 1, and one of ",
							   rlang::quo_text(section_options),
							   i=" or NULL when column 'section1' in design_frame is not provided."))
				}
			design_frame[["section1"]] <- if(!is.null(section_divider1)) design_frame[[section_divider1]] else NA_character_
			added_parts <- c(added_parts, "section1")
			}
		if(is.null(design_frame[["section2"]])) {
			if(!is.null(section_divider2) && (length(section_divider2) != 1L || !section_divider2 %in% section_options)) {
				rlang::abort(c(i="Global argument `section_divider2` must be a string length 1, and one of ",
							   rlang::quo_text(section_options),
							   i=" or NULL when column 'section2' in design_frame is not provided."))
			}
				design_frame[["section2"]] <- if(!is.null(section_divider2)) design_frame[[section_divider2]] else NA_character_
			added_parts <- c(added_parts, "section2")
		}

		if(is.null(design_frame[["sort_order"]])) {
			if(!is.null(sorting) && (length(sorting)!=1L || !sorting %in% sorting_options)) {
				rlang::abort(c(i="Global argument `sorting` must be a string length 1, and one of ",
							   rlang::quo_text(sorting_options),
							   i=" or NULL when column 'sort_order' in design_frame is not provided."))
			}
			design_frame[["sort_order"]] <- if(!is.null(sorting)) sorting else "y_var"
			added_parts <- c(added_parts, "sort_order")
		}

			# if(is.null(design_frame[["y_colour_set"]])) {
			# 	if(!is.null(y_colour_set) && !all(is_colour(design_frame[["y_colour_set"]]))) {
			# 		rlang::abort(paste0("Global argument `y_colour_set=`", " must be a set of hex-colours, or NULL when variable 'y_colour_set' in design_frame is not provided."))
			# 	}
			# 	y_colour_set_replacement <- if(!is.null(y_colour_set)) y_colour_set else get_colour_set(n_colours_needed=20, user_colour_set=built_in_colour_set)
			# 	design_frame[["y_colour_set"]] <- purrr::map(1:nrow(design_frame), function(i) y_colour_set_replacement)
			# 	added_parts <- c(added_parts, "y_colour_set")
			# }
			#
			# if(is.null(design_frame[["x_colour_set"]])) {
			# 	if(!is.null(design_frame[["x_colour_set"]]) && !all(is_colour(design_frame[["x_colour_set"]]))) {
			# 		rlang::abort(paste0("Global argument `x_colour_set=`", " must be a set of hex-colours, or NULL when variable 'x_colour_set' in design_frame is not provided."))
			# 	}
			# 	x_colour_set_replacement <- if(!is.null(x_colour_set)) x_colour_set else get_colour_set(n_colours_needed=20, user_colour_set=built_in_colour_set)
			# 	design_frame[["x_colour_set"]] <- purrr::map(1:nrow(design_frame), function(i) x_colour_set_replacement)
			# 	added_parts <- c(added_parts, "x_colour_set")
			# }
			# Return warnings
		if(length(added_parts)>0L) rlang::warn(c("Added parts that were missing in design_frame: ",
												 rlang::quo_text(added_parts)))

		design_frame <- check_options(df = design_frame, df_var = "est", global_default = NA_real_, options = numeric())
		design_frame <- check_options(df = design_frame, df_var = "pval", global_default = NA_real_, options = numeric())
		design_frame <- check_options(df = design_frame, df_var = "drop_na_y", global_default = drop_na_y, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "drop_na_x", global_default = drop_na_x, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "prefix_number", global_default = prefix_number, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "hide_label_if_less_than", global_default = hide_label_if_less_than, options = numeric())
		design_frame <- check_options(df = design_frame, df_var = "font_size", global_default = font_size, options = 0:72)
		design_frame <- check_options(df = design_frame, df_var = "round_digits", global_default = round_digits, options = -3:3)
		design_frame <- check_options(df = design_frame, df_var = "show_n_chart", global_default = show_n_chart, options = show_n_options)
		design_frame <- check_options(df = design_frame, df_var = "show_p_table", global_default = show_p_table, options = show_n_options)
		design_frame <- check_options(df = design_frame, df_var = "create_barplot1", global_default = create_barplot1, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "create_barplot2", global_default = create_barplot2, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "create_table1", global_default = create_table1, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "create_table2", global_default = create_table2, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "pptx", global_default = pptx, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "docx", global_default = docx, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "xlsx", global_default = xlsx, options = c(TRUE,FALSE))


		df <- .remove_special_chars_in_labels(obj$df)



		doc <- officer::read_docx(path = docx_template_path)
		doc_dim <- officer::docx_dim(doc)
		img_width <- doc_dim$page[["width"]] - doc_dim$margins[["left"]] - doc_dim$margins[["right"]]
		img_height_max <- doc_dim$page[["height"]] - doc_dim$margins[["top"]] - doc_dim$margins[["bottom"]] - 2
		ppt <- officer::read_pptx(path = pptx_template_path)
		xls <- officer::read_xlsx(path = xlsx_template_path)


		create_report_entry <-
			function(rownumber,
					 y_var, y_label, y_group, y_group_label, y_colour_set, drop_na_y,
					 x_var, x_label, x_group, x_group_label, x_colour_set, drop_na_x,
					 est, pval, section1, section2, sort_order,
					 prefix_number, hide_label_if_less_than, font_size, round_digits, show_n_chart,
					 create_barplot1, create_barplot2, create_table1, create_table2,
					 pptx, docx, xlsx, ...) {



				# Early abort, warnings and messages. These could be moved even further up front?


				problem_singular <-
					df %>%
					dplyr::select(dplyr::all_of(.env$y_var)) %>%
					dplyr::select(dplyr::all_of(names(.)[purrr::map_lgl(., .f = ~all(is.na(.x)))])) %>%
					names()
				if(length(problem_singular)>0L) {
					rlang::warn(c("There is only NA in ", rlang::quo_text(problem_singular), ". Will skip."))
					return()
				}

				if(is.null(x_var)) x_var <- NA_character_
				if(!is.na(x_var)) {

					if(!is.character(x_var) | !length(x_var) %in% 0:1) {

						rlang::abort(c("Current version only accepts x_var as a single string per row.",
									   i=paste0("When y_var is ", paste0(y_var, collapse=","), ","),
									   x=paste0("there is a problem with x_var: ", paste0(x_var, collapse=","))))
					}
					if(any(!x_var %in% names(df))) {
						rlang::abort(message = paste0("Following x_var not found in df:", x_var))
					}

				}

				if(any(is.na(y_colour_set)) || !all(is_colour(y_colour_set))) {
					rlang::warn("No valid colours provided in ",
								rlang::quo_text(y_colour_set), ". Using default.") # SHOULD THIS NOT ALREADY SET THESE COLOURS??
				}

				if(length(y_var) == 1L & length(y_label) == 1L) y_group_label <- paste0(y_group_label, " - ", y_label)



				# title <- dplyr::case_when(is.na(.env$x_var) || is.na(.env$est) || is.na(.env$pval) ~ .env$y_group_label,
				# 						  .env$est > 0 & .env$pval<.05 ~ paste0(.env$y_group_label, .env$str_pos, paste0(.env$x_label, collapse="_")),
				# 						  .env$est < 0 & .env$pval<.05 ~ paste0(.env$y_group_label, .env$str_neg, paste0(.env$x_label, collapse="_")),
				# 						  .env$pval >= .05 | .env$est == 0 ~ paste0(.env$y_group_label, .env$str_not, paste0(.env$x_label, collapse="_")))

				tab <-
					df %>%
					dplyr::select(dplyr::all_of(c(.env$y_var), if(!is.na(.env$x_var)) .env$x_var)) %>%
					labelled::set_variable_labels(.labels = rlang::set_names(.env$y_label, nm=unname(.env$y_var)) %>% as.list()) %>%
					rlang::set_names(nm = ifelse(nchar(labelled::var_label(., unlist = T))==0L | colnames(.) %in% .env$x_var,
												 colnames(.), labelled::var_label(., unlist = T)))

				if(!is.na(x_var) && length(y_var)>1) {
					rlang::warn("When x_var is specified, barcharts are currently limited to single-item batteries. Omitting barchart1")
				} else {


					tab_long1 <-
						tab %>%
						{if(is.na(x_var)) tidyr::pivot_longer(., cols = dplyr::all_of(.env$y_label), names_to = "var", values_to = "val") else {
							dplyr::rename(., val={y_label}, var={x_var})}} %>%
						dplyr::mutate(var = labelled::to_character(.data$var),
									  category = labelled::to_character(.data$val),
									  val = as.integer(haven::as_factor(.data$val))) %>% # Problem here if first variable lacks a category
						{if(drop_na_y) dplyr::filter(., !is.na(.data$category)) else .} %>%
						{if(!drop_na_y) dplyr::mutate(., category = rlang::`%|%`(.data$category, "<NA>")) else .} %>%
						{if(drop_na_y) dplyr::filter(., !is.na(.data$val)) else .} %>%
						{if(drop_na_x & !is.na(x_var)) dplyr::filter(., !is.na(.data$var)) else .} %>%
						{if(!drop_na_x | is.na(x_var)) dplyr::mutate(., var = rlang::`%|%`(.data$var, "<NA>")) else .} %>%
						dplyr::add_count(.data$var, name = "n_per_var") %>%
						dplyr::count(.data$var, .data$n_per_var, .data$val, .data$category, name = "n_per_var_val") %>%
						dplyr::mutate(percent = round(.data$n_per_var_val/.data$n_per_var, 3)) %>%
						{if(show_n_chart=="axis" |
							(show_n_chart=="auto" &
							 dplyr::n_distinct(.[["n_per_var"]]) > 1L)) tidyr::unite(., col = "var", c(.data$var, .data$n_per_var), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
								dplyr::mutate(var=paste0(.data$var, ")")) else .} %>%
						{if(prefix_number) tidyr::unite(., col = "category", c(.data$val, .data$category), sep = ": ", remove = FALSE) else .} %>%
						dplyr::arrange(.data$val) %>%
						dplyr::mutate(category = factor(.data$category, levels = unique(.data$category), ordered = TRUE)) %>% # Problem here if first variable lacks a category
						dplyr::arrange(.data$var, .data$val)


					### THIS WILL FAIL IF NO tab_long1 HAS BEEN CREATED BUT NEEDED LATER!
					# caption_suffix <- paste0(". ", title,
					# 						 if(show_n_chart=="caption" |
					# 						    (show_n_chart == "auto" & dplyr::n_distinct(range(tab_long1$n_per_var))==1L)) {
					# 						 	paste0(" N=[", paste0(unique(range(tab_long1$n_per_var)), collapse="-"), "]")
					# 						 })

					# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
					# {if(sort=="sum_upper_categories") dplyr::group_by(., var) %>%
					# 		dplyr::mutate(sort_var = ifelse(category = )) %>%
					# 		dplyr::arrange(if(desc) dplyr::desc(sort_var) else sort_var, val) else .} %>%
					# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
					# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
					#"average", "sum_upper_categories", "alphabetical", "sum_lower_categories", "significance",

					if(create_barplot1) {


						barplot1 <- ms_percentbar(df = tab_long1, y_var = "percent", x_var="var",
												  means = FALSE, f_size = font_size, user_colours=y_colour_set)

						if(docx) {
							n_vars1 <- length(unique(barplot1[["data"]][["var"]]))
							doc <<-
								mschart::body_add_chart(x = doc, width = img_width, height = min(img_height_max, n_vars1*7/25+150/100), style = s$figure,
														chart = mschart::chart_labels_text(x = barplot1, values=officer::fp_text(font.size=5))) %>%
								officer::body_add_par(x = ., value = gen_caption(tab_long = tab_long1, str_tag = str_figure_tag, prefix_letter = "a"),
													  style = s$figure_caption) %>%
								officer::body_add_par(x = ., value = "", style = s$paragraph)
						}

						if(pptx) {
							ppt <<-
								officer::add_slide(ppt, layout = s$pptx_layout, master = s$pptx_master) %>%
								officer::ph_with(x = ., value = barplot1, location = officer::ph_location_fullsize()) %>%
								officer::ph_with(x = ., value = gen_caption(tab_long = tab_long1, str_tag = str_figure_tag, prefix_letter = "a"),
												 location = officer::ph_location_type(type = "ftr"))
						}
					}


					if(create_table1) {
						table1 <-
							tab_long1 %>%
							dplyr::select(-dplyr::all_of("val", "n_per_var_val")) %>%
							dplyr::mutate(percent = .data$percent*100) %>%
							{if(show_n_table != "axis") dplyr::select(., -dplyr::all_of("n_per_var")) else dplyr::rename(., N=.data$n_per_var)} %>%
							dplyr::rename_with(.cols = dplyr::all_of("var"), .fn = ~ y_group_label) %>%
							tidyr::pivot_wider(names_from = all_of("category"), values_from = all_of("percent")) %>%
							{if(show_p_table == "axis") dplyr::rename_with(., .cols = -1, ~ paste0(., " (%)")) else .}

						if(xlsx) {
							table1 <<-
								officer::add_sheet(x = table1, label = paste0(y_var, if(!is.na(x_var)) x_var, collapse="_"))
						}

						df_display1 <-
							table1 %>%
							dplyr::mutate(dplyr::across(dplyr::all_of(names(.)[purrr::map_lgl(.x = ., .f = ~all(is.na(x)))]),
														~gsub("\\.", ",", round(replace(., is.na(.), str_blank), digits = round_digits))))

						if(docx) {

							doc <<-
								officer::body_add_par(x = doc, value = gen_caption(tab_long = tab_long1, str_tag = str_table_tag, prefix_letter = "a"), style = s$table_caption) %>%
								officer::body_add_table(x = .,value = df_display1, style = s$table_body, header = TRUE, alignment = c("l", rep("r", ncol(table1)-1)),
														stylenames = officer::table_stylenames(stylenames = rlang::set_names(x=rep(s$table_body, ncol(table1)), nm=colnames(table1)))) %>%
								officer::body_add_par(x = ., value = "", style = s$paragraph)
						}
						if(pptx) {

							ppt <<-
								officer::add_slide(ppt, layout = s$pptx_layout, master = s$pptx_master) %>%
								officer::ph_with(x = ., value = df_display1,
												 location = officer::ph_location_type(), alignment = c("l", rep("c", ncol(table1)-1))) %>%
								officer::ph_with(x = ., value = gen_caption(tab_long = tab_long1, str_tag = str_table_tag, prefix_letter = "a"),
												 location = officer::ph_location_type(type = "ftr"))
						}

					}
				}

				if(!is.na(x_var) & create_barplot2) {

					tab_long2 <-
						tab %>%
						dplyr::mutate(dplyr::across(.cols = -dplyr::all_of(x_var), .fns = ~as.integer(haven::as_factor(.)))) %>%
						dplyr::mutate(dplyr::across(.cols = dplyr::all_of(x_var), .fns = ~labelled::to_character(.))) %>%
						dplyr::group_by(category=.data[[x_var]]) %>%
						dplyr::summarize(dplyr::across(.cols = dplyr::all_of(y_label), .fns = ~round(mean(., na.rm=TRUE), digits = round_digits+1)), n_per_var=dplyr::n()) %>%
						dplyr::ungroup() %>%
						tidyr::pivot_longer(cols = dplyr::all_of(y_label), names_to = "var", values_to = "val")  %>%
						{if(drop_na_y) dplyr::filter(., !is.na(.data$category)) else dplyr::mutate(., category = rlang::`%|%`(.data$category, "<NA>"))} %>%
						{if(drop_na_y) dplyr::filter(., !is.na(.data$val)) else .} %>%
						{if(drop_na_x) dplyr::filter(., !is.na(.data$var)) else dplyr::mutate(., var = rlang::`%|%`(.data$var, "<NA>"))} %>%
						{if(show_n_chart=="axis" | (show_n_chart=="auto" & dplyr::n_distinct(.[["n_per_var"]])>1L)) {
							tidyr::unite(., col = "var", all_of(c("var", "n_per_var")), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
								dplyr::mutate(var = paste0(.data$var, ")"))} else .} %>%
						dplyr::arrange(.data$var, .data$category)
					barplot2 <- ms_percentbar(df = tab_long2, y_var = "val", x_var="var",
											  user_colours=x_colour_set, f_size = font_size, means=TRUE)

					if(docx) {
						n_vars2 <- dplyr::n_distinct(barplot2[["data"]][["var"]])
						doc <<-
							mschart::body_add_chart(x = doc, width = img_width, height = min(img_height_max, n_vars2*7/25+150/100), chart = barplot2, style = s$figure) %>%
							officer::body_add_par(x = ., value = gen_caption(tab_long = tab_long2, str_tag = str_figure_tag, prefix_letter = "b"), style = s$figure_caption)
					}

					if(pptx) {
						ppt <<-
							officer::add_slide(ppt, layout = , master = s$pptx_master) %>%
							officer::ph_with(x = ., value = barplot2, location = officer::ph_location_fullsize()) %>%
							officer::ph_with(x = ., value = gen_caption(tab_long = tab_long2, str_tag = str_figure_tag, prefix_letter = "b"),
											 location = officer::ph_location_type(type = "ftr"))
					}
				}

				### Three-level table for layered chart (manually constructed)
				if(!is.na(x_var) & length(y_var)>1L & create_table2) {
					table2 <-
						tab %>%
						tidyr::pivot_longer(cols = dplyr::all_of(.env$y_label), names_to = "var", values_to = "val") %>%
						dplyr::mutate(var = labelled::to_character(.data$var),
									  category = labelled::to_character(.data$val),
									  val = as.integer(haven::as_factor(.data$val))) %>% # Problem here if first variable lacks a category
						{if(.env$drop_na_y) dplyr::filter(., !is.na(.data$category)) else dplyr::mutate(., category = rlang::`%|%`(.data$category, "<NA>"))} %>%
						{if(.env$drop_na_y) dplyr::filter(., !is.na(.data$val)) else .} %>%
						{if(.env$drop_na_x & !is.na(.env$x_var)) dplyr::filter(., !is.na(.data$var)) else dplyr::mutate(., var = rlang::`%|%`(.data$var, "<NA>"))} %>%
						dplyr::add_count(.data$var, .data[[x_var]], name = "n_per_var_var2") %>%
						dplyr::count(.data$var, .data[[x_var]], .data$n_per_var_var2, .data$val, .data$category, name = "n_per_var_val_var2") %>%
						dplyr::mutate(percent = round(.data$n_per_var_val_var2/.data$n_per_var_var2*100, round_digits)) %>%
						{if(.env$show_n_chart=="axis" | (.env$show_n_chart=="auto" & dplyr::n_distinct(.[["n_per_var_var2"]]) > 1L)) {
							tidyr::unite(., col = "var", c(.data$var, .data$n_per_var), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
								dplyr::mutate(var=paste0(.data$var, ")"))} else .} %>%
						{if(.env$prefix_number) tidyr::unite(., col = "category", c(.data$val, .data$category), sep = ": ", remove = FALSE) else .} %>%
						dplyr::arrange(.data$val) %>%
						dplyr::mutate(category = factor(.data$category, levels = unique(.data$category), ordered = T)) %>% # Problem here if first variable lacks a category
						{if(.env$show_p_table == "axis") dplyr::mutate(., category = paste0(.data$category, " (%)")) else .} %>%
						{if(.env$show_n_table == "axis") dplyr::rename(., N = .data$n_per_var_var2) else dplyr::select(., -dplyr::all_of("n_per_var_var2"))} %>%
						dplyr::arrange(dplyr::all_of(x_var), .data$var, .data$val) %>%
						dplyr::select(-dplyr::all_of(c("n_per_var_val_var2", "val")))  %>%
						tidyr::pivot_wider(names_from = .data$category, values_from = .data$percent) %>%
						dplyr::relocate(dplyr::all_of(c(x_var, "var"))) %>%
						dplyr::rename_with(.cols = dplyr::all_of("var"), .fn = ~ y_group_label)

					if(docx) {
						## Can this line be generalized?
						df_display2 <-
							table2 %>%
							dplyr::mutate(dplyr::across(dplyr::all_of(colnames(.)[purrr::map_lgl(.x = ., .f = ~all(is.na(.x)))]),
														~gsub("\\.", ",", round(replace(., is.na(.), str_blank),
																				digits = round_digits))))

						doc <<-
							officer::body_add_par(x = doc, value = gen_caption(tab_long = tab_long2, str_tag = str_table_tag, prefix_letter = "b"), style = s$table_caption) %>%
							officer::body_add_table(x = .,value = df_display2, style = s$table_body, header = TRUE, alignment = c("l", rep("r", ncol(table2)-1)),
													stylenames = officer::table_stylenames(stylenames = rlang::set_names(rep(s$table_body, ncol(table2)), colnames(table2)))) %>%
							officer::body_add_par(x = ., value = "", style = s$paragraph)
					}

					if(xlsx) {
						xls <<-
							officer::add_sheet(x = xls, label = paste0(rownumber, paste0(y_var, collapse="_"), if(!is.na(x_var)) x_var, sep="x"))
					}

					# colour_palette3 <-
					# 	get_colour_set(n_colours_needed = n_distinct(tab_long3$category),
					# 					 user_colour_set = if(battery %in% nominal_vars) y_colour_set_nominal else y_colour_set) %>%
					# 	rlang::set_names(nm=unique(tab_long3$category)) %>%
					# 	.[1:dplyr::n_distinct(tab_long3$category)]
					#
					# fp_text_settings3 <-
					# 	lapply(colour_palette3, function(color) {
					# 		officer::fp_text(font.size=font_size, color=hex_bw(color))}) %>%
					# 	.[1:dplyr::n_distinct(tab_long3$category)]

					# chart3 <-
					# 	mschart::ms_barchart(data = tab_long2, y = "val", x="var", group = "category") %>%
					# 	mschart::chart_data_labels(x=., position = "ctr", show_val = T) %>%
					# 	mschart::chart_settings(x = .,   dir="horizontal") %>%
					# 	mschart::chart_labels(x = ., title = "", xlab = "", ylab = "") %>%
					# 	mschart::chart_labels_text(x = ., values=fp_text_settings2) %>%
					# 	mschart::chart_theme(x = ., legend_position = "b",
					# 						 axis_text_x = officer::fp_text(font.size = font_size),
					# 						 axis_ticks_y = officer::fp_border(style = "none"),
					# 						 axis_ticks_x = officer::fp_border(style = "none"),
					# 						 grid_major_line_x = officer::fp_border(style = "none"),
					# 						 grid_major_line_y = officer::fp_border(style = "none"),
					# 						 grid_minor_line_x = officer::fp_border(style = "none"),
					# 						 grid_minor_line_y = officer::fp_border(style = "none"),
					# 						 legend_text = officer::fp_text(font.size = font_size)) %>%
					# 	mschart::chart_data_fill(x=., values=colour_palette2) %>%
					# 	mschart::chart_data_stroke(x=., values=colour_palette2) %>%
					# 	mschart::chart_ax_y(x = ., limit_min = 0) %>%
					# 	mschart::chart_ax_x(x = ., minor_tick_mark = "none")
				}
				mget(x = ls())
			}

		design_frame <-
			design_frame %>%
			dplyr::arrange(.data[["section1"]], .data[["section2"]], .data[["sort_order"]]) %>%
			dplyr::mutate(rownumber = 1:nrow(.))

		progress_installed <- requireNamespace("progress", quietly = TRUE)
		if(progress_installed) pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nrow(design_frame))
		if(progress_installed) pb$tick(0)




		doc <<-
			officer::body_add_par(x = doc, value = "Results", style = s$heading_1) %>%
			officer::body_add_par(x = ., value = "", style = )

		out <- purrr::map(unique(design_frame[["section1"]]), .f = function(sec1) {
			if(!is.na(sec1)) {
				doc <<-
					officer::body_add_par(x = doc, value = if(!is.na(sec1)) sec1 else "", style = s$heading_2) %>%
					officer::body_add_par(x = ., value = "", style = s$paragraph)
			}

			purrr::map(unique(design_frame[design_frame[["section1"]] == sec1, "section2"]), .f = function(sec2) {
				if(!is.na(sec2)) {
					doc <<-
						officer::body_add_par(x = doc, value = if(!is.na(sec2)) sec2 else "", style = s$heading_3) %>%
						officer::body_add_par(x = ., value = "", style = s$paragraph)
				}
				input <-
					design_frame %>%
					{if(!is.na(sec1)) dplyr::filter(., .data$section1 == sec1) else dplyr::filter(., is.na(.data$section1))} %>%
					{if(!is.na(sec2)) dplyr::filter(., .data$section2 == sec2) else dplyr::filter(., is.na(.data$section2))}

				purrr::pmap(.l = input, .f = function(...) {
					if(progress_installed & exists("pb")) pb$tick()
					create_report_entry(...)
				})
			})
		})
		if(pptx) print(x = ppt, target = file.path(path, paste0(file_prefix, ".pptx")))
		if(xlsx) print(x = xls, target = file.path(path, paste0(file_prefix, ".xlsx")))
		if(docx) print2.rdocx(x=doc, target = file.path(path, paste0(file_prefix, ".docx")))

		list(df=obj$df, var_frame=obj$var_frame, design_frame=obj$design_frame, report=out)
	}



#' Not sure what this is for yet
#'
#' Some function for reverse unlabelling?
#'
#' @param var Vector
#'
#' @return A factor.
#' @export
#'
#' @examples
reverse_unlabeling <- function(var) {
	val <- as.integer(gsub("\\[([0-9]*)\\].*", "\\1", var))
	label <- gsub("\\[[0-9]*\\] (.*)", "\\1", var)
	label <- gsub("^[[:space:]]*|[[:space:]]*$", "", label)
	print(unique(val))
	print(unique(label))

	factor(val, levels = unique(val)[!is.na(unique(val))],
		   labels=unique(label)[!is.na(unique(label))])
}
