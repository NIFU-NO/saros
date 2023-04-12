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
#' @param remove_empty_y_var Logical, defaults to FALSE. As `reporter()` cannot
#'   handle rows in design_frame with missing y_var, will drop these rows if
#'   FALSE (NOT YET IMPLEMENTED). Otherwise aborts.
#' @param drop_duplicates Logical, defaults to FALSE. If there are duplicate
#'   rows in design_frame, drop duplicates silently?
#' @param filepath Directory to save file(s).
#' @param docx_template_path,pptx_template_path,xlsx_template_path File path 
#'    to docx/pptx/xlsx templates.
#' @param template_styles Either a data frame or filepath with two columns:
#'   `surveyreport_style` (must be as is), and `template_style` (must fit styles
#'   in the documents, see NIFUmal_stiler.xlsx for all available).
#'
#' @importFrom dplyr select all_of case_when filter rename across n group_by
#'   ungroup summarize mutate if_else count add_count n_distinct arrange
#'   rename_with %>%
#' @importFrom rlang abort warn .data .env  set_names
#' @importFrom purrr map_chr map2 map pmap map_lgl
#' @importFrom mschart body_add_chart chart_labels_text
#' @importFrom officer body_add_par body_add_table add_slide table_stylenames
#'   ph_with ph_location_fullsize ph_location_type fp_text add_sheet
#'   read_xlsx read_docx read_pptx

#' @import officer
#' @return design_frame

#'
#' @examples
#' ex_survey1_inf_new <- add_barchart_mschart(ex_survey1_inf)
#' ex_survey1_inf_new <- add_analysis(ex_survey1_inf_new)
#' ex_survey1_inf_new <- add_caption(ex_survey1_inf_new)
#' ex_survey1_inf_new_filepath <- file.path(tempfile(fileext = ".docx"))
#' ex_report1 <- create_report(ex_survey1_inf_new, 
#'                             filepath=ex_survey1_inf_new_filepath)

create_report <- 
	function(obj, 
			 section_divider1 = NULL,
			 section_divider2 = NULL,
			 sorting = NULL, 
			 remove_empty_y_var=FALSE, 
			 drop_duplicates=TRUE,
			 filepath = "tmp.docx", 
			 docx_template_path = "inst/template/NIFUmal_tom.docx",
			 pptx_template_path = "inst/template/NIFUmal_tom.pptx",
			 xlsx_template_path = "inst/template/NIFUmal_tom.xlsx",
			 template_styles = "inst/template/NIFUmal_stiler.xlsx") {
		
		s <- get_template_styles(template_styles=template_styles)
		ext <- 
			if(is.null(filepath) || grepl("\\.docx$", filepath)) {
				"w"
			} else if(grepl("\\.pptx$", filepath)) {
				"p" 
			} else if(grepl("\\.xlsx", filepath)) {
				"e"
			} else rlang::abort(c("Invalid filepath:",
							   i="filepath must be a single path that ends in docx, pptx or xlsx.",
							   x=paste0("filepath is now set as ", rlang::quo_text(filepath))))
			
		
		section_options <- c('y_group_label', 'y_group', 'y_var', 'y_label', 'y_type',
							 'x_group_label', 'x_group', 'x_var', 'x_label', 'x_type', 'section1', 'section2')
		sorting_options <- c(section_options, "y_pos", "x_pos",
							 "top1", "average", "sum_high_cat", "sum_low_cat", "estimate", "p.value")
		
		added_parts <- c()
		
		# Early abort, warnings and messages.
		obj <- assert_valid_infoframe(obj=obj)
		if(drop_duplicates) {
			design_frame <- unique(obj$design_frame)
		} else design_frame <- obj$design_frame
		
		if(is.null(design_frame[["x_var"]])) {
			design_frame[["x_var"]] <- NA_character_
			added_parts <- c(added_parts, "x_var")
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
		
		
		if(is.null(design_frame[["rownumber"]])) {
			design_frame[["rownumber"]] <- seq_len(nrow(design_frame))
			added_parts <- c(added_parts, "rownumber")
		}
		
		if(length(added_parts)>0L) rlang::warn(c("Added parts that were missing in design_frame: ", 
												 rlang::quo_text(added_parts)))
		
		base_vars <- c("section1", "section2", "y_var", "x_var", "rownumber")
		reportable <- grep("^interpret_|^barchart_|^table_|^caption_", names(design_frame), value = TRUE)
		report_parts <- names(design_frame)[names(design_frame) %in% reportable]
		
		design_frame <-
			design_frame %>%
			dplyr::arrange(.data[["section1"]], .data[["section2"]], .data[["sort_order"]]) %>%
			dplyr::select(dplyr::all_of(c(base_vars, report_parts)))
			
		print(names(design_frame))
		
		##########################
		
		if(ext=="w") {
			doc <- 
				officer::read_docx(path = docx_template_path) %>%
				officer::body_add_par(x = ., value = "Results", style = s$heading_1) %>%
				officer::body_add_par(x = ., value = "", style = s$paragraph)
			doc_dim <- officer::docx_dim(doc)
			img_width <- doc_dim$page[["width"]] - doc_dim$margins[["left"]] - doc_dim$margins[["right"]]
			img_height_max <- doc_dim$page[["height"]] - doc_dim$margins[["top"]] - doc_dim$margins[["bottom"]] - 2

			
		} else if(ext=="p") {
			ppt <- 
				officer::read_pptx(path = pptx_template_path) %>%
				officer::add_slide(x = ., layout = s$pptx_layout, master = s$pptx_master) %>%
				officer::ph_with(x = ., value = "Results", location = officer::ph_location_type(type = "title"))
		} else if(ext=="e") {
			xls <- officer::read_xlsx(path = xlsx_template_path)
		}
		
		create_report_entry <- 
			function(rownumber, y_var, x_var, section1, section2, sort_order, ...) {

				rp <- rlang::dots_list(... = ..., .named = TRUE, .ignore_empty = "none", .homonyms = "error")
				# print(names(rp))
				rp <- rp[names(rp) %in% report_parts] # Unnecessary
				
				for(part in names(rp)) {
					if(length(rp[[part]])>0L) {
						if(ext=="w") {
							if(vctrs::vec_is(x = rp[[part]], ptype = character(), size = 1L) && 
							   grepl("caption_", names(rp[part]))) {
								doc <<- 
									officer::body_add_par(x = doc, value = rp[[part]], style = s$figure_caption) %>%
									officer::body_add_par(x = ., value = "", style = s$paragraph)
							}
							if(vctrs::vec_is(x = rp[[part]], ptype = character(), size = 1L) && 
							   grepl("interpret_", names(rp[part]))) {
								doc <<- 
									officer::body_add_par(x = doc, value = rp[[part]], style = s$paragraph)
							}
							if(class(rp[[part]])[1]=="ms_barchart" && grepl("barchart_", names(rp[part]))) {
								n_vars <- length(unique(rp[[part]][["data"]][["var"]]))
								doc <<- 
									mschart::body_add_chart(x = doc, 
															width = img_width,
															height = min(img_height_max, n_vars*7/25+150/100), 
															style = s$figure,
															chart = mschart::chart_labels_text(x = rp[[part]], 
																							   values=officer::fp_text(font.size=5))) %>%
									officer::body_add_par(x = ., value = "", style = s$paragraph)
							}
							if(is.data.frame(rp[[part]]) && grepl("table_", names(rp[part]))) {
								table <-
									rp[[part]] %>%
									dplyr::mutate(dplyr::across(dplyr::all_of(names(.)[purrr::map_lgl(.x = ., .f = ~all(is.na(x)))]),
																~gsub("\\.", ",", round(replace(., is.na(.), str_blank), digits = round_digits))))
								styles <- rlang::set_names(x=rep(s$table_body, ncol(table)), nm=colnames(table))
								doc <<-
									officer::body_add_table(x = doc, value = table, 
															style = s$table_body, header = TRUE, alignment = c("l", rep("r", ncol(table)-1)),
															stylenames = officer::table_stylenames(stylenames = styles)) %>%
									officer::body_add_par(x = ., value = "", style = s$paragraph)
							}
						} else if(ext=="p") {
							ppt <<-	officer::add_slide(ppt, layout = s$pptx_layout, master = s$pptx_master) 
							
							if(vctrs::vec_is(x = rp[[part]], ptype = character(), size = 1L) && 
							   grepl("caption_|interpret_", names(rp[part]))) {
								ppt <<-
									officer::ph_with(x = ppt, value = rp[[part]], 
													 location = officer::ph_location_type(type = "ftr"))
							}
							if(class(rp[[part]])[1]=="ms_barchart" && grepl("barchart_", names(rp[part]))) {
								n_vars <- length(unique(rp[[part]][["data"]][["var"]]))
								ppt <<- 
									officer::ph_with(x = ppt, value = rp[[part]], 
													 location = officer::ph_location_fullsize()) 
								
							}
							if(is.data.frame(rp[[part]]) && grepl("table_", names(rp[part]))) {
								table <-
									rp[[part]] %>%
									dplyr::mutate(dplyr::across(dplyr::all_of(names(.)[purrr::map_lgl(.x = ., .f = ~all(is.na(x)))]),
																~gsub("\\.", ",", round(replace(., is.na(.), str_blank), digits = round_digits))))
								ppt <<-
									officer::ph_with(x = ppt, value = table,
													 location = officer::ph_location_type(), 
													 alignment = c("l", rep("c", ncol(table)-1)))
							}
						} else if(ext=="e") {
							xls <<-
								officer::add_sheet(x = xls,  
												   label = paste0(rownumber, "_", y_var, if(!is.na(x_var)) x_var, collapse="_"))
						}
					}
				}
				rp
			}
		

		
		progress_installed <- requireNamespace("progress", quietly = TRUE)
		if(progress_installed) pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nrow(design_frame))
		if(progress_installed) pb$tick(0)

		
		out <- purrr::map(unique(design_frame[["section1"]]), .f = function(sec1) {
			if(ext=="w" && !is.na(sec1)) {
					doc <<-
						officer::body_add_par(x = doc, value = if(!is.na(sec1)) sec1 else "", style = s$heading_2) %>%
						officer::body_add_par(x = ., value = "", style = s$paragraph)
			} else if(ext=="p" & !is.na(sec1)) {
				ppt <<-
					officer::add_slide(ppt, layout = s$pptx_layout, master = s$pptx_master) %>%
					officer::ph_with(x = ., value = if(!is.na(sec1)) sec1 else "", 
									 location = officer::ph_location_type(type = "title"))
			}

			purrr::map(unique(design_frame[design_frame[["section1"]] == sec1, "section2"]), .f = function(sec2) {
				if(ext=="w" && !is.na(sec2)) {
					doc <<-
						officer::body_add_par(x = doc, value = if(!is.na(sec2)) sec2 else "", style = s$heading_3) %>%
						officer::body_add_par(x = ., value = "", style = s$paragraph)
				} else if(ext=="p" & !is.na(sec2)) {
					ppt <<-
						officer::add_slide(ppt, layout = s$pptx_layout, master = s$pptx_master) %>%
						officer::ph_with(x = ., value = if(!is.na(sec2)) sec2 else "", 
										 location = officer::ph_location_type(type = "title"))
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
		if(ext=="w") print(x = doc, target = filepath)
		if(ext=="p") print(x = ppt, target = filepath)
		if(ext=="x") print(x = xls, target = filepath)
		
		list(df=obj$df, var_frame=obj$var_frame, design_frame=obj$design_frame, report=out)
	}







#' Generates Template Styles Object From Excel File
#' 
#' This is an internal function to check and obtain the template_styles that
#'     is given as argument to the reporter() function.
#'
#' @param template_styles Either a data frame or a link to an Excel file that
#'     contains the columns surveyreport_style and template_style.
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang abort quo_text
#' @importFrom readxl read_excel
#' @return A list object.
#'


get_template_styles <- function(template_styles) {
	template_styles <- 
		if(is.data.frame(template_styles)) {
			template_styles 
		} else if(!is.null(template_styles) && file.exists(template_styles)) {
			readxl::read_excel(template_styles)
		}
	if(length(names(template_styles)) != 2L || !all(c("surveyreport_style", "template_style") %in% names(template_styles))) {
		rlang::abort(c("Unexpected Column Names in template_styles", 
					   i = "Argument `template_styles` must refer to a data frame with columns surveyreport_style and template_style.",
					   x = "Supplied data frame contains ", 
					   rlang::quo_text(names(template_styles))))
	}
	grab_temp_style <- function(temp_styles=template_styles, style) {
		dplyr::filter(temp_styles, .data$surveyreport_style == .env$style) %>% dplyr::pull(.data$template_style)
	}
	s <- list()
	s$heading_1 <- grab_temp_style(style="heading_1")
	s$heading_2 <- grab_temp_style(style="heading_2")
	s$heading_3 <- grab_temp_style(style="heading_3")
	s$paragraph <- grab_temp_style(style="paragraph")
	s$figure <- grab_temp_style(style="figure")
	s$figure_caption <- grab_temp_style(style="figure_caption")
	s$table_body <- grab_temp_style(style="table_body")
	s$table_caption <- grab_temp_style(style="table_caption")
	s$pptx_layout <- grab_temp_style(style="pptx_layout")
	s$pptx_master <- grab_temp_style(style="pptx_master")
	s
}