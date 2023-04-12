#' Add Barchart
#'
#' Can add one of two barcharts:
#'
#' @param obj A list containing df, var_frame and design_frame, as created by
#'   create_infoframe().
#' @param prefix_number Whether to add the underlying number index for the
#'   categories as a prefix in the printed chart and table labels. Useful for
#'   troubleshooting.
#' @param hide_label_if_less_than Not yet working.
#' @param font_size Font size. Defaults to 8.
#' @param round_digits Integer, defaults to 1. Number of decimals in charts and
#'   tables. Currently not working for charts.
#' @param show_n Logical, defaults to FALSE. Should number of cases in a
#'   category/group be added to axis labels? NA means TRUE where there is
#'   variation across groups/categories, FALSE otherwise.
#' @param show_p Logical, defaults to TRUE. Should percentage symbol (%) be
#'   added to data labels?
#' @param sorting Which sorting variable to use? NULL equals data as it is.
#' @param drop_na_y,drop_na_x Logical, defaults to TRUE. Whether to drop missing
#'   as a category.
#' @param drop_duplicates Logical, defaults to FALSE. If there are duplicate
#'   rows in design_frame, drop duplicates silently?
#' @param remove_empty_y_var Logical, defaults to FALSE. As function cannot
#'   handle rows in design_frame with missing y_var, will drop these rows if
#'   FALSE (NOT YET IMPLEMENTED). Otherwise aborts.
#' @param type 1 for horizontal percent-bar for each category. 2 for
#'   average and 3 for median, primarily for interval/continuous/binary
#'   variables.
#'
#' @return obj, with the barcharts attached in design_frame under column
#'   "barchart1".
#' @importFrom dplyr select all_of case_when filter rename across n group_by
#'   ungroup summarize mutate if_else count add_count n_distinct arrange
#'   rename_with %>%
#' @importFrom mschart body_add_chart chart_labels_text
#' @importFrom labelled set_variable_labels var_label to_character val_label
#' @importFrom rlang abort inform .data .env  arg_match set_names %|% quo_text
#' @importFrom haven as_factor
#' @importFrom tidyr pivot_longer unite
#' @importFrom purrr map2 pmap map_lgl

#'
#' @examples
#' #ex_survey1_obj_new <- suppressMessages(add_barchart_mschart(ex_survey1_inf))
add_barchart_mschart <- 
	function(obj, 
			 # Model specific arguments
			 prefix_number=FALSE, hide_label_if_less_than=0, 
			 font_size=8, round_digits=1, 
			 show_n=FALSE, 
			 show_p=TRUE, 
			 sorting = NULL, drop_na_y=TRUE, drop_na_x=TRUE,
			 # Global arguments
			 type=1L,
			 drop_duplicates=TRUE, remove_empty_y_var=FALSE) {
		
		
		
		
		show_n_options <- c(T,F,NA)
		sorting_options <- c("y_pos", "x_pos", 
							 "top1", "average", "sum_high_cat", "sum_low_cat", "estimate", "p.value")
		if(!(vctrs::vec_is(type, ptype = integer(), size = 1L) & type %in% 1:2)) {
			rlang::abort("Argument `type` must be a single integer, either 1 or 2.")
		}
		
		obj <- assert_valid_infoframe(obj=obj)
		design_frame <- obj$design_frame
		design_frame <- check_options(df = design_frame, df_var = "type", global_default = type, options = 1:2)
		design_frame <- check_options(df = design_frame, df_var = "drop_na_y", global_default = drop_na_y, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "drop_na_x", global_default = drop_na_x, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "prefix_number", global_default = prefix_number, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "hide_label_if_less_than", global_default = hide_label_if_less_than, options = numeric())
		design_frame <- check_options(df = design_frame, df_var = "font_size", global_default = font_size, options = 0:72)
		design_frame <- check_options(df = design_frame, df_var = "round_digits", global_default = round_digits, options = -3:3)
		design_frame <- check_options(df = design_frame, df_var = "show_n", global_default = show_n, options = show_n_options)
		
		added_parts <- c()
		
		
		if(is.null(design_frame[["sort_order"]])) {
			if(!is.null(sorting) && (length(sorting)!=1L || !sorting %in% c(sorting_options, names(design_frame)))) {
				rlang::abort(c(i="Global argument `sorting` must be a string length 1, and one of ", 
							   rlang::quo_text(sorting_options), 
							   i=" or NULL when column 'sort_order' in design_frame is not provided."))
			}
			design_frame[["sort_order"]] <- if(!is.null(sorting)) sorting else "y_var"
			added_parts <- c(added_parts, "sort_order")
		}
		
		
		if(length(added_parts)>0L) {
			rlang::inform(c("Added parts that were missing in design_frame: ", 
							rlang::quo_text(added_parts)))
		}

		
		create_barchart_mschart <- 
			function(y_var, y_label, y_group, y_group_label, y_colour_set, y_type, drop_na_y,
					 x_var, x_label, x_group, x_group_label, x_colour_set, x_type, drop_na_x,
					 sort_order, type,
					 prefix_number, hide_label_if_less_than, font_size, round_digits, show_n, ...) {
				
				# if(is.na(y_label)) y_label <- "Missing"

				if(is.null(x_var)) x_var <- NA_character_

				if(!is.na(x_var)) {
					
					if(!is.character(x_var) | !length(x_var) %in% 0:1) {
						
						rlang::abort(c("Current version only accepts x_var as a single string per row.",
									   i=paste0("For the row when y_var is ", paste0(y_var, collapse=","), ","),
									   x=paste0("there is a problem with x_var: ", paste0(x_var, collapse=",")))) 
					}
					if(any(!x_var %in% names(df))) {
						rlang::abort(message = paste0("Following x_var not found in df:", x_var))
					}
				}
				new_names <- rlang::set_names(x = y_label, nm = y_var) %>%	as.list()
				tab <- 
					df %>%
					dplyr::select(dplyr::all_of(c(.env$y_var, if(!is.na(.env$x_var)) .env$x_var))) %>% 
					labelled::set_variable_labels(.labels = new_names) %>%
					stats::setNames(object = ., nm = ifelse(nchar(labelled::var_label(., unlist = T))==0L | colnames(.) %in% .env$x_var,
												 colnames(.), labelled::var_label(., unlist = T)))
				if(type==1L & y_type != "interval") {
					if(!is.na(x_var) && length(y_var)>1) {
						rlang::inform(c("When x_var is specified, barchart type=1 is currently limited to a single-item chart.",
									  i = paste0("Skipping for ", rlang::quo_text(y_var), " and ", x_var)))
						return()
						
					} else {

						tab_long <-
							tab %>%
							{if(is.na(x_var)) tidyr::pivot_longer(., cols = dplyr::all_of(if(any(!is.na(.env$y_label))) .env$y_label else .env$y_var), names_to = "var", values_to = "val") else {
								dplyr::rename(., val={y_label}, var={x_label})}} %>%
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
							dplyr::mutate(percent = round(.data$n_per_var_val/.data$n_per_var, 3),
										  label = dplyr::if_else(.data$percent < .env$hide_label_if_less_than, 
										  					   NA_real_, .data$percent)) %>%
							{if(show_n |
								(is.na(show_n) &
								 dplyr::n_distinct(.[["n_per_var"]]) > 1L)) tidyr::unite(., col = "var", c(.data$var, .data$n_per_var), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
									dplyr::mutate(var=paste0(.data$var, ")")) else .} %>%
							{if(prefix_number) tidyr::unite(., col = "category", c(.data$val, .data$category), sep = ": ", remove = FALSE) else .} %>%
							dplyr::arrange(.data$val) %>%
							dplyr::mutate(category = factor(.data$category, levels = unique(.data$category), ordered = TRUE)) %>% # Problem here if first variable lacks a category
							dplyr::arrange(.data$var, .data$val)

						ms_percentbar(df = tab_long, y_var = "percent", x_var="var", data_label = "label",
									  means = FALSE, f_size = font_size, user_colours=y_colour_set)
					}
					
				} else if(type==2 && !is.na(x_var)) {
					
					tab_long <-
						tab %>%
						dplyr::mutate(dplyr::across(.cols = -dplyr::all_of(.env$x_var), .fns = ~as.integer(haven::as_factor(.)))) %>%
						dplyr::mutate(dplyr::across(.cols = dplyr::all_of(.env$x_var), .fns = ~labelled::to_character(.))) %>%
						dplyr::group_by(category=.data[[x_var]]) %>%
						dplyr::summarize(dplyr::across(.cols = dplyr::all_of(.env$y_label), .fns = ~round(mean(., na.rm=TRUE), digits = round_digits+1)), n_per_var=dplyr::n()) %>%
						dplyr::ungroup() %>%
						tidyr::pivot_longer(cols = dplyr::all_of(.env$y_label), names_to = "var", values_to = "val")  %>%
						{if(drop_na_y) dplyr::filter(., !is.na(.data$category)) else dplyr::mutate(., category = rlang::`%|%`(.data$category, "<NA>"))} %>%
						{if(drop_na_y) dplyr::filter(., !is.na(.data$val)) else .} %>%
						{if(drop_na_x) dplyr::filter(., !is.na(.data$var)) else dplyr::mutate(., var = rlang::`%|%`(.data$var, "<NA>"))} %>%
						{if(show_n | (is.na(show_n) & dplyr::n_distinct(.[["n_per_var"]])>1L)) {
							tidyr::unite(., col = "var", all_of(c("var", "n_per_var")), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
								dplyr::mutate(var = paste0(.data$var, ")"))} else .} %>%
						dplyr::arrange(.data$var, .data$category)
					ms_percentbar(df = tab_long, y_var = "val", x_var="var", 
								  means=TRUE, f_size = font_size, user_colours=x_colour_set)
				}
				
				
				
				# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
				# {if(sort=="sum_upper_categories") dplyr::group_by(., var) %>%
				# 		dplyr::mutate(sort_var = ifelse(category = )) %>%
				# 		dplyr::arrange(if(desc) dplyr::desc(sort_var) else sort_var, val) else .} %>%
				# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
				# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
				#"average", "sum_upper_categories", "alphabetical", "sum_lower_categories", "significance",
			}
		df <- .remove_special_chars_in_labels(obj$df)
		
		design_frame <-
			design_frame %>%
			dplyr::arrange(.data[["sort_order"]])
		
		design_frame[[paste0("barchart_", type)]] <-
			purrr::pmap(.l = design_frame, .f = create_barchart_mschart)

		
		list(df=obj$df, var_frame=obj$var_frame, design_frame=design_frame)
	}


#' Create a MS Chart Horizontal Percent Barplot
#' 
#' Mostly an internal function.
#' 
#' @param df Data frame
#' @param y_var,x_var Variables for y and x axes. 
#' @param group_string Category to group on.
#' @param user_colours Character vector with hex colours.
#' @param f_size Font size, numeric value.
#' @param data_label String identifying variable with labels (e.g. value+symbol).
#' @param means Logical, default is FALSE. Whether to show means or percent.
#' @importFrom rlang abort set_names
#' @importFrom dplyr n_distinct %>%
#' @importFrom officer fp_text fp_border
#' @importFrom mschart ms_barchart chart_data_labels chart_settings chart_labels chart_labels_text chart_theme chart_ax_y chart_ax_x chart_data_stroke chart_data_fill
#' @return A mschart object for a rdocx.

#' @examples
ms_percentbar <- function(df, y_var, x_var, data_label=NULL, group_string="category", user_colours=NULL, f_size=8, means=FALSE) {
	if(!is.data.frame(df)) rlang::abort("df is not a data.frame")
	if(!(is.character(y_var) & length(y_var)==1L & !is.na(y_var) & y_var %in% colnames(df))) rlang::abort("y_var must be a single, non-NA, string and a valid column name in df.")
	if(!(is.character(x_var) & length(x_var)==1L & !is.na(x_var) & x_var %in% colnames(df))) rlang::abort("x_var must be a non-NA string and a valid column name in df.")
	if(!(is.character(group_string) & length(group_string)==1L & !is.na(group_string) & group_string %in% colnames(df))) rlang::abort("group_string must be a single non-NA string and a valid column name in df.")
	if(!means %in% c(TRUE, FALSE)) rlang::abort("means must be either TRUE or FALSE.")
	if(!is.null(user_colours) & !all(is_colour(user_colours))) rlang::abort("colours must be a character vector of valid colours in hex-format (e.g. #000000).")


	if(is.null(unlist(user_colours))) {

		colour_palette <-
			get_colour_set(n_colours_needed = dplyr::n_distinct(df[[group_string]]))
	} else {
		colour_palette <- user_colours
	}
	colour_palette <-
		rlang::set_names(colour_palette, nm=unique(df[[group_string]])) 
	
	
	fp_text_settings <-
		lapply(colour_palette, function(color) {
			officer::fp_text(font.size=f_size, color=hex_bw(color))}) %>%
		.[seq_len(dplyr::n_distinct(df[[group_string]]))]

	
	fp_b <- officer::fp_border(style = "none")
	fp_t <- officer::fp_text(font.size = f_size)
	mschart::ms_barchart(data = df, y = y_var, x=x_var, group = group_string, labels = data_label) %>%
		mschart::chart_data_labels(x=., position = "ctr", show_val = TRUE, num_fmt = if(!means) "0%" else NA) %>% # Control rounding as argument
		mschart::chart_settings(x = ., grouping = if(!means) "stacked" else "standard",
								overlap = if(!means) 100 else 0, gap_width= if(!means) 50 else 100, dir="horizontal") %>%
		mschart::chart_labels(x = ., title = NULL, xlab = NULL, ylab = NULL) %>% 
		mschart::chart_labels_text(x = ., values=fp_text_settings) %>%
		mschart::chart_theme(x = .,
							 axis_title_x = officer::fp_text(font.size = 1),
							 axis_title_y = officer::fp_text(font.size = 1),
							 main_title = officer::fp_text(font.size = 1), # Can be dropped
							 legend_position = "b",
							 axis_text_x = fp_t,
							 axis_ticks_y = fp_b,
							 axis_ticks_x = fp_b,
							 grid_major_line_x = fp_b,
							 grid_major_line_y = fp_b,
							 grid_minor_line_x = fp_b,
							 grid_minor_line_y = fp_b,
							 legend_text = fp_t) %>%
		mschart::chart_data_fill(x=., values=colour_palette) %>%
		mschart::chart_data_stroke(x=., values=colour_palette) %>%
		mschart::chart_ax_y(x = ., limit_min = 0, limit_max = if(!means) 1 else ceiling(max(df[[y_var]])), num_fmt = if(!means) "0%%" else NA)  %>%
		mschart::chart_ax_x(x = ., major_tick_mark = "none", minor_tick_mark = "none")
}
