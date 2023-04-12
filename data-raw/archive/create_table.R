#' Title
#'
#' @param data 
#' @param y_var 
#' @param y_label 
#' @param y_group 
#' @param y_group_label 
#' @param y_colour_set 
#' @param drop_na_y 
#' @param x_var 
#' @param x_label 
#' @param x_group 
#' @param x_group_label 
#' @param x_colour_set 
#' @param drop_na_x 
#' @param sort_order 
#' @param prefix_number 
#' @param hide_label_if_less_than 
#' @param round_digits 
#' @param show_n 
#' @param show_p 
#' @param type 
#' @param ... 
#'
#' @return data.frame

#'
#' @examples
create_table <- 
	function(data, 
			 y_var, y_label, y_group, y_group_label, y_colour_set, drop_na_y,
			 x_var, x_label, x_group, x_group_label, x_colour_set, drop_na_x,
			 sort_order,
			 prefix_number, hide_label_if_less_than, round_digits, show_n, show_p, 
			 type=1, ...) {
		
		if(is.null(x_var)) x_var <- NA_character_
		if(!is.na(x_var)) {
			
			if(!is.character(x_var) | !length(x_var) %in% 0:1) {
				
				rlang::abort(c("Current version only accepts x_var as a single string per row.",
							   i=paste0("For the row when y_var is ", paste0(y_var, collapse=","), ","),
							   x=paste0("there is a problem with x_var: ", paste0(x_var, collapse=",")))) 
			}
			if(any(!x_var %in% names(data))) {
				rlang::abort(message = paste0("Following x_var not found in data:", x_var))
			}
		}
		# print(paste0(y_var, x_var, collapse="_"))
		new_names <- rlang::set_names(y_label, nm=unname(y_var)) %>% as.list()
		tab <-
			data %>% # Use variable labels as variable names
			dplyr::select(dplyr::all_of(c(.env$y_var, if(!is.na(.env$x_var)) .env$x_var))) %>%
			labelled::set_variable_labels(.labels = new_names) %>%
			rlang::set_names(nm = ifelse(nchar(labelled::var_label(., unlist = T))==0L | colnames(.) %in% x_var,
										 colnames(.), labelled::var_label(., unlist = T)))
		
		if(type==1L) {
			if(!is.na(x_var) && length(y_var)>1) {
				rlang::inform(c("When x_var is specified, table type1 is currently limited to a single-item chart.",
								i = paste0("Skipping for ", rlang::quo_text(y_var), " and ", x_var)))
				return(data.frame())
				
			} else {
				
				tab_long <-
					tab %>%
					{if(is.na(x_var)) tidyr::pivot_longer(., cols = dplyr::all_of(.env$y_label), names_to = "var", values_to = "val") else {
						dplyr::rename(., val=dplyr::all_of(.env$y_label), var=dplyr::all_of(.env$x_label))}} %>%
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
					{if(show_n |
						(is.na(show_n) &
						 dplyr::n_distinct(.[["n_per_var"]]) > 1L)) tidyr::unite(., col = "var", c(.data$var, .data$n_per_var), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
							dplyr::mutate(var=paste0(.data$var, ")")) else .} %>%
					{if(prefix_number) tidyr::unite(., col = "category", c(.data$val, .data$category), sep = ": ", remove = FALSE) else .} %>%
					dplyr::arrange(.data$val) %>%
					dplyr::mutate(category = factor(.data$category, levels = unique(.data$category), ordered = TRUE)) %>% # Problem here if first variable lacks a category
					dplyr::arrange(.data$var, .data$val)
				
				table <-
					tab_long %>%
					dplyr::select(-dplyr::all_of(c("val", "n_per_var_val"))) %>%
					dplyr::mutate(percent = .data$percent*100) %>%
					{if(show_n) dplyr::select(., -dplyr::all_of("n_per_var")) else dplyr::rename(., N=.data$n_per_var)} %>%
					dplyr::rename_with(.cols = dplyr::all_of("var"), .fn = ~ y_group_label) %>%
					tidyr::pivot_wider(names_from = all_of("category"), values_from = all_of("percent")) %>%
					{if(show_p) dplyr::rename_with(., .cols = -1, ~ paste0(., " (%)")) else .}
			}
			
		} else if(type==2 && !is.na(x_var)) {
			
			table <-
				tab %>%
				tidyr::pivot_longer(cols = dplyr::all_of(.env$y_label), names_to = "var", values_to = "val") %>%
				dplyr::mutate(var = labelled::to_character(.data$var),
							  category = labelled::to_character(.data$val),
							  val = as.integer(haven::as_factor(.data$val))) %>% # Problem here if first variable lacks a category
				{if(drop_na_y) dplyr::filter(., !is.na(.data$category)) else dplyr::mutate(., category = rlang::`%|%`(.data$category, "<NA>"))} %>%
				{if(drop_na_y) dplyr::filter(., !is.na(.data$val)) else .} %>%
				{if(drop_na_x & !is.na(x_var)) dplyr::filter(., !is.na(.data$var)) else dplyr::mutate(., var = rlang::`%|%`(.data$var, "<NA>"))} %>%
				dplyr::add_count(.data$var, .data[[x_label]], name = "n_per_var_var2") %>%
				dplyr::count(.data$var, .data[[x_label]], .data$n_per_var_var2, .data$val, .data$category, name = "n_per_var_val_var2") %>%
				dplyr::mutate(percent = round(.data$n_per_var_val_var2/.data$n_per_var_var2*100, round_digits)) %>%
				{if(show_n | (is.na(show_n) & dplyr::n_distinct(.[["n_per_var_var2"]]) > 1L)) {
					tidyr::unite(., col = "var", c(.data$var, .data$n_per_var), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
						dplyr::mutate(var=paste0(.data$var, ")"))} else .} %>%
				{if(prefix_number) tidyr::unite(., col = "category", c(.data$val, .data$category), sep = ": ", remove = FALSE) else .} %>%
				dplyr::arrange(.data$val) %>%
				dplyr::mutate(category = factor(.data$category, levels = unique(.data$category), ordered = T)) %>% # Problem here if first variable lacks a category
				{if(show_p) dplyr::mutate(., category = paste0(.data$category, " (%)")) else .} %>%
				{if(show_n) dplyr::rename(., N = .data$n_per_var_var2) else dplyr::select(., -dplyr::all_of("n_per_var_var2"))} %>%
				dplyr::arrange(dplyr::all_of(x_label), .data$var, .data$val) %>%
				dplyr::select(-dplyr::all_of(c("n_per_var_val_var2", "val")))  %>%
				tidyr::pivot_wider(names_from = .data$category, values_from = .data$percent) %>%
				dplyr::relocate(dplyr::all_of(c(x_label, "var"))) %>%
				dplyr::rename_with(.cols = dplyr::all_of("var"), .fn = ~ y_group_label)
		}
		
		
		
		# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
		# {if(sort=="sum_upper_categories") dplyr::group_by(., var) %>%
		# 		dplyr::mutate(sort_var = ifelse(category = )) %>%
		# 		dplyr::arrange(if(desc) dplyr::desc(sort_var) else sort_var, val) else .} %>%
		# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
		# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
		#"average", "sum_upper_categories", "alphabetical", "sum_lower_categories", "significance",
		table
	}