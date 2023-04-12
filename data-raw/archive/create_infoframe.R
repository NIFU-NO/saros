
#' Create An Infoframe
#' 
#' Will create a list with the original data, a data column frame (codebook), 
#'     and a design frame for y-x analyses, tables, figures and reports. 
#'     Defaults in the design frame can be modified later. This function should
#'     be run if the original data has been changed.
#'     
#' @param df A labelled data frame containing only the data columns one wishes 
#'     to create item groups for.
#' @param var_group_item_sep Separator between item group identifier prefix 
#'     and item suffix. Default="_" is currently the only that works well for 
#'     separating. Set "$" if each item is to be considered a group.
#' @param varlabel_group_item_sep Same as for `var_group_item_sep`, but for 
#'     separating labels into group and item specific.
#' @param y_var character vector with dependent data columns.
#' @param x_var character vector with independent data columns.
#' @param med_var character vector with mediator data columns.
#' @param ordinal_var character vector with data columns having ordinal meaning.
#' @param nominal_var character vector with data columns having nominal meaning.
#' @param interval_var character vector with data columns having interval meaning.
#' @param text_var character vector with data columns having text meaning 
#'     (will be excluded from figures, tables, analyses, but will be reported 
#'     in wordclouds and as-is.
#' @param add_constructs Constructs consist of multiple data columns measuring 
#'     essentially the same. Add these as new construct based on group-variable?
#' @param add_y_univariates,add_x_univariates,add_m_univariates Whether or not 
#'     to report univariates for dependent/independent/mediator data columns.
#' @param ordinal_if_fewer_than ???
#' @param vars_as_labels Logical, TRUE (default) or FALSE). If TRUE, will use 
#'     data column names as labels if labels are missing. 
#' @param colour_set_ordinal A character vector with hex-colours used for 
#'     ordinal data columns in charts/plots.
#' @param colour_set_nominal A character vector with hex-colours used for 
#'     nominal data columns in charts/plots.
#'
#' @return A list consisting of the original data, a var_frame (codebook for 
#'     each data column), and a design_frame.
#' \itemize{
#' \item{var_frame}{The var_frame (codebook).}
#' \itemize{
#' \item{group}{Data column group.}
#' \item{item}{Original data column.}
#' \item{group_label}{Group variable label.}
#' \item{item_label}{Original data column label.}
#' \item{type}{Data column type: ordinal, nominal, text, interval, 
#'     singular (only one value)}
#' }
#' \item{design_frame}
#' \itemize{
#' \item{y_var}
#' \item{y_label}
#' \item{y_group}
#' \item{y_group_label}
#' \item{x_var}
#' \item{x_label}
#' \item{x_group}
#' \item{x_group_label}
#' \item{y_type}
#' \item{x_type}
#' }
#' }
#' @importFrom purrr map_chr map map_lgl
#' @import dplyr 
#' @importFrom haven is.labelled
#' @importFrom labelled to_labelled look_for
#' @importFrom rlang warn abort := set_names .data .env
#' @importFrom tidyr separate chop pack unpack expand_grid
#' @importFrom vctrs as_list_of

#'
#' @examples
#' data("ex_survey1")
# ex_survey1_inf <-
#' create_infoframe(df=ex_survey1,
#' 				 y_var=grep("^[abcdefgp]_", names(ex_survey1), value=TRUE),
#' 				 x_var=grep("^x[12]_", names(ex_survey1), value=TRUE),
#' 				 ordinal_var=grep("^[abdegp]_", names(ex_survey1), value=TRUE),
#' 				 nominal_var=c("x1_sex", "x2_human", "f_uni"),
#' 				 add_constructs = FALSE,
#' 				 add_x_univariates=FALSE,
#' 				 add_y_univariates=FALSE)

create_infoframe <-
	function(df,
			 var_group_item_sep="_",
			 varlabel_group_item_sep=" - ",
			 y_var = grep("^x", colnames(df), value = TRUE, invert = TRUE),
			 x_var = NULL,
			 med_var = NULL,
			 ordinal_var=NULL,
			 nominal_var=NULL,
			 interval_var=NULL,
			 text_var=NULL,
			 add_constructs=TRUE,
			 add_x_univariates=FALSE, add_y_univariates=FALSE, add_m_univariates=FALSE,
			 ordinal_if_fewer_than = 5,
			 vars_as_labels = FALSE,
			 colour_set_ordinal = c("#E8B0B7", "#DE919A", "#D5727D", "#C84957", "#BC3848", "#9D2F3C", "#7E2630"), # Red
			 colour_set_nominal = c(red="#C82D49",
			 					   black = "#363636",
			 					   beige="#EDE2D2",
			 					   blue="#2D8E9F",
			 					   purple="#DBD2E0")) {

		
		# Last helpful checks
		if(is.null(y_var)) rlang::warn("No y_var provided. This makes design_frame useless.")
		if(is.null(y_var)) rlang::inform("No x_var provided. Is this as intended?")
		

		type_checker <- function(var, data=df) {
			purrr::map_chr(var, function(v) {
				dplyr::case_when(dplyr::n_distinct(data[[v]], na.rm = T) == 1L ~ "singular",
								 v %in% text_var ~ "text",
								 v %in% nominal_var ~ "nominal",
								 v %in% ordinal_var |
								 	(dplyr::n_distinct(data[[v]], na.rm = T) >= 2L &
								 	 	dplyr::n_distinct(data[[v]], na.rm = T) < ordinal_if_fewer_than) ~ "ordinal",
								 v %in% interval_var | (dplyr::n_distinct(data[[v]], na.rm = T) >= ordinal_if_fewer_than) ~ "interval")
			})
		}
		find_uniques <- function(var, data=df) {
			purrr::map(var, function(v) {
			data[,v] %>%
				dplyr::mutate(dplyr::across(everything(), .fns = ~as.character(.x))) %>%
				unlist() %>%
				unique() %>% 
				.[!is.na(.)] %>%
					sort()
			})
		}
		find_group_uniques <- function(var_list, data=df) {
				data[,var_list] %>%
				dplyr::mutate(dplyr::across(everything(), .fns = ~as.character(.x))) %>%
				unlist() %>%
				unique() %>%
				.[!is.na(.)] %>% 
				sort() %>%
				list()
		}

		add_univariates <- function(data, e="y") {
			dplyr::bind_rows(data,
							 dplyr::filter(var_frame, .data$role==e) %>%
							 	dplyr::select(-all_of("role")) %>%
							 	dplyr::rename_with(.cols = dplyr::everything(), .fn = ~paste0("y_", .)))
		}
		if(vars_as_labels) {
			lapply(c(y_var, x_var), function(var) {
				if(!haven::is.labelled(df[[var]])) {
					rlang::warn(paste0(var, " is not labelled. Using data column as label."))
					df[[var]] <- labelled::to_labelled(df[[var]])
				}
			})
		}
		if(!all(y_var %in% colnames(df))) rlang::abort(c(x="Following y_var not found in df:", 
														 rlang::expr_text((y_var[!y_var %in% colnames(df)]))))
		if(!all(x_var %in% colnames(df))) rlang::abort(c(x="Following x_var not found in df:", 
														 rlang::expr_text((x_var[!x_var %in% colnames(df)]))))

		if(is.character(var_group_item_sep)) {
			y_lacks_sep <- grep(var_group_item_sep, y_var, value = TRUE, invert = TRUE)
			if(length(y_lacks_sep)>0L) rlang::warn(c(x="The following y_var could not be split:", 
													 rlang::expr_text((y_lacks_sep))))
		}

		x_lacks_sep <- grep(var_group_item_sep, x_var, value = TRUE, invert = TRUE)
		if(length(x_lacks_sep)>0L) rlang::warn(c(x="The following x_var could not be split:", 
												 rlang::expr_text((x_lacks_sep))))
		

		suppressWarnings(expr = {
		var_frame <-
			labelled::look_for(df, details=FALSE) %>%
			tidyr::separate(col=.data$variable, into=c("group", NA), sep=var_group_item_sep, remove = FALSE, convert = FALSE, extra = "merge", fill="right") %>%
			tidyr::separate(col=.data$label, into=c("group_label", "label"), sep=varlabel_group_item_sep, convert = FALSE, extra = "merge", fill="right") %>%
			dplyr::rename(var = all_of("variable")) %>%
			dplyr::mutate(label = dplyr::if_else(is.na(.data$label) | nchar(.data$label)==0L, .data$group_label, .data$label),
						  label = dplyr::if_else(is.na(.data$label), "Missing", .data$label),
						  role = dplyr::if_else(.data$var %in% .env$y_var, "y", 
						  					  dplyr::if_else(.data$var %in% .env$x_var, "x", 
						  					  			   dplyr::if_else(.data$var %in% .env$med_var, "m", NA_character_))),
						  type = type_checker(var=.data$var),
						  uniques = find_uniques(var=.data$var),
						  n_uniques = lengths(.data$uniques)) %>%
			dplyr::group_by(.data$group) %>%
			dplyr::mutate(uniques_per_group = find_group_uniques(var_list=.data$var),
						  n_uniques_per_group = lengths(.data$uniques_per_group)) %>%
			dplyr::ungroup() %>%
			dplyr::mutate(colour_set = colour_picker(type = .data$type, 
													 unique_set_group = .data$uniques_per_group, 
													 unique_set = .data$uniques,
						  						     colour_set_ordinal=.env$colour_set_ordinal, 
						  						     colour_set_nominal=.env$colour_set_nominal))  %>%
			dplyr::arrange(.data$group, .data$group_label, .data$var, .data$label)
		})
		print(var_frame)
		## Error checking
		n_group_label <- 
			var_frame %>% 
			dplyr::distinct(.data$group, .data$group_label) %>% 
			nrow()
		n_group <- 
			var_frame %>% 
			dplyr::distinct(.data$group) %>% 
			nrow()
		n_group_aux <- 
			var_frame %>% 
			dplyr::distinct(.data$group, .data$role, .data$type, .keep_all = T) %>% 
			nrow()


		if(n_group_label != n_group) {
			err_groups <-
				var_frame %>% 
				dplyr::distinct(.data$group, .data$group_label, .keep_all = T) %>% 
				dplyr::add_count(.data$group) %>% 
				dplyr::filter(.data$n > 1) %>%
				dplyr::select(dplyr::all_of(c("var", "group", "group_label", "label")))
			error_msg <- "Check that the battery-item pattern is valid, and that names and labels match accordingly.
			There is a mismatch between "
			rlang::abort(c("Variable group and group label mismatch:",
						   i="Each variable group must contain only one unique group label.",
						   x="Problem with variable group(s):",
						   rlang::quo_text(err_groups)))
		}
		if(n_group_aux != n_group) {
			err_obj <-
				var_frame %>% 
				dplyr::distinct(.data$group, .data$role, .data$type,  .keep_all = T) %>% 
				dplyr::add_count(.data$group) %>% 
				dplyr::filter(.data$n>1)
			# View(err_obj)
			error_msg <- "Please ensure all data columns in a group have the same role and type. Mismatch between "
			rlang::abort(paste0(error_msg, "groups (", n_group, ") and role-type (", n_group_aux, "). "),
						 ... = err_obj)
		}

		if(add_constructs) {
			var_frame <-
				dplyr::bind_rows(
					var_frame %>% # For the regular variables, make them list-columns.
						dplyr::mutate(var = vctrs::as_list_of(as.list(.data$var)),
									  label = vctrs::as_list_of(as.list(.data$label))),
					var_frame %>% # For the constructs, chop up related variables.
						dplyr::select(-dplyr::all_of("pos")) %>%
						dplyr::filter(.data$group != .data$var) %>%
						tidyr::chop(cols = c(.data$var, .data$label)))
		}
		tmp <-
			c("y", "m", "x") %>%
			rlang::set_names() %>%
			purrr::map(., function(e) {
				var_frame %>%
					dplyr::filter(.data$role == .env$e) %>%
					dplyr::select(-dplyr::all_of("role")) %>%
					dplyr::rename_with(.cols = dplyr::everything(), .fn = ~paste0(e, "_", .)) %>%
					tidyr::pack(!!e := dplyr::everything())
			}) %>%
			.[purrr::map_lgl(., .f = ~nrow(.)>0L)]


		design_frame <-
			tidyr::expand_grid(tmp[["y"]], tmp[["m"]], tmp[["x"]]) %>%
			tidyr::unpack(cols = names(.env$tmp)) %>%
			{if(add_y_univariates) add_univariates(data=., e="y") else .} %>%
			{if(add_m_univariates) add_univariates(data=., e="m") else .} %>%
			{if(add_x_univariates) add_univariates(data=., e="x") else .} %>%
			unique()


		list(df=df, var_frame=var_frame, design_frame=design_frame) 
	}
