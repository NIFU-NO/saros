#' Add Table
#'
#' Can add one of two tables:
#'
#' @param obj A list containing df, var_frame and design_frame, as created by
#'   create_infoframe().
#' @param prefix_number Whether to add the underlying number index for the
#'   categories as a prefix in the printed chart and table labels. Useful for
#'   troubleshooting.
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
#' @param remove_empty_y_var Logical, defaults to FALSE. As `reporter()` cannot
#'   handle rows in design_frame with missing y_var, will drop these rows if
#'   FALSE (NOT YET IMPLEMENTED). Otherwise aborts.
#' @param type Integer, either 1 or 2. 1 corresponds to a regular percent table
#'   whereas 2 corresponds to average per x_var category.
#'
#' @return obj, with the barcharts attached in design_frame under column
#'   "barchart1".
#' @importFrom dplyr select all_of case_when filter rename across n group_by
#'   ungroup summarize mutate if_else count add_count n_distinct arrange
#'   rename_with %>%
#' @importFrom labelled set_variable_labels var_label to_character val_label
#' @importFrom rlang abort warn .data .env  arg_match set_names %|% quo_text
#' @importFrom haven as_factor
#' @importFrom tidyr pivot_longer unite
#' @importFrom purrr map2 pmap map_lgl

#'
#' @examples
#' ex_survey1_inf_new <- add_table(ex_survey1_inf, type=1)
#' ex_survey1_inf_new <- add_table(ex_survey1_inf_new, type=2)

add_table <- 
	function(obj, 
			 # Model specific arguments
			 prefix_number=FALSE, 
			 round_digits=1L, 
			 show_n=FALSE, 
			 show_p=TRUE, 
			 sorting = NULL, drop_na_y=TRUE, drop_na_x=TRUE,
			 # Global arguments
			 type=1,
			 remove_empty_y_var=FALSE) {
		
		show_n_options <- c(T,F,NA)
		sorting_options <- c("y_pos", "x_pos", 
							 "top1", "average", "sum_high_cat", "sum_low_cat", "estimate", "p.value")
		
		design_frame <- obj$design_frame
		
		obj <- assert_valid_infoframe(obj=obj)
		design_frame <- check_options(df = design_frame, df_var = "drop_na_y", global_default = drop_na_y, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "drop_na_x", global_default = drop_na_x, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "prefix_number", global_default = prefix_number, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "round_digits", global_default = round_digits, options = -3:3)
		design_frame <- check_options(df = design_frame, df_var = "show_n", global_default = show_n, options = show_n_options)
		design_frame <- check_options(df = design_frame, df_var = "show_p", global_default = show_n, options = show_n_options)
		
		added_parts <- c()
		
		
		if(is.null(design_frame[["sort_order"]])) {
			if(!is.null(sorting) && (length(sorting)!=1L || !sorting %in% sorting_options)) {
				rlang::abort(c(i="Global argument `sorting` must be a string length 1, and one of ", 
							   rlang::quo_text(sorting_options), 
							   i=" or NULL when column 'sort_order' in design_frame is not provided."))
			}
			design_frame[["sort_order"]] <- if(!is.null(sorting)) sorting else "y_var"
			added_parts <- c(added_parts, "sort_order")
		}
		
		
		if(length(added_parts)>0L) rlang::inform(c("Added parts that were missing in design_frame: ", 
												 rlang::quo_text(added_parts)))
		

		
		data <- .remove_special_chars_in_labels(obj$df)
		
		design_frame <-
			design_frame %>%
			dplyr::arrange(.data[["sort_order"]])
		col_name <- paste0("table_", type)
		design_frame <-
			design_frame %>%
			dplyr::mutate({{col_name}} := 
				   	purrr::pmap(.l = design_frame, .f = create_table, data))

		# print(nrow(design_frame))
		# print(length(out))
		
		list(df=obj$df, var_frame=obj$var_frame, design_frame=design_frame)
				
	}