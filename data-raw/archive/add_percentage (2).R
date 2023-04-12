#' Add percentages to design frame, with various options, for later processing.
#'
#' @param obj The infoframe object.
#' @param design_frame  Design frame as returned from create_infoframe().
#' @param percent If TRUE (default), return proportion*100, otherwise proportion.
#' @param round_digits Number of decimals.
#' @param set One of 
#' \itemize{
#' \item{"top"}{The percentage for the highest category available in the variable.}
#' \item{"upper"}{The sum of the percentages for the categories above the middle category.}
#' \item{"mid_upper"}{The sum of the percentages for the categories including and above the middle category.}
#' \item{"mid_lower"}{The sum of the percentages for the categories including and below the middle category.}
#' \item{"lower"}{The sum of the percentages for the categories below the middle category.}
#' \item{"bottom"}{The percentage for the lowest category available in the variable.}
#' }
#' @importFrom purrr pmap_dfr 
#' @importFrom dplyr select filter all_of mutate count group_by ungroup rename_with full_join across matches %>%
#' @importFrom labelled to_character 
#' @importFrom haven as_factor 
#' @importFrom rlang abort .data
#' @importFrom tidyr pivot_wider 
 
#'
#' @examples
#' ex_survey1_inf_new <- add_percentage(ex_survey1_inf)

add_percentage <- function(obj, design_frame, set="upper", percent=TRUE, round_digits=0) {
	
	set_options <- c("top", "upper", "mid_upper", "lower", "mid_lower", "bottom")
	if(!(!is.null(set) && length(set)==1L && set %in% set_options)) rlang::abort(c("set must be one of"), rlang::expr_text(set_options))
	design_frame <-
		purrr::pmap_dfr(.l = obj$design_frame, .f=function(y_var, x_var, ...) {
		if(length(y_var)==1L && length(x_var) == 1L && 
		   !is.null(y_var) && !is.null(x_var) #&& 
		   #!is.na(y_var) && !is.na(x_var)
		   ) {

				obj$df %>%
				dplyr::filter(!is.na(.data[[y_var]]), !is.na(.data[[x_var]])) %>%
				dplyr::mutate(y_val = as.integer(haven::as_factor(.data[[y_var]])),
							  include = .data$y_val %in% subset_vector(sort(unique(.data$y_val)), set=set),
							  y_val = NULL,
							  x_val = as.integer(haven::as_factor(.data[[x_var]])),
							  x_val2 = .data$x_val,
							  x_cat = labelled::to_character(.data[[x_var]])) %>%
				dplyr::count(.data$x_cat, .data$x_val, .data$x_val2, .data$include) %>%
				dplyr::group_by(.data$x_cat, .data$x_val) %>%
				dplyr::mutate(percent = round(.data$n/sum(.data$n)*(if(.env$percent) 100 else 1), digits=.env$round_digits)) %>%
				dplyr::ungroup() %>%
				dplyr::filter(.data$include) %>%
				dplyr::select(-dplyr::all_of(c("include", "n"))) %>%
				tidyr::pivot_wider(names_from = c(.data$x_val, .data$x_val2), values_from = c(.data$percent, .data$x_cat), values_fill = list(0, NA)) %>%
				dplyr::rename_with(.cols = dplyr::matches("_[0-9]*$"), .fn = ~gsub("_[0-9]*$", "", .)) %>%
				dplyr::mutate(y_var=vctrs::list_of(.env$y_var), x_var=vctrs::list_of(.env$x_var))
		} else if(!is.null(y_var)) {
		data.frame(y_var=vctrs::list_of(y_var))
		} else if(!is.null(x_var)) {
			data.frame(x_var = vctrs::list_of(x_var))
		}
	}) %>%
		dplyr::left_join(x = obj$design_frame, y=., by=c("y_var", "x_var"))
	
	list(df=obj$df, var_frame=obj$var_frame, design_frame=design_frame)
}
