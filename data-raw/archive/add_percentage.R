#' Add percentages to design frame, with various options, for later processing.
#'
#' @param df The data. Extract with myObj[["df"]]
#' @param design_frame  Design frame as returned from create_infoframe().
#' @param set One of 
#' \itemize{
#' \item{"top"}{The percentage for the highest category available in the variable.}
#' \item{"upper"}{The sum of the percentages for the categories above the middle category.}
#' \item{"mid_upper"}{The sum of the percentages for the categories including and above the middle category.}
#' \item{"mid_lower"}{The sum of the percentages for the categories including and below the middle category.}
#' \item{"lower"}{The sum of the percentages for the categories below the middle category.}
#' \item{"bottom"}{The percentage for the lowest category available in the variable.}
#' }
#' @param percent If TRUE (default), return proportion*100, otherwise proportion.
#' @param round_digits Number of decimals.
#' @importFrom purrr pmap_dfr 
#' @importFrom dplyr select filter all_of mutate count group_by ungroup rename_with full_join across matches %>%
#' @importFrom labelled to_character 
#' @importFrom haven as_factor 
#' @importFrom rlang abort .data
#' @importFrom tidyr pivot_wider 
#' @export 
#'
#' @examples
#' ex_survey1_inf_new <- add_percentage(ex_survey1_inf)

add_percentage <- function(df, design_frame, set="upper", percent=TRUE, round_digits=0) {
	
	set_options <- c("top", "upper", "mid_upper", "lower", "mid_lower", "bottom")
	if(!(!is.null(set) && length(set)==1L && set %in% set_options)) rlang::abort(c("set must be one of"), rlang::expr_text(set_options))
	purrr::pmap_dfr(.l = design_frame, .f=function(Y, X, ...) {
		df %>%
			dplyr::select(y=dplyr::all_of(Y), x=dplyr::all_of(X)) %>%
			dplyr::filter(!is.na(.data$x), !is.na(.data$y)) %>%
			dplyr::mutate(x_cat = labelled::to_character(.data$x),
						  x_val = as.integer(haven::as_factor(.data$x)),
						  x_val2 = .data$x_val,
						  y_val = as.integer(haven::as_factor(.data$y)),
						  include = .data$y_val %in% subset_vector(sort(unique(.data$y_val)), set=set),
						  y_val=NULL) %>%
			dplyr::count(.data$x_cat, .data$x_val, .data$x_val2, .data$include) %>%
			dplyr::group_by(.data$x_cat, .data$x_val) %>%
			dplyr::mutate(p = round(n/sum(n)*(if(percent) 100 else 1), digits=round_digits)) %>%
			dplyr::ungroup() %>%
			dplyr::filter(.data$include) %>%
			dplyr::select(-all_of(c("include", "n"))) %>%
			tidyr::pivot_wider(names_from = c(.data$x_val, .data$x_val2), values_from = c(.data$p, .data$x_cat)) %>%
			dplyr::mutate(Y=.data$Y, X=.data$X) %>%
			dplyr::rename_with(.cols = dplyr::matches("_[0-9]*$"), .fn = ~gsub("_[0-9]*$", "", .))
	}) %>%
		dplyr::full_join(x = design_frame, y=., by=c("X", "Y")) %>%
		dplyr::mutate(dplyr::across(dplyr::matches("^p_[0-9]*$"), ~rlang::`%|%`(., 0L)))
}
