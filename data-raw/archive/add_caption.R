#' Add Caption to Figures or Tables
#' 
#' Given an infoframe and a string template, create captions.
#'
#' @param obj Infoframe
#' @param str_template glue-template string that specifies caption.
#' @param str_pos,str_neg,str_not Add comparison based on the est- and pval-variables.
#' @param show_n NOt yet implemented.
#'
#' @return Infoframe
#' @importFrom vctrs vec_assert
#' @importFrom glue glue
#' @importFrom purrr pmap
#' @export
#'
#' @examples
#' ex_survey1_inf_new <- add_caption(ex_survey1_inf)
add_caption <- function(obj, str_template = "Figure {rownumber}. Barchart of {y}{comp}{x}. {N}.", 
						str_pos=" <+> ", str_neg=" <-> ", str_not=" <0> ",
						show_n=TRUE) {
	
	obj <- assert_valid_infoframe(obj=obj)
	vctrs::vec_assert(x = str_template, ptype = character(), size = 1L)
	vctrs::vec_assert(x = str_pos, ptype = character(), size = 1L)
	vctrs::vec_assert(x = str_neg, ptype = character(), size = 1L)
	vctrs::vec_assert(x = str_not, ptype = character(), size = 1L)
	show_n_options <- c(TRUE,FALSE,NA)
	
	create_caption <- function(rownumber, y_var, y_label, y_group_label, x_var, x_label, est, pval, ...) {
		
		y <- if(length(y_var) == 1L & length(y_label) == 1L) paste0(y_group_label, " - ", y_label) else y_group_label
		x <- if(!is.null(x_var) && !is.na(x_var)) paste0(x_label, collapse="_")
		
		comp <- if(is.na(x_var) || is.na(est) || is.na(pval)) ""
		comp <- if(est > 0 & pval<.05) str_pos
		comp <- if(est < 0 & pval<.05) str_neg
		comp <- if(pval >= .05 | est == 0) str_not
		

		# n_size <- 
		# 	if(show_n | (is.na(show_n) & dplyr::n_distinct(range(tab_long$n_per_var))==1L)) {
		# 		paste0(" N=[", paste0(unique(range(tab_long$n_per_var)), collapse="-"), "]")
		# 	}
		# 
		glue::glue(str_template)
	}
	
	out <-	
		obj$design_frame %>%
		mutate(rownumber = seq_len(n())) %>%
		purrr::pmap(.l=., ~create_caption(...))
	
	list(df=obj$df, var_frame=obj$var_frame, design_frame=cbind(obj$design_frame, out))
	
}

