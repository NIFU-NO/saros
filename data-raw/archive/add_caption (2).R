#' Add Caption to Figures or Tables
#' 
#' Given an infoframe and a string template, create captions.
#'
#' @param obj Infoframe
#' @param str_template glue-template string that specifies caption.
#' @param str_pos,str_neg,str_not Add comparison based on the 
#'           estimate- and p.value-variables.
#' @param show_n Not yet implemented.
#'
#' @return Infoframe
#' @importFrom vctrs vec_assert
#' @importFrom glue glue
#' @importFrom purrr pmap

#'
#' @examples
#' 
#' ex_survey1_inf_new <- add_analysis(ex_survey1_inf)
#' ex_survey1_inf_new <- add_caption(ex_survey1_inf_new)
add_caption <- 
	function(obj, 
str_template="Fig. {rownumber}. Chart of {y_group_label}{comp}{x_group_label}.", 
						str_pos=" <+> ", str_neg=" <-> ", str_not=" <0> ",
						show_n=TRUE) {
	
	obj <- assert_valid_infoframe(obj=obj)
	vctrs::vec_assert(x = str_template, ptype = character(), size = 1L)
	vctrs::vec_assert(x = str_pos, ptype = character(), size = 1L)
	vctrs::vec_assert(x = str_neg, ptype = character(), size = 1L)
	vctrs::vec_assert(x = str_not, ptype = character(), size = 1L)
	vctrs::vec_assert(x = show_n, ptype = logical(), size = 1L)

	
	if(grepl("\\{comp\\}", str_template) & (is.null(obj$design_frame[["estimate"]]) | 
											is.null(obj$design_frame[["p.value"]]))) {
		rlang::abort(c(x="When {comp} is part of str_template, columns `estimate` and `p.value` must exist in obj$design_frame.",
					 i="Please run add_analyses(my_obj) first, or remove {comp} from template."))
	}
		
	create_caption <- function(rownumber, y_var, y_label, y_group_label, x_var, x_label, estimate, p.value, ...) {
		
		y <- if(length(y_var) == 1L & length(y_label) == 1L) paste0(y_group_label, " - ", y_label) else y_group_label
		x <- if(!is.null(x_var) && !is.na(x_var)) paste0(x_label, collapse="_")
		
		# if(grepl("{comp}", str_template, fixed = TRUE) && 
		#    !is.null(estimate) && !is.null(p.value)) {
		# 	comp <- if(is.na(x_var) || is.na(estimate) || is.na(p.value)) ""
		# 	comp <- if(estimate > 0 & p.value<.05) str_pos
		# 	comp <- if(estimate < 0 & p.value<.05) str_neg
		# 	comp <- if(p.value >= .05 | estimate == 0) str_not
		# } else comp <- ""
		
		# if(grepl("{N}", str_template, fixed = TRUE)) {
		# }

		# n_size <- 
		# 	if(show_n | (is.na(show_n) & dplyr::n_distinct(range(tab_long$n_per_var))==1L)) {
		# 		paste0(" N=[", paste0(unique(range(tab_long$n_per_var)), collapse="-"), "]")
		# 	}
		# 
		# glue::glue(str_template)
		
	}
	
	out <-	
		obj$design_frame %>%
		mutate(rownumber = seq_len(n())) %>%
		purrr::pmap_chr(.l=., ~create_caption(...))
	
	list(df=obj$df, var_frame=obj$var_frame, design_frame=cbind(obj$design_frame, caption=out))
	
}

