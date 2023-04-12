
## Computes Response Rates for A Given Response Variable, by Groups
#'
#' @param .data Data set
#' @param group_vars One or more variables (unquoted) to group on.
#' @param .response_var Name of response variable (unquoted) containing 
#' response codes.
#' @param .valid_response_values Integers of .response_var that 
#' indicate valid respondents.
#' @param group_summary_if_ngroup_above If more groups than this (e.g. schools),
#' provide means, min, max, etc.
#'
#' @return list of data frames

#'
#' @examples compute_response_rates(ex_survey1, 
#' group_vars=x1_sex, 
#' .response_var=resp_status)

compute_response_rates <- # tidyverse version
	function(data, 
			 group_vars=NULL,
			 response_var=NULL,
			 valid_response_values=3:4,
			 group_summary_if_ngroup_above=2) {
# 		
# 		quo_group_vars <- rlang::enquo(group_vars)
# 		quo_response_var <- rlang::enquo(response_var)
# 
# 		out <- list(total = dplyr::summarize(data, n=n()))
# 		if(!rlang::quo_is_null(quo_response_var)) {
# 			if(length(tidyselect::eval_select(quo_response_var, data))>1) {
# 				rlang::abort("Only a single response_var is allowed.")
# 			}
# 			
# 			response_var_name <- rlang::as_name(quo_response_var)
# 			out$total[[response_var_name]] <- "TOTAL"
# 			
# 			
# 			out$total <-
# 				data %>%
# 				dplyr::group_by({{response_var}}) %>%
# 				dplyr::summarize(n=n()) %>%
# 				dplyr::mutate(p=n/sum(.data$n, na.rm=TRUE)) %>%
# 				dplyr::ungroup() %>%
# 				dplyr::bind_rows(., out$total)
# 			
# 			if(!rlang::quo_is_null(quo_group_vars)) {
# 				
# 
# 				group_vars_pos <- tidyselect::eval_select(quo_group_vars, data)
# 
# 				out2 <-
# 					purrr::map2(.x = group_vars_pos, 
# 								.y = names(group_vars_pos), 
# 								.f = function(.x, .y) {
# 
# 					  		out2 <- dplyr::group_by(data, 
# 					  								{{.y}} := {{.x}}, 
# 					  								{{response_var}} := {{response_var}} %in% 
# 					  									.env$valid_response_values)
# 					  		
# 					  		out2 <- dplyr::summarize(out2, n=n())
# 					  		out2 <- dplyr::mutate(out2, p = n/sum(.data$n, na.rm=TRUE))
# 					  		out2 <- dplyr::filter(out2, {{response_var}})
# 					  		# out2 <- dplyr::select(out2, -{{response_var}})
# 					  		out2 <- dplyr::ungroup(out2)
# 					  		
# 					  		if(nrow(out2) >= group_summary_if_ngroup_above) {
# 					  			summary_out <-
# 					  				dplyr::summarize(out2, 
# 					  								 dplyr::across(c(.data$p, .data$n), 
# 					  								 			  list(Mean=.data$mean, 
# 					  											   	 SD=.data$sd, 
# 					  											   	 Min=.data$min, 
# 					  											   	 Max=.data$max), 
# 					  											   na.rm=TRUE, 
# 					  								 			  .names = "{.col}_{.fn}"))
# 					  			list(out2, summary_out) %>% 
# 					  				rlang::set_names(c("groups", "summary_across_groups"))
# 					  		} else list(groups=out)
# 					  	})
#  				out <- c(out, out2)
# 			}
# 			
# 		}
# 		
# 		out
	}
