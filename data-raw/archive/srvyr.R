#' I could not find a function that checks whether x is a valid survey object or not, neither in survey nor srvyr. Seems rather useful to have to avoid errors?


#' Checks if x is valid survey or tbl_srv object. Warns if not and informs what x is. 
#'
#' @param x Object.
#'
#' @return Test result (logical)
#' @export
#'
#' @examples check_valid_srv(srvyr::as_survey(mtcars))
#' check_valid_srv(mtcars)
#' check_valid_srv(pi)
check_valid_srv <- function(x, strict=TRUE) {
	# Should probably make this name vector always updated by retrieving it from a survey-creator function?
	srv_elements <- c("cluster","strata", "has.strata", "prob", 
					  "allprob", "call", "variables", "fpc", "pps")
	test <- 
		any(class(x) %in% c("tbl_svy", "survey.design", 
							"survey.design2", "twophase2", "svyrep.design"))
	if(strict) test <- test &&
		all(names(x) %in% srv_elements) &&
		is.data.frame(x$variables)
	if(!test) rlang::warn(c(x="x is not a valid survey object.",
							i=paste0("x is a ", paste0(class(x), collapse=", "))))
	test
}

survey_prop_multi <- function(x, ...) {


	test <- check_valid_srv(x)
	pos_select <- tidyselect::eval_select(expr(c(...)), 
										  data = if(test) x$variables else x)

	if(test) {
		out <-
			purrr::map_dfr(.x = pos_select, .id = "var", .f = ~{
				x0 <- srvyr::select(x, category=all_of(.x))
				x0_varlabel <- unname(labelled::var_label(x0$variables, unlist=TRUE))
				x <- srvyr::group_by(x0, category)
				x <- srvyr::summarize(x, pct = srvyr::survey_prop(vartype = "ci"))
				x <- srvyr::mutate(x, variable_label = if(nchar(x0_varlabel)>0) x0_varlabel else NULL)
				srvyr::select(x, any_of("variable_label"), category, pct)
			})
	} else if(is.data.frame(x)) {
	out <-
		purrr::map_dfr(.x = pos_select, .id = "var", .f = ~{
			x0 <- dplyr::select(x, category=all_of(.x))
			x0_varlabel <- unname(labelled::var_label(x0, unlist=TRUE))
			x <- dplyr::count(x0, category)
			x <- dplyr::mutate(x, pct = n/sum(n, na.rm=TRUE))
			x <- dplyr::mutate(x, variable_label = if(nchar(x0_varlabel)>0) x0_varlabel else NULL)
			dplyr::select(x, any_of("variable_label"), category, pct)
		})
	}
	
	out <- dplyr::mutate(out, category = labelled::to_factor(category))
	tidyr::pivot_wider(out, id_cols = c(var, any_of("variable_label")), names_from = category, values_from = pct)
}
survey_prop_multi(srvyr::as_survey(ex_survey1), b_2:b_3)
survey_prop_multi(srvyr::as_survey(mtcars), cyl)
survey_prop_multi(tibble(mtcars), cyl)
survey_prop_multi(ex_survey1, b_2:b_3)
