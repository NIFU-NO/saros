#' #' Tabulate categorical variables with proportions, v2
#'
#' @param .data data.frame with only variables you want included.
#' @param na.rm Logical, should missing be removed, or kept (default)?
#'
#' @return data.frame
#' @export
#'
#' @examples tab_cat_prop(ex_survey1[,paste0("b_", 1:3)])
tab_cat_prop2 <-
	function(.data, na.rm=FALSE) {
		out <-
			.data %>%
			names() %>%
			rlang::set_names() %>%
			purrr::map_dfr(.x = ., .id = ".variables",
						   .f = function(.var) {
						   	
						   	label <- labelled::look_for(.data,
						   								.var,
						   								details = FALSE,
						   								ignore.case = FALSE,
						   								values = FALSE,
						   								labels = FALSE)[[1,3]]
						   	.data %>%
						   		dplyr::count(.variable=.data[[.var]]) %>%
						   		{if(na.rm) filter(., !is.na(.data$.variable)) else .} %>%
						   		dplyr::mutate(p=.data$n/sum(.data$n),
						   					  n=NULL,
						   					  .variable_label = .data$label,
						   					  .variable = labelled::to_character(.data$.variable))
						   	
						   }) %>%
			tidyr::separate(col = .data$.variables,
							into = c("var_group", "var_part"), sep = "_") %>%
			dplyr::group_by(.data$var_group) %>%
			dplyr::group_map(.keep = TRUE, .f = function(.x, ...) {
				tidyr::pivot_wider(data = .x,
								   names_from = .data$.variable,
								   values_from = .data$p) %>%
					tidyr::unite(col = ".variable",
								 c(.data$var_group, .data$var_part), sep = "_")
			})
		
		extract_names <- function(i) {
			out[[i]] %>%
				colnames(.) %>%
				matrix(data = ., nrow = 1, byrow = TRUE) %>%
				tibble::as_tibble() %>%
				dplyr::mutate(V1 = .data$i)
		}
		retrieve_dfs <- function(.data, ...) {
			df_ids <-
				dplyr::pull(.data, .data$V1) %>%
				as.integer()
			out[df_ids] %>%
				dplyr::bind_rows()
		}
		
		
		seq_along(out) %>%
			rlang::set_names() %>%
			purrr::map_dfr(.x = ., .f = ~extract_names(i=.x)) %>%
			tidyr::unite(col = "V2", -c(.data$V1:.data$V2)) %>%
			dplyr::group_by(.data$V2) %>%
			dplyr::group_map(.data = ., .f = ~retrieve_dfs(.data = .x))
	}