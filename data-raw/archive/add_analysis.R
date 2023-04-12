
#' Add Analyses To A Design Frame
#'
#' Given the specifications of design_frame in obj, run regression analyses.
#'
#' @param obj A list as returned from create_infoframe()
#' @param engine either "mplus", "lavaan" or "lm". Currently only supports
#'   mplus.
#' @param estimator "Either "wlsmv", "mlr" or NA.
#' @param ignore_x_type Logical, default is FALSE. Whether to ignore the type of
#'   the independent variable (such as ordinal, nominal) and treat them as they
#'   are stored (e.g. model as continuous/binary).
#' @param ignore_dummy01 Logical, default is FALSE. If FALSE, will automatically
#'   adjust binary variables such that the lowest category is zero, and the
#'   highest is 1.
#' @param warn_low_n Logical, default is FALSE. If TRUE ????
#' @param remove_text_var Logical, default is TRUE. If TRUE, will automatically
#'   drop all variables with type=="text", except for cluster-variable.
#' @param remove_empty_y_var Logical, default is TRUE. If TRUE, ???
#' @param drop_duplicates Logical, default is TRUE. If TRUE, will automatically
#'   remove duplicate rows in design frame.
#' @param path Path to saving Mplus files. Default is a temporary folder.
#' @param cluster Character string identifying the variable used as
#'   cluster-identifier.
#' @param adjust_dummy01 Whether to fix dummies with lowest category above 0.
#' @importFrom purrr map map_chr pmap_dfr map_lgl pwalk
#' @importFrom tidyr pivot_wider
#' @importFrom labelled remove_labels
#' @importFrom dplyr filter n_distinct count distinct %>%
#' @importFrom rlang abort warn .data
#'
#' @return A data frame with model specifications in simplified form (y_var=Y,
#'   independent=>X), estimate of the regression coefficient if any, and model
#'   fit indices if any.
#' @export
#' @examples
#' #ex_survey1_inf_new <- add_analysis(ex_survey1_inf)
add_analysis <-
	function(obj,
			 cluster = NA,
			 engine=c("mplus", "lavaan", "lm"), estimator=c("wlsmv", "mlr", NA),
			 ignore_x_type=FALSE, ignore_dummy01=FALSE, warn_low_n=FALSE,
			 adjust_dummy01=TRUE,
			 remove_text_var = TRUE, remove_empty_y_var=TRUE,
			 drop_duplicates=TRUE, path=tempdir()) {



	# Early fail.
	obj <- assert_valid_infoframe(obj)
	engine <- rlang::arg_match(engine)
	estimator <- rlang::arg_match(estimator)

	engine_options <- c("mplus", "lavaan", "lm") # Needed?
	estimator_options <- c("wlsmv", "mlr")
	type_options <- c("nominal", "ordinal", "interval")
	cluster_options <- c(colnames(obj$df), NA_character_)
	added_parts <- c()

	design_frame <- obj$design_frame
	df <- labelled::remove_labels(x = obj$df, user_na_to_na = TRUE, keep_var_label = FALSE)
	if(is.null(design_frame[["y_var"]])) rlang::abort("design_frame must have at least a 'y_var' variable")

	if(remove_text_var) {
		design_frame <-
			design_frame %>%
			dplyr::filter(.data$y_type != "text", .data$x_type != "text")
	} else if(design_frame %>%
			  dplyr::filter(.data$y_type == "text", .data$x_type == "text") %>%
			  nrow() > 0L) {
		rlang::abort("Found character-variables. Please drop these or set remove_text_var=TRUE.")
	}

	empty_y_var <-
		design_frame[["y_var"]] %>%
		purrr::map_lgl(.f = ~length(.x)==0)
	if(!remove_empty_y_var && any(empty_y_var)) {
		rlang::abort("Seems there are empty 'y_var' entries. Please remove these first using:
					 `my_design_frame %>% filter(pull(y_var) %>% lapply(length) %>% unlist()>0)`")
	}
	if(drop_duplicates) {
		design_frame <- unique(design_frame)
	}

	if(is.null(design_frame[["y_group"]])) {
		design_frame[["y_group"]] <- design_frame[["y_var"]] %>%
			purrr::map(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "y_group")
	}
	if(is.null(design_frame[["x_var"]])) {
		design_frame[["x_var"]] <- NA_character_
		added_parts <- c(added_parts, "x_var")
	}
	if(is.null(design_frame[["x_group"]])) {
		design_frame[["x_group"]] <- design_frame[["x_var"]] %>%
			purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "x_group")
	}
	if(is.null(design_frame[["x_ref_cat"]])) {
		design_frame[["x_ref_cat"]] <- NA_character_
		added_parts <- c(added_parts, "x_ref_cat")
	}

	design_frame %>%
		dplyr::distinct(.data$x_var, .data$x_type) %>%
		dplyr::filter(!is.na(.data$x_var)) %>%
		purrr::pwalk(function(x_var, x_type) {
			if(!x_var %in% colnames(df)) {
				rlang::abort(message = paste0("Following x_var not found in df:", rlang::expr_text(x_var)))
			}
			if(x_type %in% c("ordinal", "nominal")) {
				if(!ignore_x_type &&
				   dplyr::n_distinct(df[[x_var]], na.rm = T)>2L) {
					rlang::abort(c(paste0("For x_var=='", x_var, "', x_type is '", x_type, "' and there are over 2 categories."),
									i="Consider `prep_data()` to automatically generate dummy variables.",
									i="Set `ignore_x_type=TRUE` to omit this check."))
				} else if(!ignore_dummy01 && !adjust_dummy01 &&
						  dplyr::n_distinct(df[[x_var]], na.rm = T)==2L &&
						  min(df[[x_var]], na.rm = T) > 0L) {
					rlang::abort(c(paste0("For x_var=='", x_var, "', x_type is '", x_type, "' and there are 2 categories where lowest value is above 0."),
									i="Use function prep_data to automatically adjust dummy variables.",
									i="Set `ignore_dummy01 = TRUE` to omit this check."))
				} else if(warn_low_n &&
						  dplyr::count(df, .data[[x_var]]) %>%
						  dplyr::filter(!is.na(.data[[x_var]]), n<10) %>%
						  nrow() > 0L) {
					rlang::warn(c(paste0("For x_var=='", x_var, "', x_type is '", x_type, "' and at least one category has fewer than 10 observations."),
								i="Use function prep_data to automatically combine categories with small n before creating dummy variables.",
								i="Set `warn_low_n = FALSE` to omit this check."))
				}
			}
		})


	design_frame <- check_options(df = design_frame, df_var = "y_type", global_default = "interval", options = type_options)
	if(!ignore_x_type) design_frame <- check_options(df = design_frame, df_var = "x_type", global_default = "interval", options = type_options)
	design_frame <- check_options(df = design_frame, df_var = "cluster", global_default = cluster, options = cluster_options)
	design_frame <- check_options(df = design_frame, df_var = "engine", global_default = engine, options = engine_options)
	design_frame <- check_options(df = design_frame, df_var = "estimator", global_default = estimator, options = estimator_options)
	# Return warnings
	if(length(added_parts)>0L) rlang::warn(paste0("Added parts that were missing in design_frame: ", paste0(added_parts, collapse=",")))

	progress_installed <- requireNamespace("progress", quietly = TRUE)
	if(progress_installed) pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nrow(design_frame))
	if(progress_installed) pb$tick(0)

	design_frame <-
		cbind(design_frame,
			  purrr::pmap_dfr(.l = design_frame, .f = function(...) {
			  	if(progress_installed) pb$tick()
			  	run_single_analysis(df=df, adjust_dummy01=adjust_dummy01, ...)
			  }))

	list(df=obj$df, var_frame=obj$var_frame, design_frame=design_frame)
}

#' Run Single Analyses in Design Frame
#'
#' Given a row in a design frame, runs it.
#'
#' @param y_var Character vector, must be provided and refer to columns in df.
#' @param y_group String, optional.
#' @param y_type Character vector, optional. Must be a single value or the same
#'   length as y_var.
#' @param x_var String, optional.
#' @param x_group String, optional.
#' @param x_type Character vector, optional. Must be a single value or the same
#'   length as x_var.
#' @param x_ref_cat Optional string indicating x_var category used as reference,
#'   if x_var type is nominal or ordinal.
#' @param cluster Optional global default.
#' @param engine String, one of "wlsmv", "mlr" or NA.
#' @param ... Other variables accidentally sent to function.
#' @param df Data frame where y_var, x_var, and optionally cluster can be found.
#' @param estimator One of "wlsmv", "mlr", or NA. WLSMV is Weighted Least
#'     Squares with Means and Variance Adjustment, suitable for non-normally
#'     distributed dependent variables. Robust Maximum Likelihood (mlr) is
#'     appropriate if the missing data is missing at random, e.g. can be
#'     explained by the available variables in the model.
#' @param adjust_dummy01 Whether to fix dummies with lowest category above 0.
#' @importFrom purrr map_lgl
#' @importFrom tidyr pivot_wider
#' @import dplyr
#' @importFrom rlang abort warn
#' @importFrom utils View
#' @importFrom stats lm as.formula
#'
#' @return Single row data frame.
#'
#' @examples
run_single_analysis <- function(df, adjust_dummy01,
	y_var, y_group, y_type=c("interval", "nominal", "ordinal"),
	x_var, x_group, x_type=c("interval", "nominal", "ordinal"), x_ref_cat,
	cluster, engine=c("mplus", "lavaan", "lm"),
	estimator = c("wlsmv", "mlr", NA), ...) {

	y_type <- rlang::arg_match(y_type)
	x_type <- rlang::arg_match(x_type)
	engine <- rlang::arg_match(engine)
	estimator <- rlang::arg_match(estimator)




	# Move all these checks to global scope
	if(length(cluster)>1L) {
		rlang::abort(message = c("Max 1 `cluster` is allowed. Problem with ", rlang::expr_text(cluster)))
	}
	if(!is.na(cluster) && !cluster %in% colnames(df)) {
		rlang::abort(message = paste0("Following `cluster` not found in df: ", cluster))
	}

	# Slim down df temporarily
	needed_vars <- c(y_var, if(all(!is.null(x_var) && !is.na(x_var))) x_var, if(!is.na(cluster)) cluster)
	print(df)
	df <- df[, needed_vars]

	### Generate dummy variables
	if(x_type %in% c("ordinal", "nominal") &
	   dplyr::n_distinct(df[[x_var]], na.rm = TRUE) > 2L) {
		if(is.null(x_ref_cat) || is.na(x_ref_cat)) x_ref_cat <-
				dplyr::count(df, .data[[x_var]]) %>%
				dplyr::arrange(dplyr::desc(n)) %>%
				dplyr::slice(1) %>%
				dplyr::pull(.data[[x_var]])
		df <-
			df %>%
			dplyr::mutate(dummy_variable=1L, ROW_IDENTIFIER=seq_len(nrow(.)),
						  "{x_var}" := iconv(x = .data[[x_var]], from = "latin1", to = "ASCII", sub="")) %>%
			tidyr::pivot_wider(names_from = dplyr::all_of(x_var), values_from = all_of("dummy_variable"),
							   values_fill = 0L, names_glue = paste0( "{.name}__", x_var)) %>%
			dplyr::select(-dplyr::all_of(c(paste0(x_ref_cat, "__", x_var), "ROW_IDENTIFIER")))

		x_var <- grep(pattern = paste0("__", x_var), x = names(df), value = T)

	}
	if(adjust_dummy01 &
	   x_type %in% c("ordinal", "nominal") &
	   dplyr::n_distinct(df[[x_var]], na.rm=TRUE) == 2L &
	   min(df[[x_var]], na.rm = TRUE)>0L) {
		min_x_val <- min(df[[x_var]], na.rm = TRUE)
		df[[x_var]] <- df[[x_var]] - min_x_val
	}

	if(engine=="mplus") {
		mod <- prepare_mplus_model(df=df, y_var=y_var, y_group=y_group, x_var=x_var, x_group=x_group, y_type=y_type, x_type=x_type, cluster=cluster, estimator=estimator)
		extract_mplus_model(mod, y_var=y_var, x_var=x_var, y_group=y_group, x_group=x_group)
	} else if(engine=="lavaan") {
		# run_lavaan(df=df, y_var = y_var, y_group = y_group,y_type=y_type, x_var=x_var, x_group=x_group, cluster=cluster, estimator=estimator)
	} else if(engine=="lm") {
		if(!is.na(cluster)) rlang::warn("Cluster ignored for lm()")
		if(!length(y_var)>1L) rlang::abort("lm() requires a single y_var string.")
		mod <- stats::lm(formula = as.formula(paste0(y_var, "~", paste0(x_var, collapse=" + "))), data = df)

	}
}

# prep_data <- function(df, var_frame, design_frame, drop_unlisted=FALSE, drop_duplicates=TRUE) {
	# if(!is.data.frame(df)) rlang::abort("df is not a data.frame")
	# if(!is.data.frame(var_frame)) rlang::abort("var_frame is not a data.frame")
	# if(nrow(var_frame)==0L) rlang::abort("Why do you give me an empty var_frame?")
	# if(is.null(var_frame[["var"]]) | is.null(var_frame[["role"]]) | is.null(var_frame[["type"]])) {
	# 	rlang::abort("var_frame must have at least the variables: var, role, type")
	# }
	#
	# empty_var <- var_frame[["var"]] %>% purrr::map_int(.f = ~length)
	# if(any(!empty_var)) {
	# 	rlang::abort("Seems there are empty 'var' entries. Please remove these first using:
	# 				 `my_design_frame %>% filter(pull(var) %>% lapply(length) %>% unlist()>0)`")
	# }
	# if(drop_duplicates) {
	# 	var_frame <- unique(var_frame)
	# }
	# dummy_code <- function(x) {
	# 	sapply(levels(x), function(y) as.integer(x == y))
	# }
	###
	# for(i in 1:nrow(var_frame)) {
	# 	if(var_frame[i, "role"] == "x" &&
	# 	   var_frame[i, "type"] %in% c("ordinal", "nominal") &&
	# 	   n_distinct(df[[var_frame[i, "var"]]], na.rm = T) > 2L) {
	# 		var_frame[["modelled_by"]] <-
	# 		df[[var_frame[i, "var"]]]

		# }
	# }

	# if(drop_unlisted) {

	# }


	# Check upfront if there is insufficient variance
	# for(i in 1:length(x_var)) {
	# 	x_n_unique <- length(unique(df[[x_var[i]]])[!is.na(unique(df[[x_var[i]]]))])
	# 	if(x_n_unique<2) {
	# 		return(data.frame(error="n_distinct(x_var)<2"))
	# 	} else if(x_n_unique >= 2L && x_n_unique < ordinal_if_fewer_than) {
	# 		df[[x_var[i]]] <- as.factor(df[[x_var[i]]])
	# 		df2 <- model.matrix.default(data = df, as.formula("~", x_var[i]))
	# 		df <- cbind(df, df2)
	# 		x_var <- c(x_var, colnames(df2))
	# 	} else if(x_n_unique == 2L) {
	# 		df[[x_var[i]]] <- as.integer(as.factor(df[[x_var[i]]])) - 1L
	# 	}
	# }
	# list(df=df, var_frame=var_frame)
# }

#' Prepare Mplus Model Syntax and Run Model
#'
#' Internal function.
#'
#' @param df Data frame.
#' @param y_var String.
#' @param x_var String.
#' @param y_group String.
#' @param x_group String.
#' @param y_type String.
#' @param x_type String.
#' @param cluster String. Optional.
#' @param estimator String. One of "wlsmv", "mlr", or NA
#' @param path Path to Mplus files. Must be provided.
#'
#' @return Data frame row.
#' @importFrom MplusAutomation mplusObject mplusModeler
#' @importFrom rlang arg_match
#'
#' @examples
prepare_mplus_model <-
	function(df, y_var, x_var, y_group, x_group, y_type, x_type,
			 cluster, estimator=c("wlsmv", "mlr", NA), path) {
		estimator <- rlang::arg_match(estimator)

	x_syntax <- paste0(" ON ", paste0(x_var, collapse="\n"), ";")

	y_group_short <- gsub("^([[:alnum:]_]{,6}).*", "\\1", y_group)
	y_syntax <- paste0(paste0(y_var, collapse="\n"))
	if(length(y_var)>1L) {
		model_syntax <- paste0(paste0(y_group_short, " BY ", y_syntax, ";\n"),
							   if(all(!is.null(x_var) && !is.na(x_var))) paste0(y_group_short, " ", x_syntax))
	} else {
		model_syntax <- if(all(!is.null(x_var) && !is.na(x_var))) paste0(y_syntax,  x_syntax)
	}

	y_variable_syntax <- paste0(if(y_type == "ordinal") "CATEGORICAL = " else "NOMINAL = ",
								paste0(y_var, collapse="\n"), ";\n")
	cluster_syntax <- if(!is.na(cluster)) paste0("CLUSTER = ", cluster,";\n")
	title <- paste0(paste0(y_var, collapse=", "), "\n",
					if(all(!is.null(x_var) && !is.na(x_var))) " ON ",
					if(all(!is.null(x_var) && !is.na(x_var))) paste(x_var, collapse=" "), ";")
	variable <- paste0(y_variable_syntax, cluster_syntax)
	analysis <- paste0(if(estimator == "wlsmv") "ESTIMATOR=WLSMV;\n" else "ESTIMATOR=MLR;\n",
					   if(!is.na(cluster)) "TYPE = COMPLEX;")
	file_path <- file.path(path, paste0(y_group_short, if(all(!is.null(x_var) && !is.na(x_var))) x_group, ".inp"))

	MplusAutomation::mplusObject(TITLE = title, VARIABLE = variable, ANALYSIS = analysis, MODEL = model_syntax,
									 OUTPUT = "STD;", usevariables = colnames(.), rdata = df) %>%
		MplusAutomation::mplusModeler(object = ., modelout = file_path, run = 1L, hashfilename = FALSE, varwarnings = TRUE, writeData = "always")
}

#' Extract Errors From MplusAutomation Model Object
#'
#' First version of this is very crude, simply extracting every error and
#'     returning a string.
#'
#' @param mod MplusAutomation model object.
#'
#' @return String
#' @export
#' @examples
extract_mplus_error <- function(mod) {
	paste0(unlist(mod[["results"]][["errors"]]), collapse=". ")
}

#' Extract MplusAutomation Results Into A Single Row
#'
#' Takes a MplusAutomation object and names of variables to extract. If model
#'     contains error, will attempt to extract these.
#'
#' @param mod mplusAutomation.model object.
#' @param y_var,x_var Character vector with dependent/independent variables to
#'   extract.
#' @param y_group,x_group String with name of factor.
#' @importFrom dplyr filter select rename mutate summarize matches %>%
#' @importFrom rlang .data .env
#' @importFrom tidyr chop
#' @return A single data frame row with most important information.
#' @export
#' @examples
extract_mplus_model <- function(mod, y_var, y_group, x_var, x_group) {
	mod_out <- data.frame(error=extract_mplus_error(mod))
	mod_available <- !is.null(mod[["results"]][["parameters"]][["unstandardized"]][["pval"]])
	if(mod_available) {

		if(all(!is.null(x_var) && !is.na(x_var))) {

			header_on <- if(length(y_var)>1L) paste0(toupper(y_group), "\\.ON") else paste0(toupper(y_var), "\\.ON")
			mod_par <-
				mod %>%
				.[["results"]] %>%
				.[["parameters"]] %>%
				.[["unstandardized"]] %>%
				dplyr::filter(grepl(header_on, .data$paramHeader)) %>%
				dplyr::select(-all_of(c("paramHeader", "se", "est_se"))) %>%
				dplyr::rename(term = all_of("param"), estimate = all_of("est"), p.value = all_of("pval")) %>%
				tidyr::chop(cols = c(.data$term, .data$estimate, .data$p.value))
			if(nrow(mod_par)==1L) mod_out <- cbind(mod_out, mod_par)
		}

		if(length(y_var)>1L) {
			mod_load <-
				mod %>%
				.[["results"]] %>%
				.[["parameters"]] %>%
				.[["unstandardized"]] %>%
				dplyr::filter(grepl(paste0(toupper(.env$y_group), "\\.BY"), .data$paramHeader),
							  !(.data$est==1.000 & .data$se==999.000)) %>%
				dplyr::select(-all_of(c("paramHeader", "se", "est_se"))) %>%
				dplyr::mutate(n_load_good = .data$est >= .env$loading_cutoff &
							  	.data$pval < .05,
							  n_load_bad = .data$est < .env$loading_cutoff | .data$pval > .05) %>%
				dplyr::summarize(n_load_good=sum(.data$n_load_good), n_load_bad=sum(.data$n_load_bad))
			if(nrow(mod_load)==1L) mod_out <- cbind(mod_out, mod_load)

		}

		if(!is.null(mod[["results"]][["summaries"]])) {
			mod_sum <-
				mod %>%
				.[["results"]] %>%
				.[["summaries"]] %>%
				dplyr::select(-all_of(c("Mplus.version", "Title", "AnalysisType", "DataType",
							  "Parameters", "Estimator", "NGroups",
							  "NContinuousLatentVars")), -dplyr::matches("Baseline"))
			if(nrow(mod_sum)==1L) mod_out <- cbind(mod_out, mod_sum)
			if(all(!is.null(x_var) && !is.na(x_var))) {
				mod_r2 <-
					mod %>%
					.[["results"]] %>%
					.[["parameters"]] %>%
					.[["r2"]] %>%
					dplyr::filter(.data$param== if(length(.env$y_var)>1L) toupper(.env$y_group) else toupper(.env$y_var)) %>%
					dplyr::mutate(r2=as.character(.data$est), r2_p=as.character(.data$pval)) %>%
					dplyr::select(all_of(c("r2", "r2_p")))
				if(nrow(mod_r2)==1L) mod_out <- cbind(mod_out, mod_r2)
			}
		}
	}
	mod_out
}

#' Add Variable with Fit Interpretation
#'
#' Given the design_frame information on fit (run add_analysis first) for
#'     constructs, add a superficial interpretation of fit given cutoffs. Note
#'     that this is generally bad scientific practice, but useful for reports
#'     and quick analyses. Also note that fit can be improved by dropping poor
#'     fitting variables, which is not covered by this function.
#'
#' @param obj An object as returned from create_infoframe.
#' @param cutoff_chisq Cutoff value for the p-value of the chisquare test of
#'     absolute fit of model to the data,
#' @param cutoff_CFI Cutoff value for the Comparative Fit Index of relative fit
#'     of the model to the null model.
#' @param cutoff_TLI Cutoff value for the Tucker-Lewis Fit Index of relative fit
#'     of the model to the null model.
#' @param cutoff_RMSEA Cutoff value for the Root Mean Square Residual of
#'     Approximation of relative fit, when penalizing model complexity.
#'     of the model to the null model.
#' @param cutoff_RMSEA_p05 Cutoff value for the p-value for the RMSEA.
#' @param cutoff_SRMR Cutoff-value for the Standardized Root Mean Square
#'     Residual for the
#' @param drop_raw_fit Remove original fit variables and keep interpretation?
#'
#' @return design_frame with fit interpretation added as a column.
#' @export
#' @examples

#' #obj <- add_analyses(obj)
#' #obj <- add_fit(obj)

add_fit <- function(obj, cutoff_chisq=.05,
					cutoff_CFI=0.95, cutoff_TLI=0.95, cutoff_RMSEA=0.05, cutoff_RMSEA_p05=.05,
					cutoff_SRMR=.05, drop_raw_fit=TRUE) {

	assert_valid_infoframe(obj)
	if(!is.null(obj[["design_frame"]][["CFI"]])) {
	obj[["design_frame"]][["fit"]] <-
		ifelse((obj[["design_frame"]][["NDependentVars"]] >= 3L &
					obj[["design_frame"]][["NIndependentVars"]] > 0L) |
						obj[["design_frame"]][["NDependentVars"]] >= 4L,
					 ifelse(obj[["design_frame"]][["ChiSqM_PValue"]] >= cutoff_chisq, "Good",
					 	   ifelse(obj[["design_frame"]][["CFI"]] >= cutoff_CFI &
					 	   	   	obj[["design_frame"]][["TLI"]] >= cutoff_TLI &
					 	   	   	obj[["design_frame"]][["RMSEA_Estimate"]] <= cutoff_RMSEA &
					 	   	   	obj[["design_frame"]][["RMSEA_pLT05"]] >= cutoff_RMSEA_p05 &
					 	   	   	obj[["design_frame"]][["SRMR"]] <= cutoff_SRMR,
					 	   	   "Acceptable", "Low")), "NA")
	if(drop_raw_fit) {
		obj[["design_frame"]][["ChiSqM_Value"]] <-
			obj[["design_frame"]][["ChiSqM_DF"]] <-
			obj[["design_frame"]][["ChiSqM_PValue"]] <-
			obj[["design_frame"]][["CFI"]] <-
			obj[["design_frame"]][["TLI"]] <-
			obj[["design_frame"]][["RMSEA_Estimate"]] <-
			obj[["design_frame"]][["RMSEA_90CI_LB"]] <-
			obj[["design_frame"]][["RMSEA_90CI_UB"]] <-
			obj[["design_frame"]][["RMSEA_pLT05"]] <-
			obj[["design_frame"]][["SRMR"]] <- NULL
	}
	}
	obj
}

# add_effectsize <- function(obj, cutoff_sig=0.05, cutoff_r2_p = .05, cutoff_r2=.01,
# 						  effect_size_lower_cutoff = c("Low" = 0.1, "Medium" = .3, "High" = .5),
# 								  drop_raw_est=TRUE) {
# 	if(!is.null(obj[["design_frame"]][["r2"]])) {
# 		obj[["design_frame"]][["sig"]] <- ifelse()
#
# 	}
# 	obj
# }

# add_mi_diff <- function(model, group_name="FOCUS",
# 								 cutoff_chisq = .05,
# 								 cutoff_cfi = .95,
# 								 cutoff_tli = .95,
# 								 cutoff_rmsea = .08,
# 								 cutoff_srmr = .05) {
# 	if(!is.null(model$results$invariance_testing$models)) {
# 		a <- model$results$summaries %>%
# 			dplyr::select(Model, ChiSqM_PValue, CFI, TLI, RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, SRMR)
# 		b <- data.frame(Model="SCALAR MODEL", ChiSq_ScalarEqConfigural = model$results$invariance_testing$compared[,"Pvalue"])
#
# 		a <- dplyr::left_join(a,b, by="Model")
#
# 		if(!is.null(model$results$parameters$unstandardized$SCALAR.MODEL)) {
# 			c <- model$results$parameters$unstandardized$SCALAR.MODEL %>%
# 				dplyr::filter(grepl("Means", paramHeader), Group==group_name) %>%
# 				dplyr::select(est, pval) %>%
# 				dplyr::mutate(Model="SCALAR MODEL", est =as.numeric(est), pval=as.numeric(pval))
# 			a <- dplyr::left_join(a, c, by = "Model") %>%
# 				dplyr::mutate(Model = gsub(" MODEL", "", Model),
# 					   Fit = dplyr::if_else(ChiSqM_PValue >= cutoff_chisq & CFI >= cutoff_cfi & TLI >= cutoff_tli & RMSEA_Estimate <= cutoff_rmsea & SRMR <= cutoff_srmr, "perfect",
# 					   					 dplyr::if_else(ChiSqM_PValue < cutoff_chisq & CFI > cutoff_cfi & TLI > cutoff_tli & RMSEA_Estimate < cutoff_rmsea & SRMR < cutoff_srmr, "acceptable",
# 					   					 			   dplyr::if_else(ChiSqM_PValue > cutoff_chisq & (CFI > cutoff_cfi | TLI > cutoff_tli | RMSEA_Estimate < cutoff_rmsea | SRMR < cutoff_srmr), "low", "poor"))))
# 		}
# 		a
# 	}
# }

