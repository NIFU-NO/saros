
#' List All Valid Names of The Elements Argument
#'
#' @param valid_only Only return implemented elements, or all (planned) elements.
#' Note that *_docx-elements have limited storage support and
#' are hence not included in examples in this package as minor (unimportant) warnings will occur.
#'
#' @export
#' @return Character vector of valid names.
list_available_element_types <-
  function(valid_only = TRUE) {
    names(eval(formals(draft_report)$element_names)[if(valid_only) unname(eval(formals(draft_report)$element_names)) else TRUE])
  }


get_authors <- function(data, col) {
  if(!rlang::is_null(data[[col]]) &&
     !all(is.na(data[[col]]))) {

    if(is.factor(data[[col]])) {

      return(levels(data[[col]]))

    } else if(is.character(data[[col]])) {

      return(unique(data[[col]]))

    } else cli::cli_abort("{.arg {col}} must be factor or character, not {.obj_type_friendly {data[[col]]}}.")
  } else ''
}

#' Given Ordered Integer Vector, Return Requested Set.
#'
#' Useful for identifying which categories are to be collected.
#'
#' @param vec A vector of any type.
#' @param set A character string, one of c(".top", ".upper", ".mid_upper", ".lower",
#'   ".mid_lower", ".bottom")
#' @param spread_n The number of values to extract when set is "spread".
#' @param sort Whether to sort the output, defaults to FALSE.
#' @return Selected set of vector.
#' @export
#' @examples
#' subset_vector(vec=1:7, set=".mid_lower")

subset_vector <-
  function(vec,
           set=c(".top", ".upper", ".mid_upper",
                 ".lower", ".mid_lower", ".bottom", ".spread"),
           spread_n=NULL,
           sort=FALSE) {

	set <- rlang::arg_match(set)
	n <- length(vec)
	if(sort) vec <- sort(vec)
	if(n %in% 0:1) {
		vec
	} else if(set==".top") {
		vec[n]
	} else if(set==".bottom") {
		vec[1]
	} else if(n %% 2 == 0L & set!=".spread") {
		if(set %in% c(".mid_upper", ".upper")) {
			vec[(n/2+1):n]
		} else if(set %in% c(".mid_lower", ".lower")) {
			vec[1:(n/2)]
		}
	} else {
		m <- stats::median(seq_len(n))
		if(set==".upper") {
			vec[(m+1):n]
		} else if(set==".lower") {
			vec[1:(m-1)]
		} else if(set==".mid_upper") {
			vec[m:n]
		} else if(set==".mid_lower") {
			vec[1:m]
		} else if(set==".spread") {
			if(n == spread_n) {
				vec
			} else {
				max_set <- c()
				if(n %% 2 != 0L) {
					if(spread_n == 1L) {
						m
					}
					if(spread_n %% 2 != 0L) {
						max_set <- c(max_set, m)
					}
					if(spread_n > 1L) {
						max_set <- c(max_set, 1L, n)
					}
					if(spread_n > 4L) {
						max_set <- c(max_set, 2L, n-1L)
					}
					if(spread_n > 5L | spread_n == 4) {
						max_set <- c(max_set, 3L, n-2L)
					}
					if(spread_n > 6L) {
						max_set <- c(max_set, 4L, n-3L)
					}
				} else if(n %% 2L == 0L) {
					if(spread_n > 1L) {
						max_set <- c(max_set, 1L, n)
					}
					if(spread_n > 3L & n <= 6) {
						max_set <- c(max_set, 2L, n-1L)
					}
					if(spread_n > 4L | (spread_n>3L & n > 6)) {
						max_set <- c(max_set, 3L, n-2L)
					}
					if(spread_n %% 2 != 0L) {
						m <- round(stats::median(seq_len(n)))
						max_set <- c(max_set, m)
					}
				}
				unique(vec[sort(max_set)])
			}
		}
	}
}


#' Recode Missing By Type of Missingness
#'
#' Useful for item difficulty estimation according to Mislevy's recommendation.
#'     Also allowing for escaping rows with all missingess (typically
#'     not administered).
#'
#' @param df Data frame, or vector. Must be a dataframe, not a matrix, in this
#'   function. Only include item variables.
#' @param accept_vector Handles vectors if accept_vector=TRUE. Set to false to
#'   avoid accidents when using function per block and there is just one item in
#'   the block.
#' @param skipped What to replace skipped values with
#' @param not_administered What to replace not administered values with.
#' @param all_missing What to replace values in rows with all missing with.
#' @importFrom rlang set_names warn abort
#' @return A data.frame (or vector, if input is vector and accept_vector=TRUE)
#'   with recoded cells.
#' @export
#'
#' @examples
#' # Original data
#' input <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	NA,0,1,0,1, # First missing
#' 	NA,NA,1,0,1, # First two missing
#' 	1,0,NA,0,1, # One in middle missing
#' 	1,NA,NA,NA,1, # All in the middle missing
#' 	1,0,1,0,NA, # Last one missing
#' 	1,0,1,NA,NA, # Last two missing
#' 	1,0,NA,NA,NA, # Last three missing
#' 	NA,NA,NA,NA,NA # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#' # What should be the output for item estimation according to Mislevy
#' # Skipped=> 0, not_administered=>NA, all_missing=>NA
#' y_i <-  stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	0,0,1,0,1, # First missing
#' 	0,0,1,0,1, # First two missing
#' 	1,0,0,0,1, # One in middle missing
#' 	1,0,0,0,1, # All in the middle missing
#' 	1,0,1,0,0, # Last one missing
#' 	1,0,1,0,NA, # Last two missing
#' 	1,0,0,NA,NA, # Last three missing
#' 	NA,NA,NA,NA,NA # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#'
#' # What should be the output for person estimation according to Mislevy
#' # Skipped=> 0, not_administered=>NA, all_missing=>NA
#' y_p <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	0,0,1,0,1, # First missing
#' 	0,0,1,0,1, # First two missing
#' 	1,0,0,0,1, # One in middle missing
#' 	1,0,0,0,1, # All in the middle missing
#' 	1,0,1,0,0, # Last one missing
#' 	1,0,1,0,0, # Last two missing
#' 	1,0,0,0,0, # Last three missing
#' 	0,0,0,0,0 # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#' # Recoding for counting skipped, not_administered, all_missing, etc
#' # Skipped=> 99, not_administered=>999, all_missing=>9999
#' y_info <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	99,0,1,0,1, # First missing
#' 	99,99,1,0,1, # First two missing
#' 	1,0,99,0,1, # One in middle missing
#' 	1,99,99,99,1, # All in the middle missing
#' 	1,0,1,0,99, # Last one missing
#' 	1,0,1,99,999, # Last two missing
#' 	1,0,99,999,999, # Last three missing
#' 	9999,9999,9999,9999,9999 # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#'
#' y_i2 <- omitted_recoder_df(input) #Mislevy item estimation
#' y_p2 <- omitted_recoder_df(input, skipped = 0L, #Mislevy person estimation
#'                            not_administered = 0L, all_missing = 0L)
#' y_info2 <- omitted_recoder_df(input, skipped = 99,
#'                               not_administered = 999, all_missing = 9999)
#' identical(y_i, y_i2)
#' identical(y_p, y_p2)
#' identical(y_info, y_info2)
#' \dontrun{
#' omitted_recoder_df(input[,4]) # Should fail
#' }
#' identical(omitted_recoder_df(input[,4], accept_vector=TRUE),
#'          c(0,0,0,0,0,0,0,NA,NA))
#' identical(omitted_recoder_df(input[,4, drop=FALSE]),
#'           input[,4, drop=FALSE]) # Output should equal input
#'
omitted_recoder_df <- function(df, accept_vector=FALSE, skipped=0L,
							   not_administered=NA_integer_,
							   all_missing=NA_integer_) {
	omittedRecoderVec <- function(vec) {
		vec_new <- vec
		N <- length(vec)
		if(all(is.na(vec))) {
			vec_new <- rep(all_missing, times=N)
		} else {
			for(i in N:1L) { # Going backwards on all a person's responses,

				if(is.na(vec[i])) { # if the response is blank AND either
					if((any(!is.na(vec[min(c(i+1L, N)):N]))) ||  #  1) any responses after this to the end are present OR  #i==1L && it is the first response AND
					   (i!=1L && !is.na(vec[i-1L]) && all(is.na(vec[min(c(i+1L, N)):N]))) ## or the prior response is PRESENT (if not first response)
					) {
						vec_new[i] <- skipped   # Then set this response as 'skipped'
					} else { # OR if the response is blank AND
						if((i == 1L || is.na(vec[i-1L])) && # 1) it is the first response or the prior response is MISSING AND
						   all(is.na(vec[i:N]))) { # 2) All responses from this and to the end are all blank
							vec_new[i] <- not_administered # Recode as not administered.
						}
					}
				}
			}
		}
		vec_new
	}
	if(is.atomic(df)) {
		if(!accept_vector) {
			rlang::abort("Vectors not accepted.")
		} else if(is.atomic(df)) omittedRecoderVec(df)
	} else {
		if(ncol(df)==1) {
			rlang::warn("Unable to recode single-column data.frame without knowing context.")
			df
		} else {
		  stats::setNames(as.data.frame(t(apply(df, 1, omittedRecoderVec))),
							 nm=colnames(df))
		}
	}
}









#' Create All Possible Combinations of Vector Elements with Minimum A and
#' Maximum B.
#'
#' @param vec Vector
#' @param n_min Minimum number of elements
#' @param n_max Maximum number of elements. Defaults to length of vec.
#'
#' @importFrom utils combn
#' @importFrom rlang is_integer
#' @return A data frame
#' @export
#' @examples combn_upto()
combn_upto <-
  function(vec=c("a", "b", "c", "d", "e", "f", "g"),
           n_min=6L,
           n_max=length(vec)) {
	stopifnot(rlang::is_integer(as.integer(n_min)))
	stopifnot(n_max<=length(vec))
	x <-
	  unlist(lapply(n_min:n_max, function(x) utils::combn(x = vec, m = x, simplify = F)), recursive = FALSE)
	x <- stats::setNames(x, x)
	rev(x)
}


#' Center String Vector
#'
#' @param string String vector
#' @param maxwidth Maximum width
#' @return String vector
#' @export
#' @examples center_string(string=c("This is a very long label for a graph.",
#' "But this one is even longer due to superfluous and verbose way of writing"),
#'  maxwidth=20)
center_string <- function(string, maxwidth=50) {
		vapply(string, USE.NAMES = F, function(x) {
			maxw <- stats::median(stringi::stri_length(string))
			maxw <- maxwidth
			if(stringi::stri_length(x)<maxw) {
				x
			} else {
				if(stringi::stri_length(x)>=maxw & stringi::stri_length(x)<2*maxw) {
					string_wrap(x, stringi::stri_length(x)/2)
				} else {
					if(stringi::stri_length(x)>=2*maxw) {
						string_wrap(x, stringi::stri_length(x)/3)
					}
				}
			}
		}, FUN.VALUE = character(1))
	}


###  Check that all pairs of cols share at least one observed response category
check_category_pairs <-
  function(data, cols_pos, call = rlang::caller_env(), return_error=TRUE) {
    lapply(X = seq_along(cols_pos), FUN = function(i) {
      x <- unname(cols_pos)[[i]]
      y <- names(cols_pos)[[i]]

      cols_rest <-
        cols_pos[-c(1:match(y, names(cols_pos)))]
      lapply(X = seq_along(cols_rest), FUN = function(e) {
        x2 <- unname(cols_rest)[[e]]
        y2 <- names(cols_rest)[[e]]

                     val_y <- if(is.factor(data[[y]])) levels(data[[y]]) else unique(data[[y]])
                     val_y2 <- if(is.factor(data[[y2]])) levels(data[[y2]]) else unique(data[[y2]])
                     common <- dplyr::intersect(val_y, val_y2)
                     if(length(common) == 0L) {
                       cli::cli_abort(
                         c("Unequal variables.",
                           "!" = "All variables must share at least one common category.",
                           "i" = "Column {.var {y}} and column {.var {y2}} lack common categories."
                         ),
                         call = call)
                     }
                   })
    })
    TRUE
  }

trim_columns <- function(data, cols = c(".variable_label_prefix_dep", ".variable_label_prefix_dep",
                                        ".variable_label_prefix_indep", ".variable_label_suffix_indep")) {
  for(col in cols) {
    if(is.character(data[[col]])) {
      data[[col]] <- stringi::stri_trim_both(data[[col]])
      data[[col]] <- stringi::stri_replace_all_regex(data[[col]], pattern = "[[:space:]]+", replacement = " ")
    }
  }
  data
}

# get_main_question <-
#   function(data, cols_pos, label_separator) {
#   x <- unlist(lapply(data[, cols_pos], FUN = function(.x) attr(.x, "label")))
#   x <- unname(x)
#   x <-
#     stringi::stri_replace(string = x,
#                          regex = stringi::stri_c(ignore_null=TRUE, "(^.*)", label_separator, "(.*$)"),
#                          replacement = "$1")
#   x <- unique(x)
#   x <-
#     stringi::stri_c(ignore_null=TRUE, x, collapse="\n")
#   x
# }


#' Mutate a (factor, character, integer, etc) column into multiple columns,
#'
#' @description Easily mutate a single column into multiple columns (~dummies+1),
#' while retaining variable labels and order of the original factor variable.
#'
#' @inheritParams draft_report
#' @param col Single column. Tidy-select.
#' @param var_separator *Variable separator*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Separator between old variable name and categories.
#'
#' @param label_separator
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Separator between old label name and new label part.
#'
#' @return Original data frame with the binary columns attached, containing new labels.
#' @export
#'
#' @examples col_to_binaries(ex_survey, col = b_3, label_separator = "  -  ")
col_to_binaries <- function(data, col,
                            var_separator = "___",
                            label_separator = " - ") {
  if(length(dplyr::select(data, {{col}}))>1L) {
    cli::cli_abort(c("Only 1 column is currently allowed, for your protection.",
                     i="You have provided {length(dplyr::select(data, {{col}})} columns."))
  }
  col_enq <- rlang::enquo(arg = col)
  col_nm <- rlang::as_name(x = col_enq)
  col_pos <- tidyselect::eval_select(expr = col_enq, data = data)
  col_label <- attr(data[[col_pos]], "label")

  if(is.factor(data[, col_pos, drop=TRUE]) | is.ordered(data[, col_pos, drop=TRUE]) |
     is.integer(data[, col_pos, drop=TRUE]) | is.numeric(data[, col_pos, drop=TRUE])) {
    data2 <-
      data |>
      dplyr::arrange(as.numeric({{col}}))
  } else {
    data2 <-
      data |>
      dplyr::arrange({{col}})
  }
  data3 <-
    data2 |>
    dplyr::select({{col}}) |>
    dplyr::mutate(`_dummy` = 1L,
                  `_id` = seq_len(nrow(data))) |>
    tidyr::pivot_wider(names_from = {{col}},
                       values_from = tidyselect::all_of("_dummy"),
                       values_fill = 0L,
                       names_glue = stringi::stri_c(ignore_null=TRUE, col_nm, var_separator, "{.name}")) |> #
    dplyr::select(!tidyselect::all_of("_id"))


  new_labels <-
    stringi::stri_c(ignore_null=TRUE, col_label, label_separator, unique(data2[[col_pos]]))

  for(i in seq_len(ncol(data3))) {
    attr(data3[[i]], "label") <- new_labels[i]
  }
  dplyr::bind_cols(data2, data3)
  }


create_text_collapse <-
  function(text = NULL,
           last_sep = NULL) {
    if(!rlang::is_string(last_sep)) last_sep <-
        eval(formals(draft_report)$translations)$last_sep
    cli::ansi_collapse(text, last = last_sep)
  }

# are all elements of list x identical to each other?
compare_many <- function(x) {
  all(unlist(lapply(as.list(x[-1]),
                    FUN = function(.x) identical(.x, x[[1]])))) ||
    nrow(x[[1]])==1
}
