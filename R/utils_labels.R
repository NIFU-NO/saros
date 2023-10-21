
string_wrap <- function(str, width) {
  unlist(
    lapply(
      stringi::stri_wrap(str = str, width = width, simplify = F),
      FUN = function(.x) paste0(.x, collapse="\n")))
}

#' Remove Special Characters (<,>) in Variable Labels
#'
#' @param df Data frame
#'
#' @return A data frame
#' @export
remove_special_chars_in_labels <-
  function(df) {
    # z <-
    #   labelled::val_labels(df)
    z <-
      lapply(X = df, FUN = function(x) attr(x, "labels"))
    z <-
      lapply(X = seq_along(z), FUN = function(i) {
        x <- z[[i]]
        y <- names(z)[[i]]

                    if(!is.null(x) && any(grepl("<|>", names(x)))) {
                      cli::cli_warn(c(
                        "Current version of function doesn't handle special characters `<` or `>` in labels.",
                        i="Will remove these in {{y}}"))
                      names(x) <- stringi::stri_replace_all(str = names(x), regex = "<|>", replacement = "")
                    }
                    x
                  })
    for(i in seq_len(ncol(df))) {
      attr(df[[i]], "labels") <- z[[i]]
    }
    # labelled::val_labels(df) <- z
    df
  }


# Helper function to extract main question from the data
get_main_question2 <-
  function(x, label_separator, warn_multiple = TRUE, call=rlang::caller_env()) {
    x <- x[!is.na(x)]
    if(length(x)==0) return("")
    if(!(is.character(x) | is.factor(x) | is.ordered(x))) {
      cli::cli_abort(c(x="{.arg x} must be of type {.cls character} or {.cls factor}.",
                       i="not {.obj_type_friendly {x}}."),
                     call = call)
    }

    x <-
      stringi::stri_replace(str = x,
                           regex = stringi::stri_c(ignore_null=TRUE, "(^.*)", label_separator, "(.*$)"),
                           replacement = "$1") %>%
      unique()
    x <- if(length(x)>0) stringi::stri_c(ignore_null=TRUE, x, collapse="\n")
    if(length(x) > 1L && warn_multiple) {
      cli::cli_warn(c(x="There are multiple main questions for these variables.",
                      i="Check your data."), call = call)
    } else if(length(x) == 0L) {
      cli::cli_warn(c(x="No main question found.",
                       i="Check your {.arg label_separator}."), call = call)
    }
    x
  }



# Helper function to extract raw variable labels from the data
get_raw_labels <-
  function(data, col_pos = NULL, return_as_list = FALSE) {
    if(is.null(col_pos)) col_pos <- colnames(data)
    out <- lapply(X = stats::setNames(col_pos, nm=col_pos),
                  FUN = function(.x) {
                    y <- attr(data[[.x]], "label")
                    if(rlang::is_string(y)) y else NA_character_
                  })
    if(isFALSE(return_as_list)) out <- unlist(out)
    out
  }


set_var_labels <- function(data, cols=colnames(data), overwrite=TRUE) {
  cols_enq <- rlang::enquo(arg = cols)
  cols_pos <- tidyselect::eval_select(expr = cols_enq, data = data)
  col_names <- colnames(data)
  data <-
    lapply(colnames(data), FUN = function(.x) {
      if(
        .x %in% cols &&
        (overwrite || is.null(attr(data[[.x]], "label")))
      ) {
        attr(data[[.x]], "label") <- cols[.x]
      }
      data[[.x]]
    })
  names(data) <- cols
  vctrs::new_data_frame(vctrs::df_list(data))
}


#' Rename Dataset Columns by Labels.
#'
#' Occasionally dataframe columns have not been named logically and consistent in the
#' software where the data originates. This function renames variable names
#' based on patterns in the variable labels, after ignoring some stop words.
#'
#' @param data Dataset.
#' @param label_sep The separator between group part and unique part of label.
#' @param sort_var When numbering variables within a group, what to sort by? pos is original position in dataset,
#' @param new_var_sep When creating new variables, how to glue together variable group name prefix and numbering?
#' @param stop_words Words to ignore in label when abbreviating label to name.
#'
#' @return Data with renamed variable names.
#' @export
#'
#' @examples
#' rename_by_labels(ex_survey1)

rename_by_labels <-
  function(data,
           label_sep=" - ",
           sort_var=c("pos", "variable", "label"),
           new_var_sep="_",
           stop_words=NULL) {
    sort_var <- rlang::arg_match(sort_var)

    df_pos <- seq_len(ncol(data))
    df_name <- names(data)
    df_label <- unname(get_raw_labels(data))
    df_labels <- data.frame(pos = df_pos,
                            variable = df_name,
                            label = df_label)
    df_labels <- tidyr::separate(data = df_labels, col = .data$label, sep = label_sep, fill = "right",
                                 into = c("label_pre", "label_suf"), remove = FALSE)
    df_labels$label_pre <- tolower(df_labels$label_pre)
    df_labels$label_pre_str <- vctrs::as_list_of(strsplit(x = df_labels$label_pre, split = "[[:space:][:punct:]]"))
    df_labels$label_pre2 <-
      unlist(lapply(df_labels$label_pre_str, FUN = function(.x) {
        out <- .x[!.x %in% stop_words]
        out <- if(length(out) > 0L) stringi::stri_c(ignore_null=TRUE, out, collapse=" ") else stringi::stri_c(ignore_null=TRUE, .x, collapse=" ")
        out <- stringi::stri_c(ignore_null=TRUE, out, collapse=" ")
      }))
    df_labels$label_pre3 <- abbreviate(names.arg = df_labels$label_pre2,
                                       named = TRUE, minlength = 2L, dot = FALSE, method = "both")
    df_labels <- dplyr::arrange(df_labels, .data$label_pre, .data[[sort_var]])
    df_labels <- dplyr::group_by(df_labels, .data$label_pre)
    df_labels <- dplyr::mutate(df_labels,
                               label_suf_no = if(dplyr::n()==1L) NA_character_ else if(dplyr::n()<10L) sprintf("%01d", seq_len(dplyr::n())) else if(dplyr::n()>=10L) sprintf("%02d", seq_len(dplyr::n())))
    df_labels <- dplyr::ungroup(df_labels)
    df_labels <- tidyr::unite(df_labels, col = "variable_new", c(.data$label_pre3, .data$label_suf_no), sep = new_var_sep, na.rm = TRUE)
    data <- dplyr::rename_with(.data = data,
                               .fn = ~unlist(lapply(X = .,
                                                     FUN = function(.x) dplyr::pull(df_labels[df_labels$variable == .x, "variable_new"]))))
    data
  }


#' Swap Dataset Columns and Labels
#'
#' Columns not containing labels will remain unaffected, and warning given.
#'
#' @param data Data frame
#'
#' @return Data.frame
#' @export
#'
#' @examples swap_label_colnames(mtcars)
swap_label_colnames <-
  function(data) {
    cls <- class(data)
    if(!inherits(data, "data.frame")) cli::cli_abort("{.arg data} must be a {.cls data.frame}.")

    colname <- colnames(data)
    data <- remove_special_chars_in_labels(data)
    lacking_labels <- c()
    label <- lapply(colname, function(.x) {
      l <- attr(data[[.x]], "label")
      if(is.null(l)) {
        assign(x = "lacking_labels",
               value = c(lacking_labels, .x),
               envir = rlang::env_parent())
        .x
      } else l
    })
    label <- as.character(unlist(label))
    if(length(lacking_labels)>0L) {
      cli::cli_warn(c(i="Missing labels for columns: {.var {lacking_labels}}.",
                      i="Leaving column names untouched for these."))
    }

    data <-
      lapply(seq_along(data), function(.x) {
        if(!is.null(colname[.x])) {
          attr(data[[.x]], "label") <- colname[[.x]]
        }
        data[[.x]]
      })
    data <- as.data.frame(data)
    colnames(data) <- label
    class(data) <- cls
    data
  }
