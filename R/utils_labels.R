
#' Remove Special Characters (<,>) in Variable Labels
#'
#' @param df Data frame
#'
#' @return A data frame
#' @export

remove_special_chars_in_labels <-
  function(df) {
    z <-
      labelled::val_labels(df)
    z <-
      purrr::map2(.x = z,
                  .y = names(z),
                  .f = function(x, y) {
                    if(!is.null(x) && any(grepl("<|>", names(x)))) {
                      cli::cli_warn(c(
                        "Current version of function doesn't handle special characters `<` or `>` in labels.",
                        i="Will remove these in {{y}}"))
                      names(x)<- gsub("<|>", "", names(x))
                    }
                    x
                  })
    labelled::val_labels(df) <- z
    df
  }


# Helper function to extract main question from the data
get_main_question2 <-
  function(x, label_separator, warn_multiple = TRUE, call=rlang::caller_env()) {
    if(!(is.character(x) | is.factor(x) | is.ordered(x))) {
      cli::cli_abort(c(x="{.arg x} must be of type {.cls character} or {.cls factor}.",
                       i="not {.obj_type_friendly {x}}."),
                     call = call)
    }

    x <-
      stringr::str_replace(string = x,
                           pattern = paste0("(^.*)", label_separator, "(.*$)"),
                           replacement = "\\1") %>%
      stringr::str_unique() %>%
      stringr::str_c(., collapse="\n")
    if(length(x) > 1L && warn_multiple) {
      cli::cli_warn(c(x="There are multiple main questions for these variables.",
                      i="Check your data."), call = call)
    } else if(length(x) == 0L) {
      cli::cli_abort(c(x="No main question found.",
                       i="Check your {.arg label_separator}."), call = call)
    }
    x
  }



# Helper function to extract raw variable labels from the data
get_raw_labels <-
  function(data, cols_pos=NULL) {
    if(is.null(cols_pos)) cols_pos <- names(data)
    cols_pos %>%
      rlang::set_names() %>%
      purrr::map_chr(.f = ~{
      y <- attr(data[[.x]], "label")
      if (!is.null(y)) y else NA
    })
  }


set_var_labels <- function(data, cols=tidyselect::everything(), overwrite=TRUE) {
  cols_enq <- rlang::enquo(arg = cols)
  cols_pos <- tidyselect::eval_select(expr = cols_enq, data = data)
  col_names <- colnames(data)
  data <-
    purrr::map(seq_len(ncol(data)), ~{
      if(
        .x %in% cols_pos &&
        (overwrite || is.null(attr(data[[.x]], "label")))
      ) {
        attr(data[[.x]], "label") <- col_names[.x]
      }
      data[[.x]]
    })
  names(data) <- col_names
  tibble::as_tibble(x = data)
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
#' @importFrom dplyr arrange group_by ungroup mutate rename_with pull n
#' @importFrom vctrs as_list_of
#' @importFrom labelled lookfor
#' @importFrom tidyr separate unite
#' @importFrom purrr map_chr
#' @importFrom rlang .data
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

    df_labels <- labelled::lookfor(data = data, details = FALSE)
    df_labels <- tidyr::separate(data = df_labels, col = .data$label, sep = label_sep, fill = "right",
                                 into = c("label_pre", "label_suf"), remove = FALSE)
    df_labels$label_pre <- tolower(df_labels$label_pre)
    df_labels$label_pre_str <- vctrs::as_list_of(strsplit(x = df_labels$label_pre, split = "[[:space:][:punct:]]"))
    df_labels$label_pre2 <-
      purrr::map_chr(df_labels$label_pre_str, .f=function(.x) {
        out <- .x[!.x %in% stop_words]
        out <- if(length(out) > 0L) paste0(out, collapse=" ") else paste0(.x, collapse=" ")
        out <- paste0(out, collapse=" ")
      })
    df_labels$label_pre3 <- abbreviate(names.arg = df_labels$label_pre2,
                                       named = TRUE, minlength = 2L, dot = FALSE, method = "both")
    df_labels <- dplyr::arrange(df_labels, .data$label_pre, .data[[sort_var]])
    df_labels <- dplyr::group_by(df_labels, .data$label_pre)
    df_labels <- dplyr::mutate(df_labels,
                               label_suf_no = if(dplyr::n()==1L) NA_character_ else if(n()<10L) sprintf("%01d", seq_len(n())) else if(dplyr::n()>=10L) sprintf("%02d", seq_len(dplyr::n())))
    df_labels <- dplyr::ungroup(df_labels)
    df_labels <- tidyr::unite(df_labels, col = "variable_new", c(.data$label_pre3, .data$label_suf_no), sep = new_var_sep, na.rm = TRUE)
    data <- dplyr::rename_with(.data = data,
                               .fn = ~purrr::map_chr(.x = .,
                                                     .f = ~dplyr::pull(df_labels[df_labels$variable == .x, "variable_new"])))
    data
  }


#' Swap Dataset Columns and Labels
#'
#' Columns not containing labels will remain unaffected, and warning given.
#'
#' @param data Data frame or tibble.
#'
#' @return Data.frame/tibble.
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
