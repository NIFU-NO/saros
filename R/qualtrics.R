#' Re-attach label information from Qualtrics not obtained from regular data downloads
#'
#' @param data Data.frame with original variable names.
#' @param questions Data frame with questions obtained from `qualtRics::survey_questions()`
#' @param reverse_stata_replacement If variable names have already been modified
#' with full stops changed to underscores, this will reverse them for connection. Rarely needed. Defaults to FALSE.
#'
#' @return Data returned with only variable labels modified.
#' @export
attach_qualtrics_labels <- function(data, questions, reverse_stata_replacement=FALSE) {
  if(!inherits(data, "data.frame")) cli::cli_abort("{.arg data} must be of type data.frame, not {.obj_type_friendly {data}}.")
  for(col in colnames(data)) {


    patterns <- c("^Q[0-9]+_[0-9]+_[0-9a-z]+",
                  "^Q[0-9]+_[0-9a-z]+",
                  "^Q[0-9a-z]+")
    col2 <- col
    for(pat in patterns) {
      if(stringi::stri_count_fixed(col2, pattern = "_") == 4-match(pat, patterns)) {
        col2 <- stringi::stri_extract_first_regex(col2, pattern = pat)
        break
      }
    }
    if(reverse_stata_replacement) col2 <- stringi::stri_replace_first_fixed(col2, pattern = "_", replacement = ".")


    if(!is.na(col2)) {
      main_question <- unname(questions[questions$qname == col2, "question", drop=TRUE])
      main_question <- stringi::stri_trim_both(main_question)
      if(length(main_question)>0 && !is.na(main_question)) {
        attr(data[[col]], "label") <- stringi::stri_c(main_question, " - ", attr(data[[col]], "label"), ignore_null = TRUE)
      }
    }

  }
  cli::cli_progress_done("Finished!")
  data
}



#' Sanitize labels originating from e.g. Qualtrics for use in Saros
#'
#' This function is quite specific to a few problems, users might find it lacking in functionality.
#'
#' @param data data.frame or tibble
#' @param sep String, separates main question from subquestion
#' @param multi_sep_replacement String. If multiple sep are found, replace the first ones with this.
#'
#' @return Identical data.frame as input, with only variable labels changed.
#' @export
#'
#' @examples sanitize_labels(ex_survey1)
sanitize_labels <- function(data, sep = " - ", multi_sep_replacement = ": ") {

  # scrape lookup table of accented char html codes, from the 2nd table on this page
  ref_url <- 'http://www.w3schools.com/charsets/ref_html_8859.asp'
  char_table <- rvest::read_html(ref_url)
  char_table <- rvest::html_table(char_table)
  char_table <- char_table[1:4]
  char_table <- lapply(char_table, function(x) x[, c("Character", "Entity Name")])
  char_table <- rbind(char_table)
  char_table <- char_table[!duplicated(char_table$`Entity Name`), ]
  # fix names
  names(char_table) <- stringi::stri_trans_tolower(names(char_table))
  names(char_table) <- stringi::stri_replace_all_fixed(names(char_table), pattern = ' ', replacement = '_')
  char_table <- char_table[char_table$entity_name != "", ]

  # here's a test string loaded with different html accents
  # test_str <- '&Agrave; &Aacute; &Acirc; &Atilde; &Auml; &Aring; &AElig; &Ccedil; &Egrave; &Eacute; &Ecirc; &Euml; &Igrave; &Iacute; &Icirc; &Iuml; &ETH; &Ntilde; &Ograve; &Oacute; &Ocirc; &Otilde; &Ouml; &times; &Oslash; &Ugrave; &Uacute; &Ucirc; &Uuml; &Yacute; &THORN; &szlig; &agrave; &aacute; &acirc; &atilde; &auml; &aring; &aelig; &ccedil; &egrave; &eacute; &ecirc; &euml; &igrave; &iacute; &icirc; &iuml; &eth; &ntilde; &ograve; &oacute; &ocirc; &otilde; &ouml; &divide; &oslash; &ugrave; &uacute; &ucirc; &uuml; &yacute; &thorn; &yuml;'

  # use mgsub from here (it's just gsub with a for loop)
  # http://stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub

  data <- lapply(rlang::set_names(colnames(data)), FUN = function(var) {
    label <- attr(data[[var]], "label")
    if(rlang::is_string(label)) {
      for(i in nrow(char_table)) {
        label <- stringi::stri_replace_all_fixed(str = label,
                                        pattern = char_table[i, "entity_name"],
                                        replacement = char_table[i, "character"])
      }

      label <- stringi::stri_replace_all_regex(label, pattern = "- Selected Choice ", replacement = "- ")
      label <- stringi::stri_replace_all_regex(label, pattern = "<.+?>|\\[.*\\]| - tekst", replacement = "")
      label <- stringi::stri_replace_all_regex(label, pattern = "\\$\\{[[:alnum:]]+[^[:alnum:]]([[:alnum:]]+)\\}", replacement = "$1")
      label <- stringi::stri_replace_all_regex(label, pattern = "\\{%name:([[:alnum:]]+) expression:.+?%\\}", replacement = "$1")
      label <- stringi::stri_replace_all_regex(label, pattern = "\\{%expression:.+?%\\}", replacement = "")
      label <- stringi::stri_replace_all_regex(label, pattern = "[[:space:]\n\r\t]+", replacement = " ")
      if(stringi::stri_count_fixed(label, " - ")>=2) label <- stringi::stri_replace_first_fixed(label, pattern = sep, replacement = multi_sep_replacement)
      if(stringi::stri_count_fixed(label, " - ")>=2) label <- stringi::stri_replace_first_fixed(label, pattern = sep, replacement = multi_sep_replacement)
      label <- stringi::stri_replace_all_regex(label, pattern = "^[[:space:]]|[[:space:]-:\\.]+$", replacement = "")

      attr(data[[var]], "label") <- label
    }
    data[[var]]
  })
  data.frame(data)
}
