
#' Are All Colours in Vector Valid Colours
#'
#' As title says. From: (https://stackoverflow.com/a/13290832/3315962)
#'
#' @param x Character vector of colours in hex-format.
#'
#' @return Logical, or error.
#' @export
#'
#' @examples
#' is_colour(c("#ff00ff", "#010101"))
is_colour <- function(x) {
  if(!rlang::is_character(x)) return(FALSE)
  sapply(x, function(X) { # Avoid sapply
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE)
  })
}

#' Identify Suitable Font Given Background Hex Colour
#'
#' Code is taken from XXX.
#'
#' @param hex_code Colour in hex-format.
#'
#' @return Colours in hex-format, either black or white.
#' @export
#'
#' @examples
#' hex_bw("#0dadfd")
hex_bw <- function(hex_code) {

  rgb_conv <-
    lapply(grDevices::col2rgb(hex_code), FUN = function(.x) {
      ifelse(.x / 255 <= 0.04045,
             .x * 12.92 / 255,
             ((.x / 255 + 0.055) / 1.055) ^ 2.4)
    }) %>%
    unlist() %>%
    matrix(ncol = length(hex_code), byrow = FALSE) %>%
    sweep(., MARGIN = 1, STATS = c(0.2126, 0.7152, 0.0722), FUN = `*`) %>%
    apply(., MARGIN = 2, FUN = sum)

  hex <- ifelse(rgb_conv > 0.179,
                "#000000",
                "#ffffff")

  hex[is.na(hex_code)] <- NA
  hex

}

check_colour_palette <- function(colour_palette, call = rlang::caller_env()) {
  if(!is.null(colour_palette) && (!all(is_colour(colour_palette)) ||
                                  rlang::is_function(colour_palette))) {
    cli::cli_abort(
      c("Invalid user-specified colours.",
        i="{.arg colour_palette} must be a character vector of valid colours in hex-format (e.g. #000000), or a function.",
        i="Problems with {colour_palette}."),
      call = call)
  }
}




get_remaining_colours <- function(user_colour_set,
                                  n_colours_needed,
                                  ordinal = FALSE) {
  check_colour_palette(user_colour_set)

  if(!is.null(user_colour_set)) {

    if(rlang::is_function(user_colour_set)) {
      user_colour_set <- user_colour_set()
    }

    if(length(user_colour_set) >= n_colours_needed) {
      if(ordinal) {
        return(
          subset_vector(vec = user_colour_set, set = ".spread",
                        spread_n = n_colours_needed)
        )
      } else return(user_colour_set[seq_len(n_colours_needed)])

    } else {
      cli::cli_warn("Fewer colours in user-provided colour palette than needed.")
    }
  }
  hues <- seq(15, 375, length = n_colours_needed + 1)
  return(grDevices::hcl(h = hues, l = 65, c = 100)[1:n_colours_needed])
}


#' Provide A Colour Set for A Number of Requested Colours
#'
#' Possibly using colour_palette_nominal if available. If not sufficient, uses a set
#'     palette from RColorBrewer.
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @param x Vector for which colours will be found.
#' @param colour_palette_nominal,colour_palette_ordinal *User specified colour set*
#'
#'  `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   User-supplied default palette, excluding `colour_na`.
#'
#' @param common_data_type *factor or ordered data type*
#'
#'  `scalar<character>` // *default:* `factor` (`optional`)
#'
#'  Currently only supports factor and ordered.
#'
#' @param ordinal
#'
#'  `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'  Is palette ordinal?
#'
#' @return A colour set as character vector, where `NA` has the `colour_na`, and the rest are taken from colour_palette_nominal if available.
#' @export
#' @examples
#' get_colour_set(x=1:4)
get_colour_set <-
  function(x,
           common_data_type = "factor",
           colour_palette_nominal = NULL,
           colour_palette_ordinal = NULL,
           colour_na = NULL,
           colour_2nd_binary_cat = NULL,
           ordinal = FALSE,
           categories_treated_as_na = NULL,
           call = rlang::caller_env()) {

    user_colour_set <-
      switch(common_data_type,
            ordered = colour_palette_ordinal,
            factor = colour_palette_nominal)



    x <- stats::setNames(x, x)
    if(rlang::is_character(colour_na)) {
      if(length(colour_na)>= length(categories_treated_as_na)) {
        for(i in seq_along(categories_treated_as_na)) {
          x[names(x) == categories_treated_as_na[i]] <- colour_na[i]
        }
      } else {
        x[names(x) %in% categories_treated_as_na] <- colour_na[1]
      }
    }
    n_colours_needed <- length(x[!x %in% colour_na])
    if(!rlang::is_null(colour_2nd_binary_cat) &&
       n_colours_needed == 2) {
      x[!x %in% colour_na][2] <- colour_2nd_binary_cat
      n_colours_needed <- 1
    }

    colours_available <-
      get_remaining_colours(
        user_colour_set = user_colour_set,
        n_colours_needed = n_colours_needed,
        ordinal = ordinal)

    if(!rlang::is_null(colour_2nd_binary_cat) &&
       n_colours_needed == 1) {
      x[!x %in% colour_na][1] <- colours_available
    } else {

      tryCatch(
        x[!x %in% colour_na] <- colours_available[seq_along(x[!x %in% colour_na])], ## PERHAPS NOT PERFECT HERE?
      warning = function(e) cli::cli_warn(stringi::stri_c(ignore_null=TRUE, "colours_available: {colours_available}. 'x' yields {x}.", e)),
      error = function(e) cli::cli_warn(stringi::stri_c(ignore_null=TRUE, "colours_available: {colours_available}. 'x' yields {x}.", e)))
    }
    x
  }


#' Provide A Colour Set for A Number of Requested Colours
#'
#' Possibly using colour_palette_nominal if available. If not sufficient, uses a set
#'     palette from RColorBrewer.
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @param col_pos Character vector of column names for which colours will be found.
#' @param colour_palette_nominal,colour_palette_ordinal *User specified colour set*
#'
#'  `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   User-supplied default palette, excluding `colour_na`.
#'
#' @return A colour set as character vector, where `NA` has the `colour_na`, and the rest are taken from colour_palette_nominal if available.
#' @export
#' @examples
#' get_colour_palette(ex_survey, col_pos=c("b_1", "b_2"))
#' get_colour_palette(ex_survey, col_pos=c("b_1", "b_2"),
#'                   colour_palette_nominal = c("red", "blue", "orange"))
get_colour_palette <-
  function(
    data,
    col_pos,
    colour_palette_nominal = NULL,
    colour_palette_ordinal = NULL,
    colour_na = NULL,
    categories_treated_as_na = NULL,
    call = rlang::caller_env()) {

    out <-
    lapply(col_pos, function(col) {
      attr(data[[col]], "colour_palette")
    })
    out <- unique(out)[lengths(out) > 0]
    if(length(out) > 1) {
      cli::cli_warn("Multiple colour palettes embedded in {col_pos}. Using first one.")
    }
    if(length(out) >= 1) {
      out <- out[[1]]
      if(rlang::is_null(names(out))) {
        out <- rlang::set_names(out, col_pos)
      }
      return(out)
    }

    common_data_type <-
      get_common_data_type(data = data,
                           col_pos = col_pos)

    ## If nothing specified, use the default palette
    common_levels <-
      get_common_levels(data = data,
                        col_pos = col_pos)

    out <- stats::setNames(common_levels, common_levels)

    user_colour_set <-
      switch(common_data_type,
             ordered = colour_palette_ordinal,
             factor = colour_palette_nominal)
    # if(rlang::is_function(user_colour_set)) {
    #   return(user_colour_set)
    # }

    categories_treated_as_na <- categories_treated_as_na[categories_treated_as_na %in% common_levels]
    if(rlang::is_character(colour_na)) {
      if(length(colour_na)>= length(categories_treated_as_na)) {
        for(i in seq_along(categories_treated_as_na)) {
          out[names(out) == categories_treated_as_na[i]] <- colour_na[i]
        }
      } else {
        out[names(out) %in% categories_treated_as_na] <- colour_na[1]
      }
    }
    n_colours_needed <- length(out[!out %in% colour_na])

    colours_available <-
      get_remaining_colours(
        user_colour_set = user_colour_set,
        n_colours_needed = n_colours_needed,
        ordinal = common_data_type == "ordered")


      tryCatch(
        out[!out %in% colour_na] <- colours_available[seq_along(out[!out %in% colour_na])], ## PERHAPS NOT PERFECT HERE?
        warning = function(e) cli::cli_warn(stringi::stri_c(ignore_null=TRUE, "colours_available: {colours_available}. 'out' yields {out}.", e)),
        error = function(e) cli::cli_warn(stringi::stri_c(ignore_null=TRUE, "colours_available: {colours_available}. 'out' yields {out}.", e)))

    out
  }

