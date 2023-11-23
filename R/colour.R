
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
#' @param colour_2nd_binary_cat If hex_code equals this, return hex_code as is.
#'
#' @return Colours in hex-format, either black or white.
#' @export
#'
#' @examples
#' hex_bw("#0dadfd")
# hex_bw <- function(hex_code, colour_2nd_binary_cat = NULL) {
#
#   myrgb <- as.integer(col2rgb(hex_code))
#
#   rgb_conv <- lapply(myrgb, function(x) {
#     i <- x / 255
#     if (i <= 0.04045) i / 12.92 else ((i + 0.055) / 1.055) ^ 2.4
#   })
#   rgb_calc <- (0.2126*rgb_conv[[1]]) + (0.7152*rgb_conv[[2]]) + (0.0722*rgb_conv[[3]])
#
#   hex <- ifelse(rgb_calc > 0.179, "#000000", "#ffffff")
#   if(!is.null(colour_2nd_binary_cat)) {
#     hex[hex_code == colour_2nd_binary_cat] <- colour_2nd_binary_cat
#   }
#   hex
#
# }

hex_bw <- function(hex_code, colour_2nd_binary_cat = NULL) {

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
  if(!is.null(colour_2nd_binary_cat)) {
    hex[hex_code == colour_2nd_binary_cat] <- colour_2nd_binary_cat
  }
  hex

}

check_colour_palette <- function(colour_palette, call = rlang::caller_env()) {
  if(!is.null(colour_palette) && !all(is_colour(colour_palette))) {
    cli::cli_abort(
      c("Invalid user-specified colours.",
        i="{.arg colour_palette} must be a character vector of valid colours in hex-format (e.g. #000000).",
        i="Problems with {{colour_palette[!is_colour(colour_palette)]}}."),
      call = call)
  }
}

get_remaining_colours <- function(user_colour_set,
                                  n_colours_needed,
                                  ordinal = FALSE) {
  check_colour_palette(user_colour_set)

  if(!is.null(user_colour_set)) {

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
  # if(n_colours_needed <= 12 && requireNamespace("RColorBrewer")) {
  #
  #     return(sample(x = RColorBrewer::brewer.pal(n = 12, name = "Paired"),
  #                   size = n_colours_needed))
  # } else if(requireNamespace("viridisLite")) {
  #   return(viridisLite::viridis(n = n_colours_needed))
  # }
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

# get_colour_set2 <-
#   function(x,
#            colour_palette_nominal=NULL,
#            colour_na = NULL,
#            colour_2nd_binary_cat = NULL,
#            call = rlang::caller_env()) {
#
#     n_colours_needed <- length(x)
#     if(!is.null(colour_palette_nominal)) {
#       check_colour_palette(colour_palette = colour_palette_nominal)
#
#       if(length(colour_palette_nominal) >= n_colours_needed) {
#         out <- subset_vector(vec = colour_palette_nominal, set = ".spread",
#                              spread_n = n_colours_needed)
#
#       } else {
#         cli::cli_warn("Fewer colours in user-provided colour palette than needed.",
#                       call = call)
#       }
#     } else if(n_colours_needed <= 12 && requireNamespace("RColorBrewer")) {
#         out <- sample(x = RColorBrewer::brewer.pal(n = 12, name = "Paired"), size = n_colours_needed)
#     } else if(requireNamespace("viridisLite")) {
#       out <- viridisLite::viridis(n = n_colours_needed)
#     }
#
#     out <- stats::setNames(out, nm = x)
#
#     if(!is.null(colour_na) && !is.na(colour_na)) {
#       check_colour_palette(colour_palette = colour_na)
#       out[names(out)=="NA"] <- colour_na
#     }
#
#     if(n_colours_needed == 2L &&
#        !is.null(colour_2nd_binary_cat)) {
#
#       check_colour_palette(colour_palette = colour_2nd_binary_cat)
#       out[2] <- colour_2nd_binary_cat
#     }
#
#     out
#   }



#' Get Colour Palette
#'
#' Give two
#'
#' @param type Character vector of variable types ("ordinal", "nominal",
#'   "interval").
#' @param unique_set_group Character vector of unique values across the battery.
#' @param unique_set Character vector of unique values within the variable.
#' @param colour_set_ordinal,colour_set_nominal Character vector with hex
#'   colours. Must be provided.
#'
#' @return Named character vector of hex colours for each element of unique_set.
#'
# colour_picker <-
#   function(type,
#            unique_set_group,
#            unique_set,
#            colour_set_ordinal,
#            colour_set_nominal) {
#     vctrs::vec_assert(x = type, ptype = character())
#     vctrs::vec_assert(x = unique_set_group, ptype = list())
#     vctrs::vec_assert(x = unique_set, ptype = list())
#
#     if(length(type) != length(unique_set_group)) {
#       rlang::abort(c("type and unique_set_group are not of equal length.",
#                      x=stringi::stri_c(ignore_null=TRUE, "type is of length ", length(type),
#                               " whereas unique_set_group is of length ", length(unique_set_group))))
#     }
#     if(length(unique_set) != length(unique_set_group)) {
#       rlang::abort(c("unique_set and unique_set_group are not of equal length.",
#                      x=stringi::stri_c(ignore_null=TRUE, "unique_set is of length ", length(unique_set),
#                               " whereas unique_set_group is of length ", length(unique_set_group))))
#     }
#     lengths_comparisons <- lengths(unique_set) <= lengths(unique_set_group)
#     if(!all(lengths_comparisons)) {
#       rlang::abort(c("unique_set and unique_set_group contain vectors of pairwise unequal lengths.",
#                      x=stringi::stri_c(ignore_null=TRUE, "Problem(s) at row ", which(!lengths_comparisons),
#                               " when unique_set is ", lengths(unique_set)[!lengths_comparisons],
#                               " and unique_set_group is ", lengths(unique_set_group)[!lengths_comparisons])))
#     }
#     # out <-
#     lapply(seq_along(type),# .y = unique_set,
#                function(i) {
#                  vctrs::vec_assert(x = unique_set_group[[i]], ptype = character())
#                  vctrs::vec_assert(x = unique_set[[i]], ptype = character())
#
#                  n_unique_set_i <- length(unique_set[[i]])
#                  n_unique_set_group_i <- length(unique_set_group[[i]])
#
#                  if(!is.na(type[i]) && type[i] %in% c("ordinal", "interval")) {
#                    unname(get_colour_set(
#                      n_colours_needed = length(unique_set[[i]]),
#                      colour_palette_nominal = colour_set_ordinal,
#                      names = unique_set_group[[i]]))[unique_set[[i]]]
#
#                  } else if(!is.na(type[i]) && type[i] == "nominal" &&
#                            n_unique_set_group_i <= 12) { # Why this limit here only?
#                    unname(get_colour_set(
#                      n_colours_needed = n_unique_set_group_i,
#                      colour_palette_nominal = colour_set_nominal,
#                      names = unique_set_group[[i]]))[unique_set[[i]]]
#                  } else NA_character_
#                })
#     # out
#   }

