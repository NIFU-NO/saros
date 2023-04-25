
#' Are All Colours in Vector Valid Colours
#'
#' As title says. From: http://stackoverflow.com/a/13290832/3315962
#'
#' @param x Character vector of colours in hex-format.
#'
#' @return Logical, or error.
#' @export
#'
#' @examples
#' is_colour(c("#ff00ff", "#010101"))
is_colour <- function(x) {
  sapply(x, function(X) { # Avoid sapply
    tryCatch(is.matrix(col2rgb(X)),
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
#' @importFrom grDevices col2rgb
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
    hex_code %>%
    col2rgb() %>%
    purrr::map(.f = ~{
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
  if(!is.null(colour_palette) & !all(is_colour(colour_palette))) {
    cli::cli_abort(
      c("Invalid user-specified colours.",
        i="{.arg colour_palette} must be a character vector of valid colours in hex-format (e.g. #000000).",
        i="Problems with {{colour_palette[!is_colour(colour_palette)]}}."),
      call = call)
  }
}

get_remaining_colours <- function(user_colour_set,
                                  n_colours_needed,
                                  call,
                                  seed) {
  check_colour_palette(user_colour_set, call = call)
  check_integerish(n_colours_needed, call = call)
  check_integerish(seed, call = call)

  if(!is.null(user_colour_set)) {

    if(length(user_colour_set) >= n_colours_needed) {
      return(
        subset_vector(vec = user_colour_set, set = ".spread",
                      spread_n = n_colours_needed)
      )

    } else {
      cli::cli_warn("Fewer colours in user-provided colour palette than needed.",
                    call = call)
    }
  }
  if(n_colours_needed <= 12 && requireNamespace("RColorBrewer")) {
    withr::with_seed(seed = seed, code = {
      return(sample(x = RColorBrewer::brewer.pal(n = 12, name = "Paired"),
                    size = n_colours_needed))
    })
  }
  if(requireNamespace("viridisLite")) {
    return(viridisLite::viridis(n = n_colours_needed))
  }
}


#' Provide A Colour Set for A Number of Requested Colours
#'
#' Possibly using user_colour_set if available. If not sufficient, uses a set
#'     palette from RColorBrewer.
#'
#' @param x Vector for which colours will be found.
#' @param user_colour_set [\code{character()>0}]\cr User-supplied default palette, excluding colour_na.
#' @param colour_na [\code{character(1)}]\cr Colour as a single string.
#' @param colour_2nd_binary_cat [\code{character(1)}]\cr Colour for second category in binary variables. Often useful to hide this.
#' @param seed [\code{integer(1)}]\cr Random seed for sampling.
#' @param call Caller function if used from inside another function.
#'
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom stats median
#' @return A colour set as character vector, where NA has the colour_na, and the rest are taken from user_colour_set if available.
#' @export
#' @examples
#' get_colour_set(x=1:4)
get_colour_set <-
  function(x,
           user_colour_set=NULL,
           colour_na = NULL,
           colour_2nd_binary_cat = NULL,
           seed=1,
           call = rlang::caller_env()) {

    check_colour(colour_na, call = call)
    check_colour(colour_2nd_binary_cat, call = call)



    x <- rlang::set_names(x)
    names(x)[is.na(names(x))] <- "NA"
    if(!rlang::is_null(colour_na)) {
      x[names(x) == "NA"] <- colour_na
    }
    n_colours_needed <- length(x[!names(x) == "NA"])
    if(!rlang::is_null(colour_2nd_binary_cat) &&
       n_colours_needed == 2) {
      x[!names(x) == "NA"][2] <- colour_2nd_binary_cat
      n_colours_needed <- 1
    }

    colours_available <-
      get_remaining_colours(
        user_colour_set = user_colour_set,
        n_colours_needed = n_colours_needed,
        call = call,
        seed = seed)

    if(!rlang::is_null(colour_2nd_binary_cat) &&
       n_colours_needed == 1) {
      x[!names(x) == "NA"][1] <- colours_available
    } else {
      x[!names(x) == "NA"] <- colours_available
    }
    x
  }

get_colour_set2 <-
  function(x,
           user_colour_set=NULL,
           colour_na = NULL,
           colour_2nd_binary_cat = NULL,
           seed=1,
           call = rlang::caller_env()) {

    n_colours_needed <- length(x)
    if(!is.null(user_colour_set)) {
      check_colour_palette(colour_palette = user_colour_set)

      if(length(user_colour_set) >= n_colours_needed) {
        out <- subset_vector(vec = user_colour_set, set = ".spread",
                             spread_n = n_colours_needed)

      } else {
        cli::cli_warn("Fewer colours in user-provided colour palette than needed.",
                      call = call)
      }
    } else if(n_colours_needed <= 12 && requireNamespace("RColorBrewer")) {
      withr::with_seed(seed = seed, code = {
        out <- sample(x = RColorBrewer::brewer.pal(n = 12, name = "Paired"), size = n_colours_needed)
      })
    } else if(requireNamespace("viridisLite")) {
      out <- viridisLite::viridis(n = n_colours_needed)
    }

    out <- rlang::set_names(out, nm = x)

    if(!is.null(colour_na) && !is.na(colour_na)) {
      check_colour_palette(colour_palette = colour_na)
      out[names(out)=="NA"] <- colour_na
    }

    if(n_colours_needed == 2L &&
       !is.null(colour_2nd_binary_cat)) {

      check_colour_palette(colour_palette = colour_2nd_binary_cat)
      out[2] <- colour_2nd_binary_cat
    }

    out
  }



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
#' @importFrom rlang abort
#' @importFrom vctrs vec_assert
#' @importFrom purrr map2
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
#                      x=paste0("type is of length ", length(type),
#                               " whereas unique_set_group is of length ", length(unique_set_group))))
#     }
#     if(length(unique_set) != length(unique_set_group)) {
#       rlang::abort(c("unique_set and unique_set_group are not of equal length.",
#                      x=paste0("unique_set is of length ", length(unique_set),
#                               " whereas unique_set_group is of length ", length(unique_set_group))))
#     }
#     lengths_comparisons <- lengths(unique_set) <= lengths(unique_set_group)
#     if(!all(lengths_comparisons)) {
#       rlang::abort(c("unique_set and unique_set_group contain vectors of pairwise unequal lengths.",
#                      x=paste0("Problem(s) at row ", which(!lengths_comparisons),
#                               " when unique_set is ", lengths(unique_set)[!lengths_comparisons],
#                               " and unique_set_group is ", lengths(unique_set_group)[!lengths_comparisons])))
#     }
#     # out <-
#     purrr::map(.x = seq_along(type),# .y = unique_set,
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
#                      user_colour_set = colour_set_ordinal,
#                      names = unique_set_group[[i]]))[unique_set[[i]]]
#
#                  } else if(!is.na(type[i]) && type[i] == "nominal" &&
#                            n_unique_set_group_i <= 12) { # Why this limit here only?
#                    unname(get_colour_set(
#                      n_colours_needed = n_unique_set_group_i,
#                      user_colour_set = colour_set_nominal,
#                      names = unique_set_group[[i]]))[unique_set[[i]]]
#                  } else NA_character_
#                })
#     # out
#   }

