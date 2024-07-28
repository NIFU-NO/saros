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
    }) |>
    unlist() |>
    matrix(ncol = length(hex_code), byrow = FALSE) |>
    sweep(MARGIN = 1, STATS = c(0.2126, 0.7152, 0.0722), FUN = `*`) |>
    apply(MARGIN = 2, FUN = sum)

  hex <- ifelse(rgb_conv > 0.179,
                "#000000",
                "#ffffff")

  hex[is.na(hex_code)] <- "#ffffff"
  hex

}

