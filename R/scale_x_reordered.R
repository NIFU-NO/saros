#' Code-snippets copied and modified from tidytext-package
#' <https://github.com/juliasilge/tidytext/blob/main/R/reorder_within.R>
#'
#'
#' Permission is hereby granted, free of charge, to any person obtaining
#' a copy of this software and associated documentation files (the
#' "Software"), to deal in the Software without restriction, including
#' without limitation the rights to use, copy, modify, merge, publish,
#' distribute, sublicense, and/or sell copies of the Software, and to
#' permit persons to whom the Software is furnished to do so, subject to
#' the following conditions:
#'
#'   The above copyright notice and this permission notice shall be
#' included in all copies or substantial portions of the Software.
#'
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#' EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#' MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#' NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
#' LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
#' OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
#' WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#'
#'
#' @source "Original: Ordering categories within ggplot2 Facets" by Tyler Rinker:
#' <https://trinkerrstuff.wordpress.com/2016/12/23/ordering-categories-within-ggplot2-facets/>
#' Based on <https://opensource.org/licenses/MIT>
#' Copyright (c) 2017, Julia Silge and David Robinson
#' @param x Vector
#' @param by Vector
#' @param within Vector (factor)
#' @param fun Function, defaults to the mean
#' @param sep String, separator
#' @param ... Dots
#' @keywords internal

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  if (!is.list(within)) {
    within <- list(within)
  }

  new_x <- do.call(paste, c(list(x, sep = sep), within))
  stats::reorder(new_x, by, FUN = fun)
}


reorder_func <- function(x, sep = "___", x_axis_label_width = 20) {
  if (!is.null(sep)) {
    reg <- paste0(sep, ".+$")
    x <- stringi::stri_replace_all_regex(str = x, pattern = reg, replacement = "")
  }
  string_wrap(x, width = x_axis_label_width)
}


scale_x_reorder <- function(..., sep = "___", x_axis_label_width = 20) {
  ggplot2::scale_x_discrete(
    labels =
      function(x) reorder_func(x, sep = sep, x_axis_label_width = x_axis_label_width),
    ...
  )
}
