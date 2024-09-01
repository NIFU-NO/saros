#' Wrapper Function for [ggplot2::ggsave()]
#'
#' This only exists to make it easy to use it in [make_link()]
#'
#' @param plot Plot
#' @param filename Note
#' @param ... Arguments forwarded to [ggplot2::ggsave()]
#'
#' @export
#' @returns No return value, called for side effects
#'
#' @examples
#' library(ggplot2)
#' my_plot <- ggplot(data=mtcars, aes(x=hp, y=mpg)) + geom_point()
#' make_link(my_plot, folder=tempdir(), file_suffix = ".png",
#'           save_fn = ggsaver, width = 16, height = 16, units = "cm")
ggsaver <- function(plot, filename, ...) {
  ggplot2::ggsave(filename = filename, plot = plot,
                  dpi = "retina", create.dir = TRUE, ...)
}
