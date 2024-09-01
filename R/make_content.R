#' Method for Creating Saros Contents
#'
#' Takes the same arguments as `makeme`, except
#' that dep and indep in make_content are character vectors,
#' for ease of user-customized function programming.
#'
#' @param type *Method name*
#'
#'   `scalar<character>` with a class named by itself.
#'
#'   Optional string indicating the specific method. Occasionally
#'   useful for error messages, etc.
#'
#' @param ... *Dots*
#'
#'   Arguments provided by `makeme`
#'
#' @returns The returned object class depends on the type.
#'   `type="*_table_html"` always returns a `tibble`.
#'   `type="*_plot_html"` always returns a `ggplot`.
#'   `type="*_docx"` always returns a `rdocx` object if `path=NULL`,
#'    or has side-effect of writing docx file to disk if `path` is set.
#'
#' @export
make_content <- function(type, ...) {
  class(type) <- type
  UseMethod("make_content", type)
}



#' Get all registered options for the type-argument in the `makeme`-function
#'
#'
#' @description
#' The [saros::makeme()]-function take for the argument `type`
#' one of several strings to indicate content type and output type.
#' This function collects all registered alternatives. Extensions are possible,
#' see further below.
#'
#'
#' Built-in types:
#'
#' Whereas the names of the types can be arbitrary, a pattern is pursued in the
#' built-in types.
#' Prefix indicates what dependent data type it is intended for
#' \describe{
#' \item{"cat"}{Categorical (ordinal and nominal) data.}
#' \item{"chr"}{Open ended responses and other character data.}
#' \item{"int"}{Integer and numeric data.}
#' }
#'
#' Suffix indicates output
#' \describe{
#' \item{"html"}{Interactive html, usually what you want for Quarto, as Quarto can usually convert to other formats when needed}
#' \item{"docx"}{However, Quarto's and Pandoc's docx-support is currently still limited, for instance as vector graphics are converted to raster graphics for docx output. Hence, `saros` offers some types that outputs into MS Chart vector graphics. Note that this is experimental and not actively developed.}
#' \item{"pdf"}{This is basically just a shortcut for "html" with `interactive=FALSE`}
#' }
#'
#' # Further details about some of the built-in types:
#' \describe{
#' \item{"cat_plot_"}{A Likert style plot for groups of categorical variables sharing the same categories.}
#' \item{"cat_table_"}{A Likert style table.}
#' \item{"chr_table_"}{A single-column table listing unique open ended responses.}
#' \item{"sigtest_table_"}{See below}
#' }
#'
#' sigtest_table_\*: Make Table with All Combinations of
#' Univariate/Bivariate Significance Tests Based on Variable Types
#'
#'  Although there are hundreds of significance tests for associations between
#'  two variables, depending upon the distributions, variables types and
#'  assumptions, most fall into a smaller set of popular tests. This function
#'  runs for all combinations of dependent and independent variables in data,
#'  with a suitable test (but not the only possible) for the combination. Also
#'  supports univariate tests, where the assumptions are that of a mean of zero
#'  for continuous variables or all equal proportions for binary/categorical.
#'
#'  This function does not allow any adjustments - use the original underlying
#'  functions for that (chisq.test, t.test, etc.)
#'
#' # Expanding with custom types
#'
#' [saros::makeme()] calls the generic [saros::make_content()],
#' which uses the S3-method system to dispatch to the relevant method (i.e.,
#' `paste0("make_content.", type)`). makeme forwards all its arguments to make_content,
#' with the following exceptions:
#'
#'  1. dep and indep are converted from [dplyr::dplyr_tidy_select()]-syntax to simple character vectors, for simplifying building your own functions.
#'  1. data_summary is attached, which contains many useful pieces of info for many (categorical) displays.
#'
#' @return Character vector
#' @export
#'
#' @examples get_makeme_types()
get_makeme_types <- function() {
  out <- as.character(utils::methods("make_content"))
  stringi::stri_replace_first_fixed(out[out != "make_content.default"],
                                    pattern = "make_content.", replacement = "")
}


#' @export
make_content.default <- function(type, ...) {
  dots <- rlang::list2(...)
  cli::cli_abort("Invalid make_content-type: {type}. Check that you have loaded the required packages/methods and registererd your custom method.")
}
