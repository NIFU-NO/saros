custom_palette <- function(palette_codes, fct_levels, priority_palette_codes = NULL) {
    function(n, lvls = fct_levels) {
        matched_palette <- NULL
        for (i in seq_along(palette_codes)) {
            if (all(lvls %in% names(palette_codes[[i]]))) {
                matched_palette <- palette_codes[[i]]
                break
            }
        }
        # matched_palette <- purrr::detect(palette_codes, function(palette) {
        #     all(lvls %in% names(palette))
        # })

        if (is.null(matched_palette) &&
            length(palette_codes[[length(palette_codes)]]) >= length(lvls)) { # If no match, pick the last vector in list

            matched_palette <- priority_palette_codes[names(priority_palette_codes) %in% lvls] # Will insert e.g. c("Nei" = "black")
            available_palette <- palette_codes[[length(palette_codes)]]
            matched_palette <- c(
                matched_palette,
                stats::setNames(
                    available_palette[!unname(available_palette) %in% unname(matched_palette)],
                    lvls[!lvls %in% names(matched_palette)]
                )
            )
        }
        if (is.null(matched_palette) && length(palette_codes[[length(palette_codes)]]) < length(lvls)) { # If no match and insufficient palettes, then generate
            matched_palette <- scales::hue_pal()(length(lvls))
        }

        return(matched_palette)
    }
}

guess_legend_ncols <- function(ggobj, char_limit = 100) {
    fill_var <- rlang::as_label(ggobj$mapping$fill)
    if (!is.null(fill_var)) {
        lvls <- as.character(unique(ggobj$data[[fill_var]]))
        # print(lvls)
        max_chars <- max(nchar(lvls), na.rm = TRUE)
        for (i in 2:15) {
            if ((max_chars + 5) * i >= char_limit) {
                return(i - 1)
            }
        }
    }
    return(NULL)
}

scale_discrete_special <- function(aesthetics = "fill", palette_codes, lvls = NULL,
                                   ncol = NULL, byrow = TRUE, label_wrap_width = 80,
                                   priority_palette_codes = NULL, ...) {
    if (is.null(lvls)) {
        ggiraph::scale_discrete_manual_interactive(
            aesthetics = aesthetics,
            name = "",
            values = palette_codes[[length(palette_codes)]],
            guide = ggiraph::guide_legend_interactive(
                title = "",
                ncol = ncol,
                byrow = byrow
            ),
            labels = ggplot2::label_wrap_gen(width = label_wrap_width, multi_line = TRUE),
            ...
        )
    } else {
        ggplot2::discrete_scale(
            aesthetics = aesthetics,
            name = "",
            palette = custom_palette(palette_codes = palette_codes, fct_levels = lvls, priority_palette_codes = priority_palette_codes),
            guide = ggiraph::guide_legend_interactive(
                title = "",
                ncol = ncol,
                byrow = byrow
            ),
            labels = ggplot2::label_wrap_gen(width = label_wrap_width, multi_line = TRUE),
            ...
        )
    }
}

convert_to_checkbox_plot <- function(ggobj, checked = "Selected", not_checked = "Not selected") {
    ggobj$data <-
        ggobj$data |>
        dplyr::mutate(
            .category = forcats::fct_relevel(.data$.category, .env$checked, .env$not_checked),
            .data_label = ifelse(.data$.category == .env$not_checked, "", .data$.data_label)
        )
    ggobj
}

#' Pull global plotting settings before displaying plot
#'
#' This function extends [ggiraph::girafe] by allowing colour palettes to be globally specified.
#' @param ggobj ggplot2-object.
#' @param ... Dots forwarded to [ggiraph::girafe()]
#' @param char_limit Integer. Number of characters to fit on a line of plot (legend-space). Will be
#'    replaced in the future with a function that guesses this.
#' @param label_wrap_width Integer. Number of characters fit on the axis text space before wrapping.
#' @param interactive Boolean. Whether to produce a ggiraph-plot with interactivity (defaults to TRUE)
#'    or a static ggplot2-plot.
#' @param palette_codes Optional list of named character vectors with names being categories and values being colours. The final character vector of the list is taken as a final resort. Defaults to `NULL`.
#' @param priority_palette_codes Optional named character of categories (as names) with corresponding colours (as values) which are used first, whereupon the remaining unspecified categories are pulled from the last vector of `palette_codes`. Defaults to `NULL`.
#' @param ncol Optional integer or NULL.
#' @param byrow Whether to display legend keys by row or by column.
#' @param checked,not_checked Optional string. If specified and the fill categories of the plot matches these, a special plot is returned where not_checked is hidden. Its usefulness comes in plots which are intended for checkbox responses where unchecked is not always a conscious choice.
#' @param pointsize,height_svg,width_svg See [ggiraph::girafe()].
#' @return If interactive, only side-effect of generating ggiraph-plot. If interactive=FALSE, returns modified ggobj.
#' @export
#'
#' @examples
#' plot <- makeme(data = ex_survey, dep = b_1)
#' girafe(plot)
girafe <- function(
    ggobj,
    ...,
    char_limit = 200,
    label_wrap_width = 80,
    interactive = TRUE,
    palette_codes = NULL,
    priority_palette_codes = NULL,
    ncol = NULL,
    byrow = TRUE,
    checked = NULL,
    not_checked = NULL,
    width_svg = NULL,
    height_svg = NULL,
    pointsize = 12) {
    # Check global settings, then provided settings, and finally resort to defaults.
    args <- check_options(
        call = match.call(),
        ignore_args = .saros.env$ignore_args,
        defaults_env = global_settings_get(fn_name = "girafe"),
        default_values = formals(girafe)
    )


    # If in a Quarto/Rmarkdown document rendering context, avoid doing the tests below unnecessarily often?
    if (!(rlang::is_integerish(args$char_limit))) cli::cli_abort("{.arg char_limit} must be an integer.")
    if (!(rlang::is_integerish(args$label_wrap_width))) cli::cli_abort("{.arg label_wrap_width} must be an integer.")
    if (!(rlang::is_integerish(args$ncol) || is.null(args$ncol))) cli::cli_abort("{.arg ncol} must be an integer or NULL.")
    if (!(rlang::is_integerish(args$ncol) || is.null(args$ncol))) cli::cli_abort("{.arg ncol} must be an integer or NULL.")
    if (!((rlang::is_list(args$palette_codes) && all(unlist(lapply(args$palette_codes, is.character)))) || is.null(args$palette_codes))) cli::cli_abort("{.arg priority_palette_codes} must be NULL or a list of character vectors.")
    if (!((rlang::is_character(args$priority_palette_codes) && rlang::is_named(args$priority_palette_codes)) || is.null(args$priority_palette_codes))) cli::cli_abort("{.arg priority_palette_codes} must be a named character vector or NULL.")
    if (!(rlang::is_bool(args$interactive))) cli::cli_abort("{.arg interactive} must be boolean.")
    if (!(rlang::is_bool(args$byrow))) cli::cli_abort("{.arg byrow} must be boolean.")
    if (!(rlang::is_string(args$checked) || is.null(args$checked))) cli::cli_abort("{.arg checked} must be a string or NULL.")
    if (!(rlang::is_string(args$not_checked) || is.null(args$not_checked))) cli::cli_abort("{.arg not_checked} must be a string or NULL.")


    if (!ggplot2::is.ggplot(ggobj)) {
        if (is.list(ggobj) && length(ggobj) == 1 && ggplot2::is.ggplot(ggobj[[1]])) {
            ggobj <- ggobj[[1]]
        } else {
            cli::cli_abort("{.arg ggobj} must be either a ggplot2 object or list of ggplot2 objects.")
        }
    }
    if (length(ggobj$data) == 0) {
        return(cat())
    }
    fill_var <- rlang::as_label(ggobj$mapping$fill)

    checkbox <- FALSE
    if (!is.null(fill_var) && fill_var != "NULL") {
        fill_levels <-
            if (is.factor(ggobj$data[[fill_var]])) {
                levels(ggobj$data[[fill_var]])
            } else {
                unique(ggobj$data[[fill_var]])
            }


        if (all(fill_levels %in% c(args$checked, args$not_checked))) {
            checkbox <- TRUE
            ggobj <- convert_to_checkbox_plot(ggobj,
                checked = args$checked,
                not_checked = args$not_checked
            )
        }




        # lvls <- levels(ggobj$data[[fill_var]])
        ggobj <-
            suppressMessages(
                ggobj +
                    scale_discrete_special(
                        palette_codes = args$palette_codes,
                        lvls = fill_levels,
                        ncol = args$ncol,
                        byrow = args$byrow,
                        label_wrap_width = args$label_wrap_width,
                        priority_palette_codes = args$priority_palette_codes
                    ) +
                    ggplot2::guides(fill = if (isTRUE(checkbox)) {
                        "none"
                    } else {
                        ggplot2::guide_legend(
                            ncol =
                                guess_legend_ncols(
                                    ggobj = ggobj,
                                    char_limit = args$char_limit
                                )
                        )
                    }),
                classes = "rlang_message"
            )

        if (isTRUE(interactive)) {
            ggiraph::girafe(
                ggobj = ggobj, pointsize = args$pointsize,
                width_svg = args$width_svg, height_svg = args$height_svg,
                ...
            )
        } else {
            ggobj
        }
    } else {
        ggobj
    }
}
