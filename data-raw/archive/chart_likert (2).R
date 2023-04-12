#' Helper Function to Prepare Data for create_chart_likert
#'
#' @param data Dataset
#' @param cols Columns to select for reporting. Supports \code{\link[dplyr:dplyr_tidy_select]{tidy-select}}.
#' @param by \code{\link[dplyr:dplyr_data_masking]{data-masking}}\cr. Optional column used to break graph by.
#' @param showNA [\code{logical(1)}]\cr Whether to show NA in categorical variables (one of c("ifany", "always", "no"), like in table()).
#' @param digits [\code{integer(1)}]\cr Number of decimal places as integer.
#' @param percent_sign [\code{logical(1)}]\cr Whether to include percentage symbol on chart.
#' @param sort_by [\code{character(1)}], sort output by "label", size of a category, or sum of categories (character vector). Defaults to none (NULL).
#' @param desc [\code{logical(1)}]\cr Reverse sorting of sort_by. Defaults to ascending order (FALSE).
#' @param call Error call function, usually not needed.
#' @param hide_label_if_below [\code{numeric(1)}] Whether to hide label if below this value.
#' @param label_separator [\code{character(1)}]\cr Split pattern.
#' @param what [\code{character(1)}]\cr Whether to compute "percentage" (default) or "frequency". Supports partial matching.
#'
#' @importFrom rlang arg_match caller_env is_integerish .env
#' @importFrom dplyr mutate group_by arrange ungroup if_else desc n_distinct
#' @importFrom cli cli_abort
#' @importFrom stringr str_c
#' @importFrom stats ave
#'
#' @return Dataset
prepare_mschart_data <-
  function(data,
           cols = tidyselect::everything(),
           by = NULL,
           what = "percent",
           showNA = "ifany",
           call = rlang::caller_env(),
           digits = 1,
           percent_sign = TRUE,
           sort_by = NULL,
           desc = FALSE,
           hide_label_if_below = 1,
           label_separator = NULL) {
    rlang::arg_match(showNA, values = c("ifany", "always", "no"), multiple = FALSE, error_call = call)
    if(!rlang::is_integerish(digits)) cli::cli_abort("{.arg digits} must be {.cls {integer(1)}}.", call = call)
    fmt <- stringr::str_c("%.", digits, "f", if(percent_sign) "%%")



    if(ncol(dplyr::select(.data = data, {{by}})) == 1L &&
       ncol(dplyr::select(.data = data, {{cols}})) == 1L) {
      data <- crosstable_list(data, col = {{cols}}, by = {{by}},
                              showNA = showNA)
    } else if(ncol(dplyr::select(.data = data, {{by}})) == 0L &&
              ncol(dplyr::select(.data = data, {{cols}})) >= 1L) {
      data <- dplyr::select(data, {{cols}})
      data <- crosstable::crosstable(data = data,
                                     percent_pattern = "{n}",
                                     showNA = showNA,
                                     label = TRUE)
    } else if(ncol(dplyr::select(.data = data, {{by}})) > 1L) {
      cli::cli_abort(c("Too many columns provided for {.arg by}.",
                       i="Only 1 by-column is currently allowed."))
    } else if(ncol(dplyr::select(.data = data, {{by}})) == 1L &&
              ncol(dplyr::select(.data = data, {{cols}})) > 1L) {
      cli::cli_abort(c("Multiple columns for {.arg cols} and {.arg by} are not allowed."))

    }

    data <- dplyr::mutate(data,
                          value = as.integer(.data$value))

    if(grepl("per", what)) {
      data <- dplyr::group_by(.data = data, .data$label)
      data <- dplyr::mutate(data,
                            data_label = .data$value/sum(.data$value, na.rm=TRUE)*100,
                            data_label = sprintf(fmt = fmt, .data$data_label))
      data <- dplyr::ungroup(x = data)
    } else if(grepl("fre", what)) {
      data <- dplyr::mutate(data,
                            data_label = as.character(.data$value))
    } else cli::cli_abort("Invalid {.arg what}. Must be either {.var percent} or {.var frequency}.")

    data <- dplyr::group_by(.data = data, .data$label)
    data <- dplyr::mutate(.data = data,
                          n_unique = dplyr::n_distinct(.data$variable, na.rm=FALSE))
    data <- dplyr::ungroup(x = data)

    fct_max <- max(data$n_unique, na.rm = TRUE)
    fct_uniques <- dplyr::filter(.data = data, .data$n_unique == .env$fct_max)
    fct_uniques <- unique(dplyr::pull(.data = data, .data$variable))
    data <- dplyr::mutate(data,
                          n_unique = NULL,
                          variable = factor(x = .data$variable,
                                            levels = .env$fct_uniques),
                          cat_id = .data$variable %in% .env$sort_by)
    if(!is.null(sort_by)) {

      if(all(sort_by %in% names(data))) {
        sort_col <- sort_by
      } else if(all(sort_by %in% unique(data$variable))) {
        sort_col <- "sum_value"
      }
      data <- dplyr::group_by(data, .data$label, .data$cat_id)
      data <- dplyr::mutate(data,
                            sum_value =
                              dplyr::if_else(.data$cat_id,
                                             sum(as.numeric(.data$value), na.rm=TRUE),
                                             NA_real_))
      data <- dplyr::ungroup(x = data)
      data <-
        dplyr::arrange(data,
                       dplyr::desc(.data$cat_id),
                       if(desc) dplyr::desc(.data[[sort_col]]) else .data[[sort_col]])
    }
    lvls <- unique(as.character(data$label))
    lbls <-
      if(!is.null(label_separator)) stringr::str_replace(
        string = lvls,
        pattern = paste0("^(.*)", label_separator, "(.*)$"),
        replacement = "\\2") else lvls
    data <- dplyr::mutate(data,
                          label = factor(x = .data$label,
                                         levels = .env$lvls,
                                         labels = .env$lbls,
                                         ordered = TRUE),
                          data_label =
                            dplyr::if_else(as.numeric(.data$value) <
                                             as.numeric(.env$hide_label_if_below),
                                           "",
                                           as.character(.data$data_label)))
    data <- dplyr::arrange(data, as.integer(.data$label), .data$variable)
    data
  }



#' Create Likert Chart from Descriptives Data Frame
#'
#' @param data Data frame or tibble.
#' @param y [\code{character(1)}]\cr Name of column in data for .
#' @param x [\code{character(1)}]\cr Name of column in data for
#' @param group [\code{character(1)}]\cr Name of column in data for
#' @param labels [\code{character(1)}]\cr Name of column in data for labels.
#' @param label_font_size [\code{integer(1)}]\cr Font size for data labels
#' @param main_font_size [\code{integer(1)}]\cr Font size for all other text
#' Must contain at least the number of unique values (excl. missing) in the data set.
#' @param colour_palette [\code{character()}]\cr
#' Must contain at least the number of unique values (incl. missing) in the data set.
#' @param colour_na [\code{character(1)}]\cr Colour as a single string.
#' @param colour_2nd_binary_cat [\code{character(1)}]\cr Colour for second category in binary variables. Often useful to hide this.
#' @param font_family Word font family. See officer::fp_text
#' @param vertical Logical. If FALSE (default), then horizontal.
#' @param what [\code{character(1)}] Either "percent" or "frequency". Supports partial matching.
#' @param seed Optional random seed for selection of colours in blender.
#'
#' @importFrom crosstable crosstable
#'
#' @return mschart-object. Can be added to an rdocx, rpptx or rxlsx object.
#'
#' @examples
#' #create_chart_likert(prepare_data_for_mschart(ex_survey1[paste0("b_", 1:3)]))
create_chart_likert <-
  function(data,
           y="value", x="label", group="variable", labels="data_label",
           label_font_size = 10,
           main_font_size = 8,
           font_family = "Calibri",
           colour_palette = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = "#ffffff",
           vertical = FALSE,
           what = "percent",
           seed = 1) {

    coll <- checkmate::makeAssertCollection()
    checkmate::assert_string(y, add = coll)
    checkmate::assert_string(x, add = coll)
    checkmate::assert_string(group, add = coll)
    checkmate::assert_string(labels, add = coll)
    checkmate::assert_character(colour_palette, null.ok = TRUE, add = coll)
    checkmate::assert_string(colour_na, na.ok = TRUE, null.ok = TRUE, add = coll)
    checkmate::assert_number(label_font_size, lower = 0, upper = 72, finite = TRUE, add = coll)
    checkmate::assert_number(main_font_size, lower = 0, upper = 72, finite = TRUE, add = coll)
    checkmate::assert_number(seed, lower = 1, finite = TRUE, add = coll)
    checkmate::assert_data_frame(data, add = coll)
    checkmate::assert_logical(vertical, len = 1, any.missing = FALSE, add = coll)
    checkmate::assert_subset(x = colnames(data), choices = c(y,x,group,labels, ".id", "cat_id", "sum_value"), add = coll)
    if(!is.null(colour_palette) & !all(is_colour(colour_palette))) {
      cli::cli_abort(
        c("Invalid user-specified colours.",
          i="{.arg colour_palette} must be a character vector of valid colours in hex-format (e.g. #000000)."))
    }

    checkmate::reportAssertions(coll)

    colour_palette <-
      get_colour_set(n_colours_needed = length(levels(data[[group]])),
                     user_colour_set = colour_palette,
                     seed = seed, call=rlang::caller_env())

    colour_palette <-
      rlang::set_names(colour_palette, nm=levels(data[[group]]))

    if(!is.null(colour_na) && !is.na(colour_na)) {
      colour_palette[names(colour_palette)=="NA"] <- colour_na
    }

    if(length(levels(data[[group]]))==2L &&
       !is.null(colour_2nd_binary_cat) &&
       is_colour(colour_2nd_binary_cat)) {
      colour_palette[2] <- colour_2nd_binary_cat
    }



    fp_text_settings <-
      lapply(colour_palette,
             function(color) {
               officer::fp_text(font.size = label_font_size,
                                color = hex_bw(color),
                                font.family = font_family)
             })
    fp_text_settings <- fp_text_settings[seq_len(dplyr::n_distinct(data[[group]]))]

    blank_border <- officer::fp_border(style = "none")

    main_text <- officer::fp_text(font.size = main_font_size, font.family = font_family)

    m <- mschart::ms_barchart(data = data, y = y, x = x,
                              group = group, labels = labels)

    if(grepl("per", what)) {
      m <- mschart::as_bar_stack(x = m, percent = TRUE)
    }
    if(grepl("fre", what)) { # Silly way due to poor programming in mschart
      m <- mschart::chart_settings(x = m, dir = if(vertical) "vertical" else "horizontal",
                                   overlap = -40)
    } else {
      m <- mschart::chart_settings(x = m, dir = if(vertical) "vertical" else "horizontal")
    }
    m <- mschart::chart_data_fill(x = m, values = colour_palette)
    m <- mschart::chart_data_stroke(x = m, values = colour_palette)
    m <- mschart::chart_labels_text(x = m, values = fp_text_settings)
    m <- mschart::chart_labels(x = m, ylab = NULL, xlab = NULL, title = NULL)
    m <- mschart::chart_ax_x(x = m, major_tick_mark = "none")
    if(grepl("per", what)) m <- mschart::chart_ax_y(x = m, num_fmt = "0%%")
    m <- mschart::chart_theme(x = m,
                              legend_text = main_text,
                              axis_text_x = main_text,
                              axis_text_y = main_text,
                              grid_major_line_x = blank_border,
                              grid_major_line_y = blank_border,
                              grid_minor_line_x = blank_border,
                              grid_minor_line_y = blank_border)
    m
  }




#' Create Word Report with Univariates for Categorical Columns Sharing Same Categories
#'
#' @param data Data frame or tibble.
#' @param cols Columns to select for reporting. Supports \code{\link[dplyr:dplyr_tidy_select]{tidy-select}}.
#' @param by \code{\link[dplyr:dplyr_data_masking]{data-masking}}\cr. Optional column used to break graph by.
#' @param showNA [\code{logical(1)>0}]\cr Whether to show NA in categorical variables (one of c("ifany", "always", "no"), like in table()).
#' @param docx_template  [\code{character(1) || officer::read_docx()}]\cr
#' Either a filepath to a template file, or a rdocx-object from \link[officer]{read_docx}.
#' @param label_font_size [\code{integer(1)}]\cr Font size for data labels
#' @param main_font_size [\code{integer(1)}]\cr Font size for all other text
#' @param font_family [\code{character(1)}]\cr Office font family. Defaults to "Arial". See ?officer::fp_text() for options.
#' @param colour_palette [\code{character()}]\cr
#' Must contain at least the number of unique values (incl. missing) in the data set.
#' @param colour_na [\code{character(1)}]\cr Colour as a single string, for NA-values.
#' @param colour_2nd_binary_cat [\code{character(1)}]\cr Colour for second category in binary variables. Often useful to hide this.
#' @param chart_formatting [\code{character(1)}]\cr Template style to be used for formatting chart
#' @param height_per_col [\code{numeric(1)>0}]\cr Height in cm per chart entry.
#' @param height_fixed [\code{numeric(1)>0}]\cr Fixed height in cm.
#' @param what [\code{character(1)}] Either "percent" or "frequency". Supports partial matching.
#' @param digits [\code{integer(1)}]\cr Number of decimal places as integer.
#' @param percent_sign [\code{logical(1)>0}]\cr Whether to include percentage symbol on chart.
#' @param sort_by [\code{character(1)}], sort output by "label", size of a category, or sum of categories (character vector). Defaults to none (NULL).
#' @param desc [\code{logical(1)>0}]\cr Sort in descending order?
#' @param vertical [\code{logical(1)>0}]\cr If FALSE (default), then horizontal.
#' @param hide_label_if_below [\code{numeric(1)}]\cr Whether to hide label if below this value.
#' @param seed [\code{numeric(1)>0}]\cr Optional random seed for selection of colours in blender.
#' @param label_separator [\code{character(1)}]\cr If not NULL (default), will split labels into main- and sub-questions and create figure caption.
#' @param caption_style [\code{character(1)}]\cr Template style to be used for formatting chart. Defaults to "Normal".
#' @param caption_autonum Object obtained from \link[officer]{run_autonum}.
#'
#' @importFrom tidyselect everything eval_select
#' @importFrom officer read_docx docx_dim block_caption body_add_caption
#' @importFrom mschart body_add_chart
#' @importFrom rlang enquo is_bare_character
#' @importFrom cli cli_abort
#' @importFrom stringr str_c str_replace
#' @importFrom stats ave
#' @importFrom purrr map_chr
#' @return rdocx object, which can be saved with print() after loading the officer-package
#' @export
#'
#' @examples
#' library(dplyr) # For piping
#' library(officer) # To save the rdocx object to disk
#' ex_survey1 %>%
#'   report_chart_likert(cols = a_1:a_9) #%>%
#'  # print(target = "test_docx_a19.docx")
#' #file.remove("test_docx_a19.docx")
#'
#'
#'   docx_template <-
#'       system.file("template","NIFUmal_tom.docx",
#'                    package = "surveyreport", mustWork = TRUE)
#'   colour_palette <-
#'     readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx",
#'                                    package = "surveyreport", mustWork = TRUE),
#'                        sheet = "NIFUblue") %>%
#'     dplyr::pull(hex)
#' chart_format <-
#'  system.file("template", "NIFUmal_stiler.xlsx",
#'              package = "surveyreport", mustWork = TRUE) %>%
#'  readxl::read_excel(., sheet = 1) %>%
#'  dplyr::filter(surveyreport_style == "figure") %>%
#'  dplyr::pull(template_style)
#'
#'  test_docx_b13 <-
#'    ex_survey1 %>%
#'    report_chart_likert(cols = b_1:b_3,
#'                        docx_template = docx_template,
#'                        colour_palette = colour_palette,
#'                        chart_formatting = chart_format,
#'                        height_per_col = .3,
#'                        height_fixed = 1)
#' #print(test_docx_b13, target = "test_docx_b13.docx")
#' #file.remove("test_docx_b13.docx")

report_chart_likert <-
  function(data,
           cols = everything(),
           by = NULL,
           showNA = "ifany",
           docx_template = NULL,
           label_font_size = 8,
           main_font_size = 9,
           font_family = "Calibri",
           colour_palette = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = "#ffffff",
           height_per_col = .3,
           height_fixed = 1,
           chart_formatting = NULL,
           caption_style = NULL,
           what = "percent",
           digits = 1,
           percent_sign = TRUE,
           sort_by = NULL,
           vertical = FALSE,
           desc = FALSE,
           hide_label_if_below = 1,
           label_separator = NULL,
           caption_autonum = NULL,
           seed = 1) {

    if(!inherits(data, what = "data.frame")) {
      cli::cli_abort("data must be a {.cls data.frame} or {.cls tibble}.")
    }
    data <- dplyr::select(data, {{cols}}, {{by}})


    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data)

    check_category_pairs(data = data, cols_pos = c(cols_pos))


    data_out <-
      prepare_mschart_data(data = data,
                           cols = {{cols}},
                           by = {{by}},
                           what = what,
                           showNA = showNA,
                           sort_by = sort_by,
                           desc = desc,
                           hide_label_if_below = hide_label_if_below,
                           label_separator = label_separator,
                           percent_sign = percent_sign,
                           digits = digits)


    chart <-
      create_chart_likert(data = data_out,
                          label_font_size = label_font_size,
                          main_font_size = main_font_size,
                          colour_palette = colour_palette,
                          colour_na = colour_na,
                          colour_2nd_binary_cat = colour_2nd_binary_cat,
                          font_family = font_family,
                          vertical = vertical,
                          what = what,
                          seed = seed)

    docx_file <- use_docx(docx_template = docx_template)


    if(!is.null(label_separator) &&
       rlang::is_bare_character(x = label_separator, n = 1)) {


      raw_labels <-
        purrr::map_chr(cols_pos, ~{
          y<-attr(data[[.x]], "label")
          if(!is.null(y)) y else NA
        })


      main_question <-
        get_main_question2(x = raw_labels,
                          label_separator = label_separator)

      caption_style <-
        if(!is.null(caption_style)) caption_style else "Normal"

      block_caption <-
        officer::block_caption(label = main_question,
                               style = caption_style,
                               autonum = caption_autonum)

      officer::body_add_caption(x = docx_file, value = block_caption)
    }


    docx_dims <- get_docx_dims(docx_file)
    determine_height <-
      min(c(height_fixed + height_per_col*length(cols_pos),
            docx_dims[["h"]]))

    mschart::body_add_chart(x = docx_file,
                            chart = chart,
                            style = chart_formatting,
                            pos = "after",
                            width = docx_dims[["w"]],
                            height = determine_height)

    docx_file
  }

