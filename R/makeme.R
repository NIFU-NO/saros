#' Embed Interactive Plot of Various Kinds Using Tidyselect Syntax
#'
#' This function allows embedding of interactive or static plots based on various types of data using tidyselect syntax for variable selection.
#'
#' @param data *Your data.frame/tibble or srvyr-object (experimental)*
#'
#'   `data.frame` // *required*
#'
#'   The data to be used for plotting.
#'
#' @param dep,indep *Variable selections*
#'
#'   <`tidyselect`> // *Default:* `NULL`, meaning everything for dep, nothing for indep.
#'
#'   Columns in `data`. `dep` is compulsory.
#'
#' @param type *Kind of output*
#'
#'   `scalar<character>` // *default:* `"cat_plot_html"` (`optional`)
#'
#' For a list of registered types in your session, use `get_makeme_types()`.
#'
#' @param categories_treated_as_na *NA categories*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Categories that should be treated as NA.
#'
#'
#' @param data_label *Data label*
#'
#'   `scalar<character>` // *default:* `"proportion"` (`optional`)
#'
#'   One of "proportion", "percentage", "percentage_bare", "count", "mean", or "median".
#'
#' @param simplify_output
#'
#'   `scalar<logical>` // *default:* `TRUE`
#'
#'   If TRUE, a list output with a single output element will return the element
#'   itself, whereas list with multiple elements will return the list.
#'
#' @param mesos_var *Variable in `data` indicating groups to tailor reports for*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Column name in data indicating the groups for which mesos reports will be produced.
#'
#' @param mesos_group
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   String, target group.
#'
#' @param crowd *Which group(s) to display results for*
#'
#'   `vector<character>` // *default:* `c("target", "others", "all")` (`optional`)
#'
#'   Choose whether to produce results for target (mesos) group, others, all, or
#'   combinations of these.
#'
#' @param showNA *Show NA categories*
#'
#'   `vector<character>` // *default:* `c("ifany", "always", "never")` (`optional`)
#'
#'   Choose whether to show NA categories in the results.
#'
#' @param html_interactive *Toggle interactive plot*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Whether the plot is to be interactive (ggiraph) or static (ggplot2).
#'
#' @param hide_axis_text_if_single_variable *Hide y-axis text if just a single variable*
#'
#'   `scalar<boolean>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to hide text on the y-axis label if just a single variable.
#'
#' @param hide_for_all_crowds_if_hidden_for_crowd *Conditional hiding*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Select one of the `crowd` output groups. If selected, will hide a variable across
#'   all `crowd`-outputs if it for some reason is not displayed for
#'   `hide_for_all_if_hidden_for_crowd`. For instance, say:
#'
#'   `crowd = c("target", "others"), hide_variable_if_all_na = TRUE,`
#'   `hide_for_all_if_hidden_for_crowd = "target"`
#'
#'   will hide variables from both target and others-outputs if all are NA in
#'   the target-group.
#'
#' @param hide_for_crowd_if_all_na *Hide variable from output if containing all NA*
#'
#'   `scalar<boolean>` // *default:* `TRUE`
#'
#'   Whether to remove all variables (in particular useful for mesos) if all values are NA
#'
#' @param hide_for_crowd_if_valid_n_below *Hide variable if variable has < n observations*
#'
#'   `scalar<integer>` // *default:* `0`
#'
#'   Whether to hide a variable for a crowd if variable contains fewer than n observations (always ignoring NA).
#'
#' @param hide_for_crowd_if_category_k_below *Hide variable if < k categories*
#'
#'   `scalar<integer>` // *default:* `2`
#'
#'   Whether to hide a variable for a crowd if variable contains fewer than k used categories (always ignoring NA).
#'   Defaults to `2` because a unitary plot/table is rarely informative.
#'
#' @param hide_for_crowd_if_category_n_below *Hide variable if having a category with < n observations*
#'
#'   `scalar<integer>` // *default:* `0`
#'
#'   Whether to hide a variable for a crowd if variable contains a category with less than n observations (ignoring NA)
#'   Cells with a 0 count is not considered as these are usually not a problem for anonymity.
#'
#' @param hide_for_crowd_if_cell_n_below *Hide variable if having a cell with < n*
#'
#'   `scalar<integer>` // *default:* `0`
#'
#'   Whether to hide a variable for a crowd if the combination of dep-indep results in a cell with less than n observations (ignoring NA).
#'   Cells with a 0 count is not considered as these are usually not a problem for anonymity.
#'
#' @param hide_indep_cat_for_all_crowds_if_hidden_for_crowd *Conditionally hide independent categories*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   If `hide_for_all_crowds_if_hidden_for_crowd` is specified, should categories of the
#'   `indep` variable(s) be hidden for a crowd if it does not exist for the
#'   crowds specified in `hide_for_all_crowds_if_hidden_for_crowd`? This is useful when e.g.
#'   `indep` is academic disciplines, `mesos_var` is institutions, and a specific
#'   institution is not interested in seeing academic disciplines they do not offer themselves.
#'
#' @param label_separator *How to separate main question from sub-question*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Separator for main question from sub-question.
#'
#' @param require_common_categories *Check common categories*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Whether to check if all items share common categories.
#'
#' @param path *Output path for DOCX*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Path to save docx-output.
#'
#' @param inverse *Flag to swap x-axis and faceting*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   If TRUE, swaps x-axis and faceting.
#'
#' @param vertical *Display plot vertically*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   If TRUE, display plot vertically.
#'
#' @param labels_always_at_top,labels_always_at_bottom *Top/bottom variables*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Column names in `data` that should always be placed at the top or bottom of figures/tables.
#'
#' @param colour_palette *Colour palette*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Must contain at least the number of unique values (including missing) in the data set.
#'
#' @param colour_2nd_binary_cat *Colour for second binary category*
#'
#'   `scalar<character>` // *default:* `"#ffffff"` (`optional`)
#'
#'   Colour for the second category in binary variables. Often useful to hide this.
#'
#' @param colour_na *Colour for NA category*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Colour as a single string for NA values, if showNA is "ifany" or "always".
#'
#' @param x_axis_label_width,strip_width *Label width of x-axis and strip texts in plots*
#'
#'   `scalar<integer>` // *default:* `20` (`optional`)
#'
#'   Width of the labels used for the categorical column names in x-axis texts and strip texts.
#'
#' @param translations *Localize your output*
#'
#'   `list<character>`
#'
#'   A list of translations where the name is the code and the value is the translation. See the examples.
#'
#' @param add_n_to_dep_label,add_n_to_indep_label *Add N= to the variable label*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   For some plots and tables it is useful to attach the `"N="` to the end of the label of
#'   the dependent and/or independent variable.
#'   Whether it is `N` or `N_valid` depends on your `showNA`-setting. See also
#'   `translations$add_n_to_dep_label_prefix`,
#'   `translations$add_n_to_dep_label_suffix`,
#'   `translations$add_n_to_indep_label_prefix`,
#'   `translations$add_n_to_indep_label_suffix`.
#'
#' @param add_n_to_label *Add N= to the variable label of both dep and indep*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   For some plots and tables it is useful to attach the `"N="` to the end of the label.
#'   Whether it is `N` or `N_valid` depends on your `showNA`-setting. See also
#'   `translations$add_n_to_label_prefix` and
#'   `translations$add_n_to_label_suffix`.
#'
#' @param add_n_to_category *Add N= to the category*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   For some plots and tables it is useful to attach the `"N="` to the end of the category.
#'   This will likely produce a range across the variables, hence an infix (comma)
#'   between the minimum and maximum can be specified.
#'   Whether it is `N` or `N_valid` depends on your `showNA`-setting. See also
#'   `translations$add_n_to_category_prefix`,
#'   `translations$add_n_to_category_infix`, and
#'   `translations$add_n_to_category_suffix`.
#'
#'
#' @param totals *Include totals*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to include totals in the output.
#'
#' @param digits *Decimal places*
#'
#'   `scalar<integer>` // *default:* `0L` (`optional`)
#'
#'   Number of decimal places.
#'
#' @param data_label_decimal_symbol *Decimal symbol*
#'
#'   `scalar<character>` // *default:* `"."` (`optional`)
#'
#'   Decimal marker, some might prefer a comma ',' or something else entirely.
#'
#' @param hide_label_if_prop_below *Hide label threshold*
#'
#'   `scalar<numeric>` // *default:* `NULL` (`optional`)
#'
#'   Whether to hide label if below this value.
#'
#' @param sort_by *What to sort output by*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Sort output (and collapse if requested). When using `indep`-argument,
#'   sorting differs between ordered factors and unordered factors: Ordering
#'   of ordered factors is always respected in output. Unordered factors will be
#'   reordered by `sort_by`. Currently, this works best for a single `dep`.
#'
#' \describe{
#' \item{NULL}{No sorting.}
#' \item{".top"}{The proportion for the highest category available in the variable.}
#' \item{".upper"}{The sum of the proportions for the categories above the middle category.}
#' \item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
#' \item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
#' \item{".lower"}{The sum of the proportions for the categories below the middle category.}
#' \item{".bottom"}{The proportions for the lowest category available in the variable.}
#' \item{".variable_label"}{Sort by the variable labels.}
#' \item{".id"}{Sort by the variable names.}
#' \item{".by_group"}{The groups of the by argument.}
#' \item{character()}{Character vector of category labels to sum together.}
#' }
#'
#' @param descend *Sorting order*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Reverse sorting of `sort_by` in figures and tables. See `arrange_section_by`
#'   for sorting of report sections.
#'
#' @param table_main_question_as_header *Table main question as header*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to include the main question as a header in the table.
#'
#' @param table_wide *Pivot table wider*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to pivot table wider.
#'
#' @param n_categories_limit *Limit for cat_table_ wide format*
#'
#'   `scalar<integer>` // *default:* `12` (`optional`)
#'
#'   If there are more than this number of categories in the categorical variable,
#'   cat_table_* will have a long format instead of wide format.
#'
#' @param plot_height *DOCX-setting*
#'
#'   `scalar<numeric>` // *default:* `12` (`optional`)
#'
#'   DOCX plots need a height, which currently cannot be set easily with a Quarto chunk option.
#'
#' @param main_font_size,label_font_size,strip_font_size,legend_font_size *Font sizes*
#'
#'   `scalar<integer>` // *default:* `6` (`optional`)
#'
#'   ONLY FOR DOCX-OUTPUT. Other output is adjusted using e.g. ggplot2::theme() or set with a global theme (ggplot2::theme_set()).
#'   Font sizes for general text (6), data label text (3), strip text (6) and legend text (6).
#'
#' @param font_family *Font family*
#'
#'   `scalar<character>` // *default:* `"sans"` (`optional`)
#'
#'   Word font family. See officer::fp_text.
#'
#' @param docx_template *Filename or rdocx object*
#'
#'   `scalar<character>|<rdocx>-object` // *default:* `NULL` (`optional`)
#'
#'   Can be either a valid character path to a reference Word file, or an existing rdocx-object in memory.
#'
#'
#' @param ... *Dynamic dots*
#'
#'   <[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)>
#'
#'   Arguments forwarded to the corresponding functions that create the elements.
#'
#'
#'
#' @returns ggplot-object, optionally an extended ggplot object with ggiraph features.
#' @importFrom rlang !!!
#' @export
#'
#' @examples
#' makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = b_1, indep = x1_sex
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3, indep = c(x1_sex, x2_human),
#'   type = "sigtest_table_html"
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = b_1, indep = x1_sex,
#'   type = "cat_prop_plot_docx"
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = p_1:p_4, indep = x2_human,
#'   type = "cat_table_html"
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3,
#'   crowd = c("target", "others", "all"),
#'   mesos_var = "f_uni",
#'   mesos_group = "Uni of A"
#' )
makeme <-
  function(data,
           dep = tidyselect::everything(),
           indep = NULL,
           type = c(
             "cat_plot_html",
             "int_plot_html",
             "cat_table_html",
             "int_table_html",
             "sigtest_table_html",
             "cat_prop_plot_docx",
             "cat_freq_plot_docx",
             "int_plot_docx"
           ),
           ...,
           require_common_categories = TRUE,
           # Multiple output, splits and selective hiding of variables
           crowd = c("all"), # "target", "others",
           mesos_var = NULL,
           mesos_group = NULL,
           simplify_output = TRUE,
           # Hide variable (combinations) for a crowd if...
           hide_for_crowd_if_all_na = TRUE,
           hide_for_crowd_if_valid_n_below = 0,
           hide_for_crowd_if_category_k_below = 2,
           hide_for_crowd_if_category_n_below = 0,
           hide_for_crowd_if_cell_n_below = 0,
           hide_for_all_crowds_if_hidden_for_crowd = NULL,
           hide_indep_cat_for_all_crowds_if_hidden_for_crowd = FALSE,
           add_n_to_dep_label = FALSE,
           add_n_to_indep_label = FALSE,
           add_n_to_label = FALSE,
           add_n_to_category = FALSE,
           totals = FALSE,
           categories_treated_as_na = NULL,
           label_separator = " - ",
           showNA = c("ifany", "always", "never"),
           data_label = c("percentage_bare", "percentage", "proportion", "count"),
           html_interactive = TRUE,
           hide_axis_text_if_single_variable = TRUE,
           hide_label_if_prop_below = .01,
           inverse = FALSE,
           vertical = FALSE,
           digits = 0,
           data_label_decimal_symbol = ".",
           x_axis_label_width = 25,
           strip_width = 25,
           # Sorting
           sort_by = ".upper",
           descend = TRUE,
           labels_always_at_top = NULL,
           labels_always_at_bottom = NULL,
           # For tables
           table_wide = TRUE,
           table_main_question_as_header = FALSE,
           n_categories_limit = 12,
           translations =
             list(
               last_sep = " and ", # Not in use
               table_heading_N = "Total (N)",
               table_heading_data_label = "%",
               add_n_to_dep_label_prefix = " (N = ",
               add_n_to_dep_label_suffix = ")",
               add_n_to_indep_label_prefix = " (N = ",
               add_n_to_indep_label_suffix = ")",
               add_n_to_label_prefix = " (N = ",
               add_n_to_label_suffix = ")",
               add_n_to_category_prefix = " (N = [",
               add_n_to_category_infix = ",",
               add_n_to_category_suffix = "])",
               by_total = "Everyone",
               sigtest_variable_header_1 = "Var 1",
               sigtest_variable_header_2 = "Var 2",
               crowd_all = "All",
               crowd_target = "Target",
               crowd_others = "Others"
             ),
           # Only for docx, for ggplot2 it is set globally or wtih gplot2::theme()
           plot_height = 15,
           colour_palette = NULL,
           colour_2nd_binary_cat = "#ffffff",
           colour_na = "grey",
           label_font_size = 6,
           main_font_size = 6,
           strip_font_size = 6,
           legend_font_size = 6,
           font_family = "sans",
           path = NULL,
           docx_template = NULL) {
    ##


    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data)

    args <- check_options(
      call = match.call(),
      ignore_args = .saros.env$ignore_args,
      defaults_env = global_settings_get(fn_name = "makeme"),
      default_values = formals(makeme)
    )

    args$data <- data # reinsert after check_options
    args$dep <- names(dep_pos)
    args$indep <- names(indep_pos)
    args$showNA <- args$showNA[1]
    args$data_label <- args$data_label[1]
    args$type <- eval(args$type)[1]

    validate_makeme_options(params = args)


    if (!args$type %in% c("sigtest_table_html")) {
      check_multiple_indep(data, indep = {{ indep }})
      check_category_pairs(data = data, cols_pos = c(dep_pos))
    }




    # if(grepl(x=args$type, pattern = "freq")) args$data_label <- "count"

    # Set hide_for_all_crowds_if_hidden_for_crowd first to get its excluded variables early
    # This only happens if hide_for_all_crowds_if_hidden_for_crowd are in the set of crowd.
    args$crowd <- c(
      args$hide_for_all_crowds_if_hidden_for_crowd[args$hide_for_all_crowds_if_hidden_for_crowd %in% args$crowd],
      args$crowd[!args$crowd %in% args$hide_for_all_crowds_if_hidden_for_crowd[args$hide_for_all_crowds_if_hidden_for_crowd %in% args$crowd]]
    )


    kept_cols_list <- rlang::set_names(vector(mode = "list", length = length(args$crowd)), args$crowd)
    omitted_cols_list <- rlang::set_names(vector(mode = "list", length = length(args$crowd)), args$crowd)
    kept_indep_cats_list <- rlang::set_names(vector(mode = "list", length = length(args$crowd)), args$crowd)

    for (crwd in names(kept_cols_list)) {
      kept_cols_tmp <-
        keep_cols(
          data = args$data,
          dep = args$dep,
          indep = args$indep,
          crowd = crwd,
          mesos_var = args$mesos_var,
          mesos_group = args$mesos_group,
          hide_for_crowd_if_all_na = args$hide_for_crowd_if_all_na, # 1
          hide_for_crowd_if_valid_n_below = args$hide_for_crowd_if_valid_n_below, # 2
          hide_for_crowd_if_category_k_below = args$hide_for_crowd_if_category_k_below, # 3
          hide_for_crowd_if_category_n_below = args$hide_for_crowd_if_category_n_below, # 4
          hide_for_crowd_if_cell_n_below = args$hide_for_crowd_if_cell_n_below # , # 5
          # hide_for_all_crowds_if_hidden_for_crowd_vars = omitted_vars
        )
      omitted_cols_list[[crwd]] <- kept_cols_tmp[["omitted_vars"]]

      kept_indep_cats_list[[crwd]] <-
        keep_indep_cats(
          data = kept_cols_tmp[["data"]],
          indep = args$indep
        )
    }



    kept_indep_cats_list <-
      lapply(rlang::set_names(names(kept_indep_cats_list)), function(crwd) {
        lapply(rlang::set_names(names(kept_indep_cats_list[[crwd]])), function(x) {
          if (is.character(args$hide_for_all_crowds_if_hidden_for_crowd) &&
            !crwd %in% args$hide_for_all_crowds_if_hidden_for_crowd) {
            kept_globally <-
              kept_indep_cats_list[args$hide_for_all_crowds_if_hidden_for_crowd] |>
              unlist() |>
              unique()

            kept_indep_cats_list[[crwd]][[x]][
              kept_indep_cats_list[[crwd]][[x]] %in%
                kept_globally
            ]
          } else {
            kept_indep_cats_list[[crwd]][[x]]
          }
        })
      })

    out <- rlang::set_names(vector(mode = "list", length = length(args$crowd)), args$crowd)




    for (crwd in names(out)) {
      #
      omitted_vars_crwd <-
        omitted_cols_list[
          c(
            crwd,
            args$hide_for_all_crowds_if_hidden_for_crowd
          )
        ] |>
        lapply(FUN = function(x) if ("omitted_vars" %in% names(x)) x["omitted_vars"]) |>
        unlist() |>
        unique()


      dep_crwd <- args$dep[!args$dep %in% omitted_vars_crwd]
      if (length(dep_crwd) == 0) next

      indep_crwd <- args$indep
      if (length(indep_crwd) == 0) indep_crwd <- NULL



      subset_data <-
        dplyr::filter(
          args$data[, # subetting would remove variable labels, filter keeps them
            !colnames(args$data) %in% omitted_vars_crwd,
            drop = FALSE
          ],
          makeme_keep_rows(
            data = data,
            crwd = crwd,
            mesos_var = mesos_var,
            mesos_group = mesos_group
          )
        )

      if (isTRUE(args$hide_indep_cat_for_all_crowds_if_hidden_for_crowd)) {
        for (x in indep_crwd) {
          subset_data <-
            dplyr::filter(subset_data, as.character(subset_data[[x]]) %in%
              kept_indep_cats_list[[crwd]][[x]])
        }
      }

      if (nrow(subset_data) == 0) {
        indep_msg <- if (is.character(args$indep)) paste0("indep=", cli::ansi_collapse(args$indep))
        cli::cli_warn(c("No data left to make you {.arg {args$type}} with dep={.arg {args$dep}}, {.arg {indep_msg}}, crowd={.arg {crwd}}.",
          i = "Skipping."
        ))
        next
      }

      variable_type_dep <-
        lapply(args$dep, function(v) class(subset_data[[v]])) |>
        unlist()

      # Future: switch or S3
      if (all(variable_type_dep %in% c("integer", "numeric"))) {
        args$data_summary <-
          rlang::exec(summarize_int_cat_data, !!!args)
      } else if (all(variable_type_dep %in% c("factor", "ordered", "character"))) {
        args$data_summary <-
          summarize_cat_cat_data(
            data = subset_data,
            dep = dep_crwd,
            indep = indep_crwd,
            ...,
            label_separator = args$label_separator,
            showNA = args$showNA,
            totals = args$totals,
            sort_by = args$sort_by,
            descend = args$descend,
            data_label = args$data_label,
            digits = args$digits,
            add_n_to_dep_label = args$add_n_to_dep_label,
            add_n_to_indep_label = args$add_n_to_indep_label,
            add_n_to_label = args$add_n_to_label,
            add_n_to_category = args$add_n_to_category,
            hide_label_if_prop_below = args$hide_label_if_prop_below,
            data_label_decimal_symbol = args$data_label_decimal_symbol,
            categories_treated_as_na = args$categories_treated_as_na,
            labels_always_at_bottom = args$labels_always_at_bottom,
            labels_always_at_top = args$labels_always_at_top,
            translations = args$translations
          )
      }

      args$main_question <-
        as.character(unique(args$data_summary[[".variable_label_prefix"]]))

      if (!args$type %in% c("sigtest_table_html")) {
        args$data_summary <-
          post_process_makeme_data(
            data = args$data_summary,
            indep = indep_crwd,
            showNA = args$showNA,
            colour_2nd_binary_cat = if (grepl(x = args$type, pattern = "docx")) args$colour_2nd_binary_cat
          )
      }

      args_crwd <- args
      args_crwd$dep <- dep_crwd
      args_crwd$indep <- indep_crwd

      out[[crwd]] <-
        suppressPackageStartupMessages(
          rlang::exec(make_content,
            type = args_crwd$type,
            !!!args_crwd[!names(args_crwd) %in% c("type")]
          )
        )
    }

    for (crwd in names(out)) {
      if (rlang::is_string(args$translations[[paste0("crowd_", crwd)]])) {
        names(out)[names(out) == crwd] <- args$translations[[paste0("crowd_", crwd)]]
      }
    }
    out <- out[lapply(out, function(x) !is.null(x)) |> unlist()]

    if (isTRUE(args$simplify_output) && length(out) == 1) {
      out[[1]]
    } else {
      out
    }
  }
