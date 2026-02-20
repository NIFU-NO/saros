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
#'   `scalar<character>` // *default:* `"auto"` (`optional`)
#'
#'   The type of output to generate. Use `"auto"` (default) to automatically
#'   detect the appropriate type based on your dependent variables:
#'   - Numeric/integer variables → `"int_plot_html"`
#'   - Factor/character variables → `"cat_plot_html"`
#'
#'   For a list of all registered types in your session, use `get_makeme_types()`.
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
#' @param data_label_position *Data label position*
#'
#'   `scalar<character>` // *default:* `"center"` (`optional`)
#'
#'   Position of data labels on bars. One of "center" (middle of bar), "bottom" (bottom but inside bar),
#'   "top" (top but inside bar), or "above" (above bar outside).
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
#' @param error_on_duplicates *Error or warn on duplicate labels*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Whether to abort (`TRUE`) or warn (`FALSE`) if the same label (suffix) is
#'   used across multiple variables.
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
#' @param sort_dep_by *What to sort dependent variables by*
#'
#'   `vector<character>` // *default:* `".variable_position"` (`optional`)
#'
#'   Sort dependent variables in output. When using `indep`-argument,
#'   sorting differs between ordered factors and unordered factors: Ordering
#'   of ordered factors is always respected in output (their levels define
#'   the base order). Unordered factors will be reordered by `sort_dep_by`.
#'
#' \describe{
#' \item{NULL or ".variable_position"}{Sort by variable position in the supplied data frame (default).}
#' \item{".variable_label"}{Sort by the variable labels.}
#' \item{".variable_name"}{Sort by the variable names.}
#' \item{".top"}{The proportion for the highest category available in the variable.}
#' \item{".upper"}{The sum of the proportions for the categories above the middle category.}
#' \item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
#' \item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
#' \item{".lower"}{The sum of the proportions for the categories below the middle category.}
#' \item{".bottom"}{The proportions for the lowest category available in the variable.}
#' }
#'
#' @param sort_indep_by *What to sort independent variable categories by*
#'
#'   `vector<character>` // *default:* `".factor_order"` (`optional`)
#'
#'   Sort independent variable categories in output. When `".factor_order"`,
#'   preserves the original factor level order for the independent variable.
#'   Passing `NULL` is accepted and treated as `".factor_order"`.
#'
#' \describe{
#' \item{NULL}{No sorting - preserves original factor level order (default).}
#' \item{".top"}{The proportion for the highest category available.}
#' \item{".upper"}{The sum of the proportions for the categories above the middle category.}
#' \item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
#' \item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
#' \item{".lower"}{The sum of the proportions for the categories below the middle category.}
#' \item{".bottom"}{The proportions for the lowest category available.}
#' \item{character()}{Character vector of category labels to sum together.}
#' }
#'
#' @param sort_by *What to sort output by (legacy)*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   **DEPRECATED:** Use `sort_dep_by` and `sort_indep_by` instead for clearer control.
#'   When specified, this parameter will be used for both dependent and independent sorting.
#'   If `NULL` (default), dependent variables will be sorted by `.variable_position`.
#'
#' \describe{
#' \item{NULL}{Uses `.variable_position` for dependent variables, no sorting for independent.}
#' \item{".top"}{The proportion for the highest category available in the variable.}
#' \item{".upper"}{The sum of the proportions for the categories above the middle category.}
#' \item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
#' \item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
#' \item{".lower"}{The sum of the proportions for the categories below the middle category.}
#' \item{".bottom"}{The proportions for the lowest category available in the variable.}
#' \item{".variable_label"}{Sort by the variable labels.}
#' \item{".variable_name"}{Sort by the variable names.}
#' \item{".variable_position"}{Sort by the variable position in the supplied data frame.}
#' \item{".by_group"}{The groups of the by argument.}
#' \item{character()}{Character vector of category labels to sum together.}
#' }
#'
#' @param descend *Sorting order*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Reverse sorting of `sort_by` in figures and tables. Works with both
#'   ordered and unordered factors - for ordered factors, it reverses the
#'   display order while preserving the inherent level ordering.
#'   See `arrange_section_by` for sorting of report sections.
#'
#' @param descend_indep *Sorting order for independent variables*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Reverse sorting of `sort_indep_by` in figures and tables. Works with both
#'   ordered and unordered factors - for ordered factors, it reverses the
#'   display order while preserving the inherent level ordering.
#'   See `arrange_section_by` for sorting of report sections.
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
#'   ONLY FOR DOCX-OUTPUT. Other output is adjusted using e.g. ggplot2::theme() or set with a global theme (ggplot2::set_theme()).
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
#' @param docx_return_as_mschart *Return mschart object instead of rdocx*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   For `cat_plot_docx` type: if TRUE, return the mschart object instead of embedding it in an rdocx document.
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
#'   dep = b_1:b_2
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3, indep = c(x1_sex, x2_human),
#'   type = "sigtest_table_html"
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = p_1:p_4, indep = x2_human,
#'   type = "cat_table_html"
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = c_1:c_2, indep = x1_sex,
#'   type = "int_table_html"
#' )
#' makeme(
#'   data = ex_survey,
#'   dep = b_1:b_2,
#'   crowd = c("target", "others"),
#'   mesos_var = "f_uni",
#'   mesos_group = "Uni of A"
#' )
makeme <-
  function(
    data,
    dep = tidyselect::everything(),
    indep = NULL,
    type = c(
      "auto",
      "cat_plot_html",
      "int_plot_html",
      "cat_table_html",
      "int_table_html",
      "sigtest_table_html",
      "cat_plot_docx",
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
    error_on_duplicates = TRUE,
    showNA = c("ifany", "always", "never"),
    data_label = c(
      "percentage_bare",
      "percentage",
      "proportion",
      "count",
      "mean",
      "median"
    ),
    data_label_position = c("center", "bottom", "top", "above"),
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
    sort_dep_by = ".variable_position",
    sort_indep_by = ".factor_order",
    sort_by = NULL,
    descend = TRUE,
    descend_indep = FALSE,
    labels_always_at_top = NULL,
    labels_always_at_bottom = NULL,
    # For tables
    table_wide = TRUE,
    table_main_question_as_header = FALSE,
    n_categories_limit = 12,
    translations = list(
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
    label_font_size = 9,
    main_font_size = 9,
    strip_font_size = 6,
    legend_font_size = 6,
    font_family = "sans",
    path = NULL,
    docx_template = NULL,
    docx_return_as_mschart = FALSE
  ) {
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

    # Handle sort_by parameter logic
    if (!is.null(args$sort_by)) {
      # If sort_by is specified and the new parameters are defaults, use sort_by for both
      if (
        identical(args$sort_dep_by, ".variable_position") &&
          (is.null(args$sort_indep_by) ||
            identical(args$sort_indep_by, ".factor_order"))
      ) {
        args$sort_dep_by <- args$sort_by
        args$sort_indep_by <- args$sort_by
      }
      # Issue deprecation warning
      cli::cli_warn(
        "The 'sort_by' parameter is deprecated. Use 'sort_dep_by' and 'sort_indep_by' instead for clearer control.",
        call. = FALSE
      )
    }

    # Convert NULL to .variable_position for sort_dep_by
    if (is.null(args$sort_dep_by)) {
      args$sort_dep_by <- ".variable_position"
    }

    # Convert NULL to .factor_order for sort_indep_by (explicitly allowed)
    if (is.null(args$sort_indep_by)) {
      args$sort_indep_by <- ".factor_order"
    }

    # Setup and validate arguments
    args <- setup_and_validate_makeme_args(
      args,
      data,
      dep_pos,
      indep_pos,
      {{ indep }}
    )

    # Initialize crowd-based filtering
    crowd_filtering <- initialize_crowd_filtering(args$crowd, args)
    kept_cols_list <- crowd_filtering$kept_cols_list
    omitted_cols_list <- crowd_filtering$omitted_cols_list
    kept_indep_cats_list <- crowd_filtering$kept_indep_cats_list

    # Process global independent category hiding logic
    kept_indep_cats_list <- process_global_indep_categories(
      kept_indep_cats_list,
      args$hide_for_all_crowds_if_hidden_for_crowd
    )

    # Process all crowds and generate final output
    out <- process_all_crowds(
      args,
      omitted_cols_list,
      kept_indep_cats_list,
      data,
      mesos_var,
      mesos_group,
      ...
    )
    process_output_results(out, args)
  }
