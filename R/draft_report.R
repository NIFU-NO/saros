#' Automatically Draft a Quarto Report
#'
#' @description
#' The `draft_report()` function is the main function, and the only necessary
#' user interface, to create semi-automated (draft) reports. It does not need to
#' be the first step, however, as one might want to store and read in arguments
#' for the function with the `read_yaml_params()`-function first. After the
#' report files has been drafted with `draft_report()`, you can edit, render, and
#' ultimately publish these as usual with Quarto features in RStudio.
#' The index.qmd will be the main output file containing "includes" to other
#' chapters.
#'
#' @details
#' This function requires at a minimum a dataset (data frame and tibbles are
#' supported so far). Note that saros treats data as they are stored: numeric,
#' integer, factor, ordinal, character, and datetime. Currently, only
#' factor/ordinal and character are implemented. Second, the chapter_overview
#' must be specified, also as a (small) data frame, with at least the character
#' columns 'chapter' and 'dep', where the first names the output chapters, and
#' the 'dep'-column contain comma-separated (alternatively using tidyselect-syntax)
#' columns in the `data` which are to be treated as dependent variables.
#' See `chapter_overview` for more options.
#'
#' @param data *Survey data*
#'
#'   `obj:<data.frame>|obj:<tbl_df>` // Required
#'
#'   A data frame (or a srvyr-object) with the columns specified in the
#'   chapter_overview 'dep_cat', etc columns.
#'
#' @param chapter_overview *What goes in each chapter*
#'
#'   `obj:<data.frame>|obj:<tbl_df>` // Required
#'
#'   Data frame (or tibble, possibly grouped). One row per chapter. Should
#'   contain the columns 'chapter' and 'dep', Optionally 'indep' (independent
#'   variables) and other informative columns as needed.
#'
#' @param ... *Dynamic dots*
#'
#'   <[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)>
#'
#'   Arguments forwarded to the corresponding functions that create the elements.
#'
#' @param title *Title of report*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Added automatically to YAML-header of index.qmd-file.
#'
#' @param authors *Authors of entire report*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   If NULL, infers from chapter_overview$authors, and collates for entire report.
#'
#' @param mesos_report *Whether to produce reports per mesos group*
#'
#'    `scalar<logical>` // *default:* `FALSE`
#'    If false, returns a regular single report.
#'
#' @param mesos_var *Variable in ´data´ indicating groups to tailor reports for*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Column name in data indicating the groups for which mesos reports will be produced.
#'
#' @param label_separator *Variable label separator*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   String to split labels on main question and sub-items.
#'
#' @param name_separator *Variable name separator*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   String to split column names in data between main question and sub-items
#'
#' @param index_yaml_file,report_yaml_file *Path to YAML-file to insert into index.qmd and report.qmd respectively*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Path to file used to insert header YAML, in index and report files.
#'
#' @param chapter_yaml_file *Path to YAML-file to insert into each chapter qmd-file*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Path to file used to insert header YAML, in each chapter.
#'
#' @param index_filename *Index filename*
#'
#'   `scalar<character>` // *default:* `"index.qmd"` (`optional`)
#'
#'   The name of the main index Quarto file (and its subfolder) used as landing page for each report. Will link to a PDF (report.qmd) which collects all chapters.
#'
#' @param qmd_start_section_filepath,qmd_end_section_filepath *Path to qmd-bit for start/end of each qmd*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Path to qmd-snippet placed before/after body of all chapter qmds.
#'
#' @param path *Output path*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Path to save all output.
#'
#' @param element_names *Elements to be reported*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Elements to be reported for all sets (batteries) of y-variables.
#'
#' @param data_label *Data label*
#'
#'   `scalar<character>` // *default:* `"proportion"` (`optional`)
#'
#'   One of "proportion", "percentage", "percentage_bare", "count", "mean", or "median".
#'
#' @param showNA *Show/hide NA in categorical variables*
#'
#'   `scalar<logical>` // *default:* `NULL` (`optional`)
#'
#'   Whether to show NA in categorical variables (one of c("ifany", "always", "never")).
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
#'   NOTE: Future version will likely postpone formatting this until `gt()`, `kable()`, etc.
#'
#' @param descend *Sorting order*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Reverse sorting of `sort_by`.
#'
#' @param hide_label_if_prop_below *Hide label threshold*
#'
#'   `scalar<numeric>` // *default:* `NULL` (`optional`)
#'
#'   Whether to hide label if below this value.
#'   NOTE: Future versions will likely distinguish between element_types.
#'
#' @param contents *Text interpretations*
#'
#'   `vector<character>` // *default:* all available (`optional`)
#'
#'   The type of text interpretations to return.
#'
#' @param include_numbers *Include numbers*
#'
#'   `scalar<logical>` // *default:* `NULL` (`optional`)
#'
#'   Whether or not to include the actual numbers in parentheses.
#'
#' @param require_common_categories *Check common categories*
#'
#'   `scalar<logical>` // *default:* `NULL` (`optional`)
#'
#'   Whether to check if all items share common categories.
#'
#' @param n_top_bottom *Top and bottom entries to report*
#'
#'   `scalar<integer>` // *default:* `NULL` (`optional`)
#'
#'   The number of top and bottom entries to report.
#'
#' @param sort_by *What to sort output by*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'  Sort output (and collapse if requested).
#'
#' \itemize{
#' \item{".top"}{The proportion for the highest category available in the variable.}
#' \item{".upper"}{The sum of the proportions for the categories above the middle category.}
#' \item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
#' \item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
#' \item{".lower"}{The sum of the proportions for the categories below the middle category.}
#' \item{".bottom"}{The proportions for the lowest category available in the variable.}
#' \item{".variable_label"}{Sort by the variable labels.}
#' \item{".id"}{Sort by the variable names.}
#' \item{".by_group"}{The groups of the by argument.}
#' \item{Character vector of category labels to sum together.}
#' }
#' @param plot_height_multiplier *Height multiplier*
#'
#'   `scalar<double>` // *default:* `1`
#'
#'   Height in cm per chart entry, for all static plots.
#'
#' @param plot_height_fixed_constant *Height constant addition*
#'
#'   `scalar<double>` // *default:* `0`
#'
#'   Fixed height in cm to add to all static plots.
#'
#' @param return_raw *NOT IN USE*
#'
#'   `scalar<integer>` // *default:* `FALSE`
#'
#'   Whether to return the raw static element.
#'
#' @param label_font_size *Data labels font size*
#'
#'   `scalar<integer>` // *default:* `10` (`optional`)
#'
#'   Font size for data labels.
#'
#' @param main_font_size *Main font size*
#'
#'   `scalar<integer>` // *default:* `12` (`optional`)
#'
#'   Font size for all other text.
#'
#' @param colour_palette_nominal,colour_palette_ordinal *Colour palettes (nominal and ordinal)*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Must contain at least the number of unique values (including missing) in the data set.
#'
#' @param colour_na *Colour for NA category*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Colour as a single string for NA values.
#'
#' @param colour_2nd_binary_cat *Colour for second binary category*
#'
#'   `scalar<character>` // *default:* `"#ffffff"` (`optional`)
#'
#'   Colour for second category in binary variables. Often useful to hide this.
#'
#' @param font_family *Font family*
#'
#'   `scalar<character>` // *default:* `"sans"` (`optional`)
#'
#'   Word font family. See officer::fp_text.
#'
#' @param vertical *Orientation of plots*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   If FALSE (default), then horizontal plots.
#'
#' @param x_axis_label_width *X-axis label width of plots*
#'
#'   `scalar<integer>` // *default:* `20` (`optional`)
#'
#'   Width of the labels used for the categorical column names.
#'
#' @param reps *Number of permutations*
#'
#'   `scalar<integer>` // *default:* `100` (`optional`)
#'
#'   Number of permutations to be performed in bootstrap significance tests.
#'
#' @param hide_test_if_n_below *Threshold n for hiding significance test*
#'
#'   `scalar<integer>` // *default:* `0` (`optional`)
#'
#'   If N is below this value, p-value will not be shown.
#'
#' @param organize_by *Grouping columns*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Column names used for identifying chapters and sections.
#'
#' @param information *Pre-computed information*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Which pre-computed information for each variable-category to display.
#'
#' @param always_show_bi_for_by *Always show bivariate for by-variable*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Specific combinations with a by-variable where bivariates should always be shown.
#'
#' @param categories_treated_as_na *NA categories*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Categories that should be treated as NA.
#'
#' @param variables_always_at_top,variables_always_at_bottom *Top/bottom variables*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Column names in `data` that should always be placed at the top or bottom of figures/tables.
#'
#' @param attach_chapter_dataset *Toggle inclusion of chapter-specific datasets in qmd-files*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   Whether to save in each chapter folder an 'Rds'-file with the chapter-specific dataset, and load it at the top of each QMD-file.
#'
#' @param auxiliary_variables *Auxiliary variables to be included in datasets*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Column names in `data` that should always be included in datasets for chapter qmd-files, if `attach_chapter_dataset=TRUE`. Not publicly available.
#'
#' @param panel_tabset_mesos *mesos panel tabset*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Whether in mesos reports the comparison group should be displayed as
#'   a Quarto panel tabset (`TRUE`), or above each other (`FALSE`).
#'
#' @param open_after_drafting *Whether to open index.qmd*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to open the main output file (index.qmd) after completion.
#'
#' @param totals *Include totals*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to include totals in the output.
#'
#' @param flexi *Create page with user-editable categorical plots and tables*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to include totals in the output.
#'
#' @param hide_bi_entry_if_sig_above *p-value threshold for hiding bivariate entry*
#'
#'   `scalar<double>` // *default:* `1` (`optional`)
#'
#'   Whether to hide bivariate entry if significance is above this value.
#'   Defaults to showing all.
#'
#' @param hide_chr_for_others *Hide open response displays for others*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   For mesos reports using the element "chr_table", open responses are
#'   displayed for also the entire sample (`FALSE`) or only for the mesos
#'   group to ensure data privacy (`TRUE`).
#'
#' @param plot_height_max *Maximum plot height*
#'
#'   `scalar<double>` // *default:* `10` (`optional`)
#'
#'   Maximum height for the plot.
#'
#' @param plot_height_min *Minimum plot height*
#'
#'   `scalar<double>` // *default:* `2` (`optional`)
#'
#'   Minimum height for the plot.
#'
#' @param png_scale *PNG scale*
#'
#'   `scalar<double>` // *default:* `1` (`optional`)
#'
#'   Scale factor for PNG output.
#'
#' @param png_width,png_height *PNG width and height*
#'
#'   `scalar<double>` // *default:* `12` (`optional`)
#'
#'   Width for PNG output.
#'
#' @param vertical_height *Vertical height*
#'
#'   `scalar<double>` // *default:* `NULL` (`optional`)
#'
#'   Height for vertical layout of plot? NEEDS CHECKING
#'
#' @param max_width_obj *Maximum object width*
#'
#'   `scalar<integer>` // *default:* `NULL` (`optional`)
#'
#'   Maximum width for object names in the Quarto script. In particular useful
#'   when having label as part of the structure.
#'
#' @param max_width_file *Maximum filename width*
#'
#'   `scalar<integer>` // *default:* `NULL` (`optional`)
#'
#'   Maximum width for any filename. Due to OneDrive having a max path of about
#'   400 characters, this can quickly be exceeded with a long path base path,
#'   long file names if using labels as part of structure, and hashing with
#'   Quarto's `cache: true` feature. This argument truncates the filenames.
#'
#' @param mesos_first *mesos first*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to place the mesos group element before or after the entire sample.
#'
#' @param single_y_bivariate_elements *Single Y bivariate elements*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Whether to display bivariates with a single dependent variable. NEEDS CHECKING.
#'
#' @param translations *Translations*
#'
#'   `list` // *default:* `saros:::.saros.env$defaults$translations` (`optional`)
#'
#'   Named list of strings for translations.
#'
#' @importFrom rlang !!!
#'
#' @return Path to index qmd-file. If not specified in the yaml_path file, will default to index.qmd.
#' @export
#'
#' @examples
#' \donttest{
#' index_filepath <-
#'   draft_report(
#'     chapter_overview = ex_survey_ch_overview,
#'     data = ex_survey1,
#'     path = tempdir())
#' #quarto::quarto_render(index_filepath)
#' #index_filepaths <-
#'#   draft_report(
#' #    chapter_overview = ex_survey_ch_overview,
#'  #   data = ex_survey1,
#'   #  mesos_report = TRUE,
#'    # mesos_var = "f_uni",
#'     #path = tempdir())
#' #lapply(index_filepaths, quarto::quarto_render)
#' }
draft_report <-
  function(data,
           chapter_overview = NULL,
           ...,
           path = "testreport",
           title = "Report",
           authors = NULL,
           mesos_report = FALSE,
           mesos_var = NULL,
           label_separator = " - ",
           name_separator = NULL,
           index_yaml_file = NULL,
           report_yaml_file = NULL,
           chapter_yaml_file = NULL,
           qmd_start_section_filepath = NULL,
           qmd_end_section_filepath = NULL,
           index_filename = "index.qmd",
           organize_by = c("chapter", ".variable_label_prefix", ".element_name"),

           element_names =
             c(#"opening_text",
               #"uni_opening_text",
               #"method",

               #"uni_int_text",
               "uni_cat_text",
               #"uni_chr_text",
               #"uni_int_plot",
               "uni_cat_prop_plot",
               "uni_cat_freq_plot",
               # "uni_chr_plot",
               # "uni_int_table",
               "uni_cat_table",
               "uni_chr_table",
               "uni_sigtest",

               # "bi_opening_text",
               # "bi_catcat_text",
               # "bi_intcat_text",
               # "bi_catint_text",
               # "bi_intint_text",

               "hline",

               "bi_catcat_prop_plot",
               "bi_catcat_freq_plot",
               "bi_catcat_prop_plot2",
               "bi_catcat_freq_plot2",
               "bi_catcat_table",
               # "bi_intcat_plot",
               # "bi_intint_plot",
               # "bi_intcat_table",
               # "bi_intint_table",
               "bi_sigtest"
             ),

           sort_by = ".upper",
           data_label = saros::get_data_label_opts(),
           always_show_bi_for_by = NULL,
           categories_treated_as_na = NULL,
           variables_always_at_top = NULL,
           variables_always_at_bottom = NULL,
           auxiliary_variables = NULL,
           return_raw = TRUE,
           attach_chapter_dataset = TRUE,
           panel_tabset_mesos = TRUE,
           showNA = c("never", "always", "ifany"),
           totals = FALSE,
           hide_label_if_prop_below = .01,
           hide_bi_entry_if_sig_above = 1,
           hide_test_if_n_below = 10,
           hide_chr_for_others = TRUE,
           label_font_size = 8,
           main_font_size = 8,
           x_axis_label_width = 20,
           plot_height_multiplier = NA_real_,
           plot_height_fixed_constant = NA_real_,
           plot_height_max = 20,
           plot_height_min = 1.5,
           png_scale = 1.2,
           png_width = 14,
           png_height = 16,
           vertical_height = 12,
           max_width_obj = 90,
           max_width_file = 64,
           font_family = "sans",
           open_after_drafting = FALSE,

           vertical = FALSE,
           mesos_first = TRUE,
           single_y_bivariate_elements = FALSE,
           descend = TRUE,
           require_common_categories = TRUE,
           flexi = TRUE,


           colour_palette_nominal = NULL,
           colour_palette_ordinal = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = NULL,
           digits = 1,
           data_label_decimal_symbol = ".",
           reps = 1000,
           information =
             c(".variable_label", #".variable_name",
               ".category",
               ".count", ".count_se",
               ".proportion", ".proportion_se",
               ".mean", ".mean_se", #".mean_base",
               ".data_label", ".comb_categories", ".sum_value"),
           contents = c("intro", "not_used_category",
                        "mode_max",
                        "value_max", "value_min", "value_diff", # Diff not implemented
                        "mean_max", "mean_min", "mean_diff",
                        "median_max", "median_min", "median_diff", # Not implemented
                        "variance_max", "variance_min"), # Not implemented
           include_numbers = TRUE, # not implemented
           n_top_bottom = 1,

           translations =
             list(last_sep = " and ",
                  download_report = "Download report (PDF)",
                  intro_prefix = "We will now look at the questions asked regarding ",
                  intro_suffix = "",
                  mode_max_onfix = " on ",
                  mode_max_prefix = "The most common responses were ",
                  mode_max_suffix = "",
                  not_used_prefix = "The following response categories were not used: ",
                  not_used_suffix = "",
                  value_max_prefix = "",
                  value_max_infix = " {?is/are} the {dots$n_top_bottom} item{?s} where the most responded ",
                  value_max_suffix = "",
                  value_min_prefix = "",
                  value_min_infix = " {?is/are} the {dots$n_top_bottom} item{?s} where the fewest responded ",
                  value_min_suffix = "",
                  mean_onfix = "M = ",
                  mean_max_prefix = "They have highest mean on ",
                  mean_max_suffix = "",
                  mean_min_prefix = "They have lowest mean on ",
                  mean_min_suffix = "",
                  median_onfix = "Median = ",
                  median_max_prefix = "They have highest median on ",
                  median_max_suffix = "",
                  median_min_prefix = "They have lowest median on ",
                  median_min_suffix = "",
                  intro_by_prefix = "We will now look at the questions asked regarding ",
                  intro_by_infix = " broken down by ",
                  intro_by_suffix = "",
                  by_breakdown = " by ",
                  n_equal_prefix = " (N &equals; ",
                  n_equal_suffix = ")",
                  table_heading_N = "Total (N)",
                  by_total = "Everyone",
                  sigtest_prefix = "Significance testing of ",
                  sigtest_suffix = "",
                  mesos_group_prefix = " Group: ",
                  mesos_group_suffix = "",
                  mesos_label_all_others = "Others",
                  empty_chunk_text = "\nText\n",
                  flexi_input_chapter = "Chapter(s):",
                  flexi_input_dep = "Dependent variable(s):",
                  flexi_input_indep = "Independent variable:",
                  flexi_input_mesos_group = "Filter:",
                  flexi_figure_type = "Figure type:",
                  flexi_data_label = "Summary to display",
                  flexi_showNA = "Show NA (Missing)",
                  flexi_sort_by = "Sort by",
                  flexi_totals = "Totals",
                  flexi_digits = "Digits after decimal",
                  flexi_table = "Table",
                  flexi_figure = "Figure",
                  flexi_cols_variable_name = "Variable name",
                  flexi_cols_variable_label = "Variable label",
                  flexi_cols_category = "Response category",
                  flexi_cols_count = "N",
                  flexi_cols_count_se = "SE(N)",
                  flexi_cols_proportion = "Proportion",
                  flexi_cols_proportion_se = "SE(Proportion)",
                  flexi_cols_mean = "Mean",
                  flexi_cols_mean_se = "SE(Mean)",
                  flexi_cols_data_label = "Data label",
                  flexi_cols_comb_categories = "Combined categories",
                  flexi_cols_sum_value = "Sum of data label across combined categories",
                  flexi_validate = "Error: Columns must have some categories in common.",
                  flexi_settings = "Settings",
                  flexi_basic_settings = "Basic",
                  flexi_advanced_settings = "Advanced",
                  flexi_input_indep_none = "<none>",
                  flexi_figure_type_proportion = "Proportion",
                  flexi_figure_type_frequency = "Frequency",
                  flexi_hide_label_if_prop_below = "Hide label if proportion below:"
                  )
  ) {

    args <- utils::modifyList(as.list(environment()),
                              rlang::list2(...)
                              )
    timestamp <- proc.time()


    args <- argument_validation_and_insertion(params = args)


    data <- ungroup_data(data)


    # if(!rlang::is_null(dots$colour_na)) {
    #   data <-
    #   dplyr::mutate(data, dplyr::across(tidyselect::where(~{
    #     dplyr::n_distinct(.x, na.rm = dots$showNA == "never") == 2 &
    #       (is.factor(.x) | is.character(.x))}),
    #     .fns = ~forcats::fct_rev(.x)))
    # }

    chapter_overview <-
      validate_chapter_overview(chapter_overview=chapter_overview, args=args)
    chapter_overview <-
      dplyr::filter(chapter_overview, .data$.variable_role == "dep") ## TEMPORARY FIX!!!!!!!!!!!!!!!!!!!!!!!


    chapter_overview_indep <- dplyr::filter(chapter_overview, .data$.variable_role != "dep")

    if(nrow(chapter_overview)==0) cli::cli_abort("{.var chapter_overview} is empty! Something is not right. Are there no factors in your data? Consider `chapter_overview=NULL` for everything in a single phantom chapter")

    all_authors <- get_authors(data = chapter_overview, col="author")

    if(rlang::is_false(args$mesos_report) ||
       rlang::is_null(args$mesos_var)) {

      uniques <- NA_character_

    } else {
      # Mesos reports
      uniques <- pull_uniques(data[[args$mesos_var]])

      if(any(nchar(uniques) > 12)) {
        cli::cli_warn(c(x="mesos_var has levels > 12 characters: {{uniques[nchar(uniques)>12]}}.",
                        i="This creates filepaths that are likely too long for Sharepoint to handle..."))
      }
    }
    index_filepath <-
      lapply(X =
               cli::cli_progress_along(uniques,
                                       format = "Generating mesos report for... {uniques[cli::pb_current]}",
                                       clear = FALSE,
                                       auto_terminate = FALSE),
             FUN = function(.x) {


               if(is.na(uniques[.x])) { # Macro

                 mesos_group <- NULL


               } else {  # Mesos

                 mesos_group <- uniques[.x]
                 path <- file.path(path, uniques[.x])
                 args$title <- stringi::stri_c(args$title,
                                          uniques[.x],
                                          ignore_null=TRUE)

               }


               chapter_filepaths <-
                 rlang::exec(
                   gen_qmd_chapters,
                   chapter_overview = chapter_overview,
                   data = data,
                   mesos_group = mesos_group,
                   path = path,
                   !!!args[!names(args) %in% c("chapter_overview", "path", "data")])



               report_filepath <-
                 rlang::exec(
                   gen_qmd_index,
                   title = args$title,
                   authors = all_authors,
                   index_filepath = file.path(path, stringi::stri_c(args$title, ".qmd", ignore_null = TRUE)),
                   chapter_filepaths = chapter_filepaths,
                   yaml_file = args$report_yaml_file,
                   !!!args[!names(args) %in% c("title", "authors", "report_yaml_file")],
                   call = rlang::caller_env())

               index_filepath <-
                 rlang::exec(
                   gen_qmd_index,
                   title = args$title,
                   authors = all_authors,
                   index_filepath = file.path(path, args$index_filename),
                   chapter_filepaths = NULL,
                   report_filepath = report_filepath,
                   yaml_file = args$index_yaml_file,
                   !!!args[!names(args) %in% c("title", "authors", "index_yaml_file")],
                   call = rlang::caller_env())


               index_filepath

             })

    index_filepath <- as.character(unlist(index_filepath))
    if(interactive() && isTRUE(args$open_after_drafting)) {
      lapply(index_filepath, utils::browseURL)
    }

    cat(proc.time()-timestamp)
    cat("\n")
    index_filepath
  }
