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
#' @param index_yaml_file,report_yaml_file *Path to YAML-file to insert into index.qmd and 0_report.qmd respectively*
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
#'   The name of the main index Quarto file used as landing
#'   page for each report. Will link to a PDF (report.qmd) which collects all chapters.
#'
#' @param report_filename *Report filename*
#'
#'   `scalar<character>` // *default:* `"0_report.qmd"` (`optional`)
#'
#'   The name of the main report QMD-file used when compiling a complete report
#'   collecting all chapters in its folder (except itself).
#'   If provided, will be linked to in the index.
#'   If NULL, will generate a filename based on the report title, prefixed with "0_".
#'   To turn off, set `pdf=FALSE`.
#'
#'
#' @param chapter_qmd_start_section_filepath,chapter_qmd_end_section_filepath,index_qmd_start_section_filepath,index_qmd_end_section_filepath,report_qmd_start_section_filepath,report_qmd_end_section_filepath, *Path to qmd-bit for start/end of each qmd*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Path to qmd-snippet placed before/after body of all chapter/index/report qmd-files.
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
#'   Reverse sorting of `sort_by` in figures and tables. See `arrange_section_by`
#'   for sorting of report sections.
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
#' \describe{
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
#' @param plot_height_multiplier_per_vertical_letter,plot_height_multiplier_per_horizontal_line *Height multiplier*
#'
#'   `scalar<double>` // *default:* `.1`
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
#' @param main_font_size,label_font_size,strip_font_size,legend_font_size *Font sizes*
#'
#'   `scalar<integer>` // *default:* `12` (`optional`)
#'
#'   Font sizes for general text (10), data label text (3), strip text (7) and legend text (7).
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
#' @param x_axis_label_width,strip_width *Label width of x-axis and strip texts in plots*
#'
#'   `scalar<integer>` // *default:* `20` (`optional`)
#'
#'   Width of the labels used for the categorical column names in x-axis texts and strip texts.
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
#' @param arrange_section_by *Grouping columns*
#'
#'   `vector<character>` or `named vector<logical>` // *default:* `NULL` (`optional`)
#'
#'   Column names used for sorting section within each organize_by group. If
#'   character vector, will assume all are to be arranged in ascending order.
#'   If a named logical vector, FALSE will indicate ascending, TRUE descending.
#'   Defaults to sorting in ascending order (alphabetical) for commonly needed
#'   variable name/label info, and in descending order for element_names as one
#'   typically wants *u*nivariates before *b*ivariates.
#'
#' @param na_first_in_section *Whether to place NAs first when sorting*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Default ascending and descending sorting with `dplyr::arrange()` is to place
#'   NAs at the end. This would have placed univariates at the end, etc. Thus,
#'   saros places NAs first in the section. Set this to FALSE to override.
#'
#' @param ignore_heading_for_group *Ignore heading for group*
#'
#'  `vector<character>` // *default:* `NULL` (`optional`)
#'
#'  Type of refined chapter_overview data for which to suppress the heading
#'  in the report output. Typically variable_name_dep, variable_name_indep, etc.
#'
#' @param replace_heading_for_group *Replacing heading for group*
#'
#'  `named vector<character>` // *default:* `c(".variable_label_suffix_dep" = ".variable_name_dep")`
#'
#'  Occasionally, one needs to replace the heading with another piece of information
#'  in the refined chapter_overview. For instance, one may want to organize output
#'  by variable_name_indep, but to display the variable_label_indep instead. Use
#'  the name for the replacement and the value for the original.
#'
#' @param information *Pre-computed information*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Which pre-computed information for each variable-category to display.
#'
#' @param always_show_bi_for_indep *Always show bivariate for indep-variable*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Specific combinations with a by-variable where bivariates should always be shown.
#'
#' @param single_y_bivariates_if_indep_cats_above *Single y bivariates if indep-cats above ...*
#'
#'  `scalar<integer>` // *default:* `3` (`optional`)
#'
#'  Figures and tables for bivariates can become very long if the independent
#'  variable has many categories. This argument specifies the number of indep categories
#'  above which only single y bivariates should be shown.
#'
#' @param single_y_bivariates_if_deps_above *Single y bivariates if dep-vars above ...*
#'
#'  `scalar<integer>` // *default:* `20` (`optional`)
#'
#'  Figures and tables for bivariates can become very long if there are many dependent
#'  variables in a battery/question matrix. This argument specifies the number of dep variables
#'  above which only single y bivariates should be shown. Set to 0 to always show single y bivariates.
#'
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
#'   Whether to save in each chapter folder an 'Rds'-file with the
#'   chapter-specific dataset, and load it at the top of each QMD-file.
#'
#' @param auxiliary_variables *Auxiliary variables to be included in datasets*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Column names in `data` that should always be included in datasets for
#'   chapter qmd-files, if `attach_chapter_dataset=TRUE`. Not publicly available.
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
#' @param micro *Create page with raw data (micro data) and codebook*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to a page with local links to a raw dataset (in various formats) and codebook (in various formats).
#'
#' @param pdf *Create PDF of full report?*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to create a PDF of the entire report (all chapters included in a single file).
#'
#' @param hide_bi_entry_if_sig_above *p-value threshold for hiding bivariate entry*
#'
#'   `scalar<double>` // *default:* `1` (`optional`)
#'
#'   Whether to hide bivariate entry if significance is above this value.
#'   Defaults to showing all.
#'
#' @param hide_result_if_n_below *Hide result if N below*
#'
#'  `scalar<integer>` // *default:* `10` (`optional`)
#'
#'  Whether to hide result if N for a given datasets (or mesos group) is below
#'  this value. NOTE: Exceptions will be made to chr_table and chr_plot as these are
#'  typically exempted in the first place. This might change in the future with
#'  a separate argument.
#'
#' @param hide_chr_for_others *Hide open response displays for others*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   For mesos reports using the element "chr_table", open responses are
#'   displayed for also the entire sample (`FALSE`) or only for the mesos
#'   group to ensure data privacy (`TRUE`).
#'
#' @param plot_height_multiplier_per_facet *Plot height multiplier per facet*
#'
#'   `scalar<double>` // *default:* `0.95` (`optional`)
#'
#'   Multiplier for plot height per facet. Defaults to optimal at .95, i.e. slightly less than no change (1).
#'
#' @param plot_height_multiplier_per_legend_line *Plot height multiplier per legend line*
#'
#'   `scalar<double>` // *default:* `1.1` (`optional`)
#'
#'   Multiplier for plot height per horizontal line of legend.
#'   Defaults to optimal at 1.1, i.e. slightly more than no change (1).
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
#' @param docx *Whether to produce docx-files with mschart versions of the figures*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   mschart figures are vector graphics which may be of use for some
#'
#' @param hide_variable_if_all_na *Hide variable from outputs if containing all NA*
#'
#'   `scalar<boolean>` // *default:* `TRUE` (`optional`)
#'
#'   Whether to remove all variables (in particular useful for mesos) if all values are NA
#'
#' @param hide_axis_text_if_single_variable *Hide y-axis text if just a single variable*
#'
#'   `scalar<boolean>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to hide text on the y-axis label if just a single variable
#'
#' @param strip_angle *Angle on the facet strip in plots*
#'
#'   `scalar<double>` // *default:* `0`
#'
#' @param vertical_height *Vertical height*
#'
#'   `scalar<double>` // *default:* `NULL` (`optional`)
#'
#'   Height for vertical layout of plot? NEEDS CHECKING
#'
#' @param table_main_question_as_header *Table main question as header*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to include the main question as a header in the table.
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
#' @param max_clean_folder_name *Maximum clean folder name length*
#'
#'   `scalar<integer>` // *default:* `NULL` (`optional`)
#'
#'   Whereas `max_width_file` truncates the file name, this argument truncates
#'   the folder name. It will not impact the report or chapter names in website,
#'   only the folders.
#'
#' @param max_path_warning_threshold *Maximum number of characters in paths warning*
#'
#'   `scalar<integer>` // *default:* `260` (`optional`)
#'
#'   Microsoft has set an absolute limit of 260 characters for its Sharepoint/OneDrive
#'   file paths. This will mean that files with cache (hash suffixes are added) will
#'   quickly breach this limit. When set, a warning will be returned if files are found
#'   to be longer than this threshold. Also note that spaces count as three characters
#'   due to its URL-conversion: %20. To avoid test, set to Inf
#'
#' @param mesos_first *mesos first*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to place the mesos group element before or after the entire sample.
#'
#' @param log_file *Path to log file*
#'
#'   `scalar<string>` // *default:* `"_log.txt"` (`optional`)
#'
#'   Path to log file. Set to NULL to disable logging.
#'
#' @param serialized_format *Serialized format*
#'
#'   `scalar<string>` // *default:* `"rds"`
#'
#'   Format for serialized data. One of `"rds"` (default), `"qs"` or `"fst"`.
#'   The latter two requires the respective packages to be installed.
#'   qs is usually the fastest and most space efficient, but sets package
#'   dependencies on the report.
#'
#' @param tabular_format *Serialized format*
#'
#'   `scalar<string>` // *default:* `"delim"`
#'
#'   Format for pretty tabular data, meant for end-user to peruse and will be
#'   linked to in reports (the graph data, etc). One of `"delim"` (tab-separated delim-files)
#'   `"xlsx"` requires `writexl`-package), `"csv"` or `"csv2"` (requires `readr`-package.
#'   `"dta"` or `"sav"` requires `haven`-package. Currently must be specified,
#'   in the future this will become an optional argument.
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
#'  draft_report(
#'     chapter_overview = ex_survey_ch_overview,
#'     data = ex_survey,
#'     path = tempdir())
#' index_filepaths <-
#'   draft_report(
#'    chapter_overview = ex_survey_ch_overview,
#'    data = ex_survey,
#'    mesos_report = TRUE,
#'    mesos_var = "f_uni",
#'    path = tempdir())
#' }
draft_report <-
  function(data,
           chapter_overview = NULL,
           ...,
           path,
           title = "Report",
           authors = NULL,
           mesos_report = FALSE,
           mesos_var = NULL,
           label_separator = " - ",
           name_separator = NULL,

           chapter_yaml_file = NULL,
           chapter_qmd_start_section_filepath = NULL,
           chapter_qmd_end_section_filepath = NULL,

           index_filename = "index",
           index_yaml_file = NULL,
           index_qmd_start_section_filepath = NULL,
           index_qmd_end_section_filepath = NULL,

           report_filename = "0_report",
           report_yaml_file = NULL,
           report_qmd_start_section_filepath = NULL,
           report_qmd_end_section_filepath = NULL,

           element_names =
             c(#"opening_text",
               #"uni_opening_text",
               #"method",

               #"uni_int_text",
               # "uni_cat_text",
               #"uni_chr_text",
               #"uni_int_plot",
               "uni_cat_prop_plot",
               "uni_cat_freq_plot",
               # "uni_chr_plot",
               # "uni_int_table",
               "uni_cat_table",
               "uni_chr_table",

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

           # What sections to display (and their structure)
           single_y_bivariates_if_indep_cats_above = 3,
           single_y_bivariates_if_deps_above = 20,
           always_show_bi_for_indep = NULL,
           hide_bi_entry_if_sig_above = 1,
           hide_test_if_n_below = 10,
           hide_result_if_n_below = 10,
           hide_variable_if_all_na = TRUE,
           hide_chr_for_others = TRUE,
           organize_by = c("chapter",
                           ".variable_label_prefix_dep",
                           ".variable_name_indep",
                           ".element_name"),
           arrange_section_by = c(chapter = FALSE,
                                  .variable_name_dep = FALSE,
                                  .variable_name_indep = FALSE,
                                  .element_name = FALSE),
           na_first_in_section = TRUE,

           ignore_heading_for_group = c(".element_name",
                                        ".variable_type_dep",
                                        ".variable_type_indep",
                                        "chapter"),
           replace_heading_for_group = c(".variable_label_suffix_dep" = ".variable_name_dep",
                                         ".variable_label_suffix_indep" = ".variable_name_indep"),

           mesos_first = TRUE,
           descend = TRUE, # DROP, SHOULD NOW BE IN ARRANGE
           require_common_categories = TRUE,
           panel_tabset_mesos = TRUE,

           # Formats and attachments
           pdf = TRUE, # DROP? Should now be in the index/report_prefix
           attach_chapter_dataset = TRUE,
           auxiliary_variables = NULL,
           micro = FALSE, # DROP
           serialized_format = c("rds", "qs"),
           tabular_format = c("delim", "xlsx", "csv", "csv2", "tsv", "sav", "dta"),


           return_raw = TRUE,

           # Path limits
           max_width_obj = 128,
           max_width_file = 64,
           max_clean_folder_name = 12,  # Tidy up argument name: max_width_clean_folder_name
           max_path_warning_threshold = 260,  # Tidy up argument name: max_width_path_warning

           open_after_drafting = FALSE, # Drop?

           # Debugging
           log_file = NULL,


           # For contents: figures and tables - might be extracted in saros2.0
           digits = 1,
           data_label_decimal_symbol = ".",
           showNA = c("never", "always", "ifany"),
           totals = FALSE,
           categories_treated_as_na = NULL,
           variables_always_at_top = NULL,
           variables_always_at_bottom = NULL,
           sort_by = ".upper",
           data_label = saros::get_data_label_opts(),

           # Specifically for figures
           hide_label_if_prop_below = .01,
           hide_axis_text_if_single_variable = FALSE,
           main_font_size = 10, # Remove all font size stuff
           label_font_size = 3,
           strip_font_size = 7,
           legend_font_size = 7,
           strip_width = 15,
           strip_angle = 0,
           x_axis_label_width = 20, #Strictly speaking y_axis_text
           plot_height_multiplier_per_horizontal_line = NA, # Separate into exported function
           plot_height_multiplier_per_vertical_letter = .2,
           plot_height_multiplier_per_facet = .95,
           plot_height_multiplier_per_legend_line = 1.1,
           plot_height_fixed_constant = 0,
           plot_height_max = 8,
           plot_height_min = 1.5,
           vertical_height = 12,
           vertical = FALSE,
           png_scale = 1.2, # Drop?
           png_width = 14,
           png_height = 16,
           font_family = "sans", # Drop
           docx = FALSE,

           colour_palette_nominal = NULL, # Drop in saros2.0
           colour_palette_ordinal = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = NULL,

           # Tables
           table_main_question_as_header = FALSE,

           # Sigtest
           reps = 1000, # DROP

           # Text stuff (drop?)
           information =
             c(".variable_label_dep", #".variable_name",
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


           translations = # In saros2.0 this can be turned into glue templates
             list(last_sep = " and ",
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
                  empty_chunk_text = "\n"
                  )
  ) {

    args <- utils::modifyList(as.list(environment()),
                              rlang::list2(...)
                              )
    timestamp <- proc.time()


    args <- argument_validation_and_insertion(params = args)


    data <- ungroup_data(data)


    # if(!is.null(dots$colour_na)) {
    #   data <-
    #   dplyr::mutate(data, dplyr::across(tidyselect::where(~{
    #     dplyr::n_distinct(.x, na.rm = dots$showNA == "never") == 2 &
    #       (is.factor(.x) | is.character(.x))}),
    #     .fns = ~forcats::fct_rev(.x)))
    # }

    chapter_overview <-
      refine_chapter_overview(chapter_overview = chapter_overview,
                              data=data,
                              !!!args[!names(args) %in% .saros.env$ignore_args])

    if(nrow(chapter_overview)==0) cli::cli_abort("{.var chapter_overview} is empty! Something is not right. Are there no factors in your data? Consider `chapter_overview=NULL` for everything in a single phantom chapter")

    all_authors <- get_authors(data = chapter_overview, col="author")

    if(isFALSE(args$mesos_report) ||
       !is_string(args$mesos_var)) {

      uniques <- NA_character_

    } else {
      # Mesos reports
      uniques <- pull_uniques(data[[args$mesos_var]])
    }

    report_foldername_clean <- filename_sanitizer(uniques, max_chars = args$max_clean_folder_name)

    index_filepath <-
      lapply(X =
               cli::cli_progress_along(uniques, # Not working well
                                       format = "Generating mesos report for... {uniques[cli::pb_current]}",
                                       clear = FALSE,
                                       auto_terminate = FALSE),
             FUN = function(.x) {


               if(is.na(uniques[.x])) { # Macro

                 mesos_group <- NULL


               } else {  # Mesos

                 mesos_group <- uniques[.x]
                 path <- file.path(path, report_foldername_clean[.x])
                 args$title <- stringi::stri_c(args$title,
                                          uniques[.x],
                                          ignore_null=TRUE)


               }

# browser()
               chapter_filepaths <-
                 rlang::exec(
                   gen_qmd_chapters,
                   chapter_overview = chapter_overview,
                   data = data,
                   mesos_group = mesos_group,
                   path = path,
                   !!!args[!names(args) %in% .saros.env$ignore_args])




               if(isTRUE(args$pdf)) {

                 report_filepath <-
                     gen_qmd_file(
                       path = path,
                       filename = args$report_filename,
                       yaml_file = args$report_yaml_file,
                       qmd_start_section_filepath = args$report_qmd_start_section_filepath,
                       qmd_end_section_filepath = args$report_qmd_end_section_filepath,
                       title = args$title,
                       authors = all_authors,
                       output_formats = NULL,
                       output_filename = NULL,
                       call = rlang::caller_env())
               }

               index_filepath <-
                   gen_qmd_file(
                   path = path,
                   filename = args$index_filename,
                   yaml_file = args$index_yaml_file,
                   qmd_start_section_filepath = args$index_qmd_start_section_filepath,
                   qmd_end_section_filepath = args$index_qmd_end_section_filepath,
                   title = args$title,
                   authors = all_authors,
                   output_formats = if(!is.null(args$report_yaml_file)) find_yaml_formats(args$report_yaml_file),
                   output_filename = args$report_filename,
                   call = rlang::caller_env())

               index_filepath

             })

    if(isTRUE(micro)) {
      gen_micro(data = data,
                cols = unique(c(as.character(chapter_overview$.variable_name_dep),
                                as.character(chapter_overview$.variable_name_indep),
                                args$mesos_var,
                                args$auxiliary_variables)),
                serialized_format = args$serialized_format,
                tabular_format = args$tabular_format)
    }

    index_filepath <- as.character(unlist(index_filepath))
    if(interactive() && isTRUE(args$open_after_drafting)) {
      lapply(index_filepath, utils::browseURL)
    }

    max_paths <-
      nchar(list.files(path=path,
               all.files = TRUE, full.names = TRUE, recursive = TRUE))
    if(.Platform$OS.type == "windows" && max(max_paths, na.rm=TRUE) >= max_path_warning_threshold) {
      cli::cli_warn(c(x="{.val {length(max_path[max_path >= max_path_warning_threshold])}} files exceed {.arg max_path_warning_threshold} ({max_path_warning_threshold}) characters.",
                      i="This may create problems in Windows if path is on a Sharepoint/OneDrive.",
                      i="Omit warning by adjusting {.arg max_path_warning_threshold}"))
    }


    cat(proc.time()-timestamp)
    cat("\n")
    if(is_string(args$log_file)) {
      cat("Run time: ", proc.time()-timestamp, file=args$log_file)
    }
    stringi::stri_replace_all_regex(index_filepath, pattern="\\\\+", replacement="/")
  }
