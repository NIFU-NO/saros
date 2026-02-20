# Changelog

## saros 1.6.1.9000 (dev)

### New Features

- Added
  [`make_file_links()`](https://nifu-no.github.io/saros/reference/make_file_links.md)
  function for dynamically creating markdown lists with links to files.
  Extracts document titles from DOCX, PPTX, and PDF file metadata and
  generates formatted markdown lists. Ideal for creating navigation
  links in Quarto/RMarkdown documents that point to generated reports in
  a folder. Supports glob patterns, recursive search, and customizable
  list formatting (unordered or numbered)
- Enhanced
  [`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md) to
  automatically apply colour palettes from
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md)
  global settings when saving plots. This ensures saved PNG/PDF images
  match the appearance of interactive plots displayed with
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md).
  Palette settings can be configured via
  `global_settings_set(fn_name = "girafe", new = list(palette_codes = ...))`
  and will be automatically applied when saving plots through
  [`get_fig_title_suffix_from_ggplot()`](https://nifu-no.github.io/saros/reference/get_fig_title_suffix_from_ggplot.md)
  or direct
  [`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md)
  calls
- Added `folder` and `file_prefix` parameters to
  [`get_fig_title_suffix_from_ggplot()`](https://nifu-no.github.io/saros/reference/get_fig_title_suffix_from_ggplot.md)
  for controlling where files are saved and what prefix to use for
  filenames
- Enhanced
  [`get_fig_title_suffix_from_ggplot()`](https://nifu-no.github.io/saros/reference/get_fig_title_suffix_from_ggplot.md)
  to support global settings inheritance via
  [`global_settings_set()`](https://nifu-no.github.io/saros/reference/global_settings_set.md),
  consistent with other saros functions like
  [`make_link()`](https://nifu-no.github.io/saros/reference/make_link.md)
  and [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
- **Unified DOCX plot types**: Merged `cat_prop_plot_docx` and
  `cat_freq_plot_docx` into a single `cat_plot_docx` type, bringing it
  in line with the `cat_plot_html` API. The `freq` parameter now
  controls whether to create stacked proportion/percentage plots
  (`freq=FALSE`, default) or dodged frequency plots (`freq=TRUE`)
- Added `docx_return_as_mschart` parameter to
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) to
  control return type for `cat_plot_docx`. When `TRUE`, returns the
  mschart object directly instead of embedding it in an rdocx document,
  enabling more flexible composition of Word documents
- **Refactored
  [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.md)
  to S3 generic**: Now supports both `ggplot2` objects (from
  `type="cat_plot_html"`) and `mschart` objects (from
  `type="cat_plot_docx", docx_return_as_mschart=TRUE`). This enables
  automatic height calculation for Word charts, making it easier to size
  charts consistently across output formats

### Bug Fixes

- Fixed `colour_palette` parameter not being applied in `cat_plot_docx`
  output. The mschart library requires colour palettes to be named
  vectors where names match category levels, and the length must match
  the number of categories. The function now: (1) names the colour
  palette vector with category levels, (2) expands the palette with
  generated colours if user provides fewer colours than categories,
  and (3) trims the palette if user provides more colours than needed
- Fixed
  [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.md)
  where `hide_axis_text_if_single_variable=TRUE` was paradoxically
  giving larger heights than `FALSE`. Now correctly detects when axis
  text has been hidden (via `.variable_label_original` column) and
  applies a reduction factor (default 0.6 via
  `multiplier_hide_axis_single_var` parameter) plus reduced minimum
  height, resulting in appropriately smaller plots when axis text is
  hidden
- Fixed
  [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.md)
  to properly handle `int_plot_html` plots with independent variables.
  The function now forwards to
  [`fig_height_h_barchart()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart.md)
  with appropriate parameters for interval plots, returning the `max`
  parameter value (default 12) while allowing user customization,
  instead of erroring with “only supports a single indep variable”
- Fixed
  [`n_range2()`](https://nifu-no.github.io/saros/reference/n_range2.md)
  for `int_plot_html` plots to report N range per dependent variable
  instead of total count across all variables. Now correctly calculates
  sample size separately for each variable and reports the range (e.g.,
  \[250-299\] when variables have different amounts of missing data)
- Fixed `guess_legend_ncols()` in
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) to
  properly handle fill aesthetics using expressions like
  `fill = factor(cyl)`. The function now uses
  [`get_fill_levels()`](https://nifu-no.github.io/saros/reference/get_fill_levels.md)
  to evaluate expressions in data context instead of direct column
  access, preventing “no non-missing arguments to max; returning -Inf”
  warnings

### Internal Improvements

- **Major refactoring of validation infrastructure** (implements
  refactoring opportunities
  [\#1](https://github.com/NIFU-NO/saros/issues/1) and
  [\#4](https://github.com/NIFU-NO/saros/issues/4)):
  - All validation functions now use consistent `validate_*` prefix for
    better discoverability via auto-complete (e.g.,
    [`validate_bool()`](https://nifu-no.github.io/saros/reference/validate_bool.md),
    [`validate_integerish()`](https://nifu-no.github.io/saros/reference/validate_integerish.md),
    [`validate_double()`](https://nifu-no.github.io/saros/reference/validate_double.md),
    [`validate_string()`](https://nifu-no.github.io/saros/reference/validate_string.md))
  - Added
    [`validate_params()`](https://nifu-no.github.io/saros/reference/validate_params.md)
    helper function that consolidates multiple parameter validations
    into a single declarative call, reducing code duplication from 30+
    lines to ~5 lines in
    [`fig_height_h_barchart()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart.md)
  - Created validation rule builders
    ([`validate_integerish_rule()`](https://nifu-no.github.io/saros/reference/validate_integerish_rule.md),
    [`validate_double_rule()`](https://nifu-no.github.io/saros/reference/validate_double_rule.md),
    [`validate_bool_rule()`](https://nifu-no.github.io/saros/reference/validate_bool_rule.md),
    [`validate_string_rule()`](https://nifu-no.github.io/saros/reference/validate_string_rule.md))
    for more expressive and maintainable validation specifications in
    `validate_makeme_options()`
  - Maintained backwards compatibility with `check_*` aliases for
    existing code
  - Improved code maintainability and readability across
    validation-heavy functions
- Added
  [`is_int_plot_html()`](https://nifu-no.github.io/saros/reference/is_int_plot_html.md)
  helper function to centralize detection logic for int_plot_html data
  structures, improving code maintainability and consistency across
  [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.md)
  and [`n_rng2()`](https://nifu-no.github.io/saros/reference/n_rng2.md)

## saros 1.6.1

CRAN release: 2026-01-28

### New Features

- Added `quiet` parameter to
  [`global_settings_reset()`](https://nifu-no.github.io/saros/reference/global_settings_reset.md)
  to optionally suppress informational messages when resetting global
  settings to package defaults
- **Addressed issue
  [\#510](https://github.com/NIFU-NO/saros/issues/510)**:
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) now
  defaults to `type = "auto"` which intelligently detects the
  appropriate output type based on dependent variable classes:
  - Numeric variables -\> `int_plot_html`
  - Single character variable -\> `chr_table_html`
  - Factor/ordered or multiple character variables -\> `cat_plot_html`
  - Unsupported types (Date, POSIXct, POSIXt, list, complex) produce
    clear error messages identifying the problematic variables
  - This eliminates the uninformative “arguments must have same length”
    error when accidentally providing numeric variables without
    specifying type. Mixed variable types produce a clear error message
    suggesting the correct type to use

### Bug Fixes

- Fixed issue [\#518](https://github.com/NIFU-NO/saros/issues/518) where
  [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  with `save = NULL` produced cryptic “object ‘caption’ not found”
  error. Added proper validation to ensure `save` parameter is a single
  logical value (TRUE or FALSE)
- Fixed
  [`txt_from_cat_mesos_plots()`](https://nifu-no.github.io/saros/reference/txt_from_cat_mesos_plots.md)
  where second group (others) proportions incorrectly became zero.
  Refactored to process each variable separately, ensuring both groups’
  proportions are correctly calculated per variable
- Fixed
  [`txt_from_cat_mesos_plots()`](https://nifu-no.github.io/saros/reference/txt_from_cat_mesos_plots.md)
  where `n_highest_categories=2` with binary (2-category) variables
  summed all categories to 1.0, producing uninformative results. Now
  only applies `n_highest_categories` when the variable has more
  categories than the threshold, otherwise uses only the single
  highest/lowest category
- Fixed issue [\#511](https://github.com/NIFU-NO/saros/issues/511) where
  `x_axis_label_width` parameter had no effect in `int_plot_html` when
  no independent variable was present. The
  [`apply_label_wrapping()`](https://nifu-no.github.io/saros/reference/apply_label_wrapping.md)
  function now correctly wraps `.variable_label` when
  `indep_length == 0`
- Fixed issue [\#512](https://github.com/NIFU-NO/saros/issues/512) where
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) with
  multiple crowds produced identical plots instead of crowd-specific
  filtered data. The
  [`process_crowd_data()`](https://nifu-no.github.io/saros/reference/process_crowd_data.md)
  function now correctly passes filtered `subset_data` to
  [`make_content()`](https://nifu-no.github.io/saros/reference/make_content.md)
  for each crowd, ensuring each plot displays statistics computed from
  only that crowd’s data subset
- Fixed
  [`txt_from_cat_mesos_plots()`](https://nifu-no.github.io/saros/reference/txt_from_cat_mesos_plots.md)
  to handle cases where `.category_order` contains NA values by using
  `na.rm = TRUE` when calculating max values, preventing “NA/NaN
  argument” errors
- Fixed
  [`n_rng2()`](https://nifu-no.github.io/saros/reference/n_rng2.md) to
  correctly calculate sample sizes for `int_plot_html` plots by only
  checking complete cases on relevant variables (`.value` and
  independent variables) instead of all columns in the dataset. Also
  modified `make_content.int_plot_html()` to only include necessary
  columns in the plot object, reducing memory footprint

## saros 1.6.0

CRAN release: 2025-11-10

### Breaking Changes

- [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  returns an empty data.frame instead of `NULL` when no plot or table
  can be created, simplifying downstream code (e.g. `gt::gt()` fails if
  served `NULL`)
- Resolved issue [\#372](https://github.com/NIFU-NO/saros/issues/372) -
  `descend` parameter now works correctly with ordered factors while
  preserving their inherent level ordering. Ordered factors maintain
  their natural order as the base, but `descend` can reverse the display
  order
- Enhanced `custom_palette()` to allow `priority_palette_codes` to
  supplement base palette when insufficient colors are available
- Improved color palette handling for explicit NA factor levels when
  using `showNA = "always"`

### New Features

#### New Functions and Output Types

- **New output type:** `makeme(type = "int_plot_html")` for interactive
  interval plots with violin and box plots
- **New function**:
  [`txt_from_cat_mesos_plots()`](https://nifu-no.github.io/saros/reference/txt_from_cat_mesos_plots.md)
  to generate textual summaries from two categorical mesos-scale plots,
  highlighting significant differences in selected categories between
  groups. Supports global settings via
  [`global_settings_set()`](https://nifu-no.github.io/saros/reference/global_settings_set.md)
  (closes [\#506](https://github.com/NIFU-NO/saros/issues/506))
- **New function**:
  [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  to convert a list of ggplot2 objects (typically from
  `makeme(crowd = ...)`) into Quarto tabsets with automatic height
  calculation and optional download links. Supports both categorical and
  interval plots with intelligent auto-detection based on plot layers
- **New exported function**:
  [`get_fig_title_suffix_from_ggplot()`](https://nifu-no.github.io/saros/reference/get_fig_title_suffix_from_ggplot.md)
  generates figure title suffixes with N range and optional download
  links for plots

#### Enhanced Functionality

- Added support for `data_label = "mean"` and `data_label = "median"` in
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) for
  `type = "cat_*_html"` outputs (addresses issue
  [\#460](https://github.com/NIFU-NO/saros/issues/460))
- Added `data_label_position` argument to
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  allowing data labels to be positioned at “center”, “bottom”, “top”, or
  “above” bars in categorical plots (addresses issue
  [\#365](https://github.com/NIFU-NO/saros/issues/365))
- Enhanced `chr_table_html` to support multiple independent variables
  for displaying background context with open-ended text responses
- Added `na_colour` parameter to
  [`hex_bw()`](https://nifu-no.github.io/saros/reference/hex_bw.md)
  function to allow customization of text color for NA fills (default:
  “#ffffff”)
- Added `colour_2nd_binary_cat` parameter to
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) for
  checkbox plots - when set with `checked`/`not_checked`, reverses
  category order so the second category receives the specified color

### Sorting Improvements

- Independent-variable ordering is now computed per dependent variable,
  allowing indep order to vary per dep; `descend_indep` now consistently
  reverses indep order across tables and plots; plots use centralized
  `.indep_order` when indep is on the x-axis; ordered indep factors take
  precedence over `sort_indep_by` (reversed only when
  `descend_indep=TRUE`); legends preserve unused response levels in
  `.category`
- Implemented B1 strategy for direct column-based sorting via
  whitelists. Allowed keys for dependent variables are now centrally
  defined and enforced; independent variables use a similar whitelist
  (including `.count_total_indep`). This prevents accidental sorting on
  arbitrary or missing columns and clarifies supported behavior
- `sort_indep_by` now explicitly defaults to `".factor_order"` and
  accepts `NULL` (treated as `".factor_order"`). When no independent
  variable is provided (`indep = NULL`), specifying `sort_indep_by` or
  `descend_indep` no longer errors; they are simply ignored
- Improved sorting stability for dependent variables with
  missing/duplicate labels — internal joins for computing `.dep_order`
  now key by `.variable_name` (unique) instead of `.variable_label`,
  preventing many-to-one join errors in edge cases while preserving
  displayed labels

### Bug Fixes & Improvements

#### Critical Fixes

- **CRITICAL:** Resolved bug in
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  where combinations of valid factor variables with all-NA factor
  variables incorrectly threw “mix of categorical and continuous
  variables” error. Variable type checking now uses filtered variable
  lists instead of original lists, preventing premature type validation
  errors
- Resolved issue [\#464](https://github.com/NIFU-NO/saros/issues/464) -
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  failures for sigtest_table when dep and indep variables overlap. Now
  automatically excludes indep variables from dep selection to prevent
  conflicts

#### Robustness Improvements

- Added vector length validation in
  [`get_fig_title_suffix_from_ggplot()`](https://nifu-no.github.io/saros/reference/get_fig_title_suffix_from_ggplot.md)
  to prevent subscript-out-of-bounds errors when vectorized parameters
  have mismatched lengths
- [`n_rng2()`](https://nifu-no.github.io/saros/reference/n_rng2.md) now
  gracefully handles plots not created by
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) by
  calculating N from complete cases instead of aborting, and properly
  handles all-NA and empty `.count_per_indep_group` values by returning
  “0” with clear warnings instead of producing infinite values
- [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  now validates each plot individually in the loop, preventing invalid
  objects from causing failures
- Improved empty data frame handling throughout - using
  [`is.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`nrow()`](https://rdrr.io/r/base/nrow.html) checks instead of
  [`length()`](https://rdrr.io/r/base/length.html) for more reliable
  validation

#### Other Fixes

- Resolved faceting issue in `int_plot_html` where
  `label_separator = NULL` with independent variables caused
  violin/boxplot and label geoms to appear in separate facets due to
  inconsistent string wrapping
- Removed unnecessary “multiple main questions” warning when using
  `label_separator = NULL`, as having different main questions is
  expected behavior
- Corrected double NA check logic in `check_bool()` function - removed
  redundant condition that made validation always pass for NA values
- Improved NULL and NA handling in `glue_together_range()` to prevent
  edge case failures with empty or invalid data ranges
- Improved robustness of `check_no_duplicated_label_suffix()` to handle
  empty data frames and missing columns gracefully
- Enhanced `check_sort_by()` validation to properly handle empty
  character vectors with clear error messages
- Improved `keep_subitem()` to handle character inputs and use factor
  levels for better NA handling
- Simplified
  [`arrange_table_data()`](https://nifu-no.github.io/saros/reference/arrange_table_data.md)
  sorting logic for better reliability
- Updated documentation reference from
  [`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  to
  [`ggplot2::set_theme()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  due to ggplot2 4.0.0
- Fixed `custom_palette()` in
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) to
  properly handle unnamed elements in `priority_palette_codes` as colors
  for “NA” category. Previously, unnamed elements were filtered out,
  causing NA categories to receive no color assignment (appearing as
  white fill with white text)

### Performance Enhancements

- Optimized
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  examples for 73.8% faster execution (6.6s → 1.7s total). Reduced
  variable counts and crowd configurations while maintaining educational
  value
- Updated
  [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.md)
  example for consistency with optimized examples
- Completely rewrote the `.spread` algorithm in
  [`subset_vector()`](https://nifu-no.github.io/saros/reference/subset_vector.md)
  for better spread maximization using evenly spaced positions

### Internal Refactoring

- Substantially modularized internal implementation of
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) into
  focused helper functions (argument setup, crowd processing, output
  assembly, validation). Improves readability, testability, and
  robustness without changing public API (closes
  [\#368](https://github.com/NIFU-NO/saros/issues/368))
- Split
  [`post_process_makeme_data()`](https://nifu-no.github.io/saros/reference/post_process_makeme_data.md)
  into focused single-responsibility functions:
  [`process_indep_factor_levels()`](https://nifu-no.github.io/saros/reference/process_indep_factor_levels.md)
  for general factor reversal and
  [`process_binary_category_colors()`](https://nifu-no.github.io/saros/reference/process_binary_category_colors.md)
  for cat_plot_html-specific binary category processing
- Moved
  [`process_binary_category_colors()`](https://nifu-no.github.io/saros/reference/process_binary_category_colors.md)
  logic from `make_content.cat_plot_html()` to
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md),
  consolidating all color-related logic in one place
- Refactored checkbox plot handling in
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) to
  integrate `colour_2nd_binary_cat` parameter with
  `convert_to_checkbox_plot()` function
- Modularized tabular I/O functionality - renamed `pretty_tabular.R` to
  `tabular_write.R` and extracted
  [`tabular_read()`](https://nifu-no.github.io/saros/reference/tabular_read.md)
  function into separate file
- Centralized global constants (sorting whitelists) in `zzz.R` under
  `.saros.env` for easier maintenance
- Extracted duplicate label wrapping logic into
  [`apply_label_wrapping()`](https://nifu-no.github.io/saros/reference/apply_label_wrapping.md)
  helper function, improving maintainability and consistency
- Added explicit return statement in `make_content.int_plot_html()` for
  clarity
- Added `.onUnload()` function to clean up global options when package
  is unloaded, preventing option pollution in user’s R session
- Using `air` for consistent code formatting
- **CRAN Compliance:** Reduced string lengths in documentation for
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) and
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  functions to meet CRAN’s character limit per line requirement

### Development & Testing

- Added comprehensive test coverage for
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  helper functions with full roxygen2 documentation and
  [@keywords](https://github.com/keywords) internal annotation for
  internal API clarity
- Added comprehensive visual regression testing for `int_plot_html`
  using vdiffr snapshot tests covering various scenarios
- Added comprehensive test coverage for utility validation functions and
  edge cases including all-NA variables
- Added test coverage for global settings support in
  [`txt_from_cat_mesos_plots()`](https://nifu-no.github.io/saros/reference/txt_from_cat_mesos_plots.md)
  with proper cleanup using
  [`withr::defer()`](https://withr.r-lib.org/reference/defer.html)
- Added VS Code configuration for improved development experience
- Added `survey` package to Suggests for enhanced testing capabilities
- Updated build ignore patterns for coverage reports and library files

## saros 1.5.4

CRAN release: 2025-06-04

- Fix: Corrected error introduced in 1.5.3

## saros 1.5.3

CRAN release: 2025-06-02

- Fix: `makeme` now handles arguments in parent frame scopes correctly.
- Fix: internal function `get_common_levels` now works with no provided
  `col_pos`.

## saros 1.5.1

CRAN release: 2025-02-12

- Feature: Added `".percentage"` as valid (sorting) column
- Feature: `cat_table_html` returns NULL if nothing to display
- Fix: `scale_x_reordered` now only sorts when needed
- Fix: `cat_table_html` takes showNA properly
- Minor: refactoring of `cat_plot_html`

## saros 1.5.0

CRAN release: 2025-01-10

### Major changes

- Lots of settings for
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
  useful for creating plots comparing a group with all other groups
  combined, or everyone.
- [`fig_height_h_barchart()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart.md)
  finally works adequately for 90 percent of figure combinations.
- Global settings inheritance for
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md),
  [`make_link()`](https://nifu-no.github.io/saros/reference/make_link.md),
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md),
  [`n_range()`](https://nifu-no.github.io/saros/reference/n_range.md),
  [`n_range2()`](https://nifu-no.github.io/saros/reference/n_range2.md)
- New function `makeme(type="int_plot_html")` for violin plots

### Minor changes

- `makeme(type="cat_plot_html")` no longer shows a line one the y axis
  for single variable plots.
- Refactored `crosstable.data.frame()` and `crosstable.survey()`
  methods.
- Many more tests.
- Countless bugfixes.

## saros 1.2.0

CRAN release: 2024-09-03

### Major changes

- `embed_*`-functions are replaced by `makeme(type="cat_plot")` etc
  - [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
    takes S3-methods which eases expansions.
  - `embed_*` are thus lifecycle soft-deprecated.

### New features

- [`n_range()`](https://nifu-no.github.io/saros/reference/n_range.md)
  gives the n_range for a given dataset, dependent and independent
  variables.
- [`n_range2()`](https://nifu-no.github.io/saros/reference/n_range2.md)
  same as
  [`n_range()`](https://nifu-no.github.io/saros/reference/n_range.md)
  but takes a
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)-object
  for convenience.
- [`make_link()`](https://nifu-no.github.io/saros/reference/make_link.md)
  for generating downloadable figures, tables, data or anything else, on
  the spot.
- [`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md) is
  a minor wrapper to ease using make_link with ggplot2::ggsave().
- [`fig_height_h_barchart()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart.md)
  estimates the optimal figure height for a horizontal barchart.
- [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.md)
  same as above, but takes a makeme()-object for convenience.
- `makeme(type="cat_plot_html")` allows sorting within each dependent
  variable-by-independent variable (facet-sort)

### Bugfixes

- Too many to list, but see saros 1.1.0 for some of them.

## saros 1.1.0

-feat: Removed colour_palette
([\#328](https://github.com/NIFU-NO/saros/issues/328)) -fix: Argument
`chapter_qmd_start_section_filepath` now has effect.
([\#321](https://github.com/NIFU-NO/saros/issues/321)) -fix: simplified
`create_email_credentials()`
([\#305](https://github.com/NIFU-NO/saros/issues/305)) -fix: pkgdown for
recode_checkbox_sets
([\#304](https://github.com/NIFU-NO/saros/issues/304)) -fix:
cat\_\*\_plot now displays keys for unused categories in legend
([\#301](https://github.com/NIFU-NO/saros/issues/301)) -fix:
`single_y_bivariates_if_deps_above()` no longer affects univariates
([\#300](https://github.com/NIFU-NO/saros/issues/300)) -fix: More robust
estimate_plot_height if dep only has one NA category
([\#299](https://github.com/NIFU-NO/saros/issues/299)) -fix:
Significance test now works in edge cases
([\#297](https://github.com/NIFU-NO/saros/issues/297)) -feat: chr_table
now ignores NA and empty strings.
([\#296](https://github.com/NIFU-NO/saros/issues/296)) -fix:
`serialized_format` now actually supports `{qs}`, if installed.
([\#293](https://github.com/NIFU-NO/saros/issues/293)) -feat: Added
default to replace_heading_for_group
([\#291](https://github.com/NIFU-NO/saros/issues/291)) -feat: Removed
flexi-app ([\#289](https://github.com/NIFU-NO/saros/issues/289)) -feat:
Removed unnecessary rendering-assistance tools.
([\#287](https://github.com/NIFU-NO/saros/issues/287)) -feat:
`gen_qmd_index` has now been refactored
([\#286](https://github.com/NIFU-NO/saros/issues/286)) - simplified to
`gen_qmd_file`, which better handles index and report - Also allows
separate qmd_snippets at start and end for index and - Extract formats
from report_header_yaml which is used to create links -fix:
insert_obj_in_qmd now uses conv_to_valid_obj_name() again
([\#284](https://github.com/NIFU-NO/saros/issues/284)) -feat: Removed
saving png files as it is rarely used.
([\#283](https://github.com/NIFU-NO/saros/issues/283)) -fix:
`crosstable3()` now handles character vectors.
([\#278](https://github.com/NIFU-NO/saros/issues/278))

## saros 1.0.5

## saros 1.0.3

- Added more arguments for plot text sizes, with more useful defaults
  for Word and HTML layouts.
- `attach_qualtrics_labels` is now more flexible.
- Minor fix in `sanitize_labels`.
- Internal change to accommodate ggplot2 3.5.0.
- Improved performance for argument validation checks.
- `setup_access_restrictions` now accepts NULL password input.
- `setup_access_restrictions` now can warn instead of error if
  `rel_parent_path` does not exist.
- `sort_by` now correctly sorts with `descend=TRUE` for “.upper”, etc.
- Fixed `list_available_element_types`
- Set one long-running test for `draft_report` to only run on
  maintainer’s local computer.
- `sanitize_labels` now has option to translate ASCII HTML-escaped
  characters to unicode.

## saros 1.0.1

CRAN release: 2024-01-26

- Minor fixes for CRAN manual review

## saros 1.0.0

- Submitted to CRAN

## saros 0.9.1

- First version ready
