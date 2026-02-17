# saros 1.6.1.9000 (dev)

## New Features
-   Added `make_file_links()` function for dynamically creating markdown lists with links to files. Extracts document titles from DOCX, PPTX, and PDF file metadata and generates formatted markdown lists. Ideal for creating navigation links in Quarto/RMarkdown documents that point to generated reports in a folder. Supports glob patterns, recursive search, and customizable list formatting (unordered or numbered)
-   Enhanced `ggsaver()` to automatically apply colour palettes from `girafe()` global settings when saving plots. This ensures saved PNG/PDF images match the appearance of interactive plots displayed with `girafe()`. Palette settings can be configured via `global_settings_set(fn_name = "girafe", new = list(palette_codes = ...))` and will be automatically applied when saving plots through `get_fig_title_suffix_from_ggplot()` or direct `ggsaver()` calls
-   Added `folder` and `file_prefix` parameters to `get_fig_title_suffix_from_ggplot()` for controlling where files are saved and what prefix to use for filenames
-   Enhanced `get_fig_title_suffix_from_ggplot()` to support global settings inheritance via `global_settings_set()`, consistent with other saros functions like `make_link()` and `makeme()`

## Bug Fixes
-   Fixed `fig_height_h_barchart2()` to properly handle `int_plot_html` plots with independent variables. The function now forwards to `fig_height_h_barchart()` with appropriate parameters for interval plots, returning the `max` parameter value (default 12) while allowing user customization, instead of erroring with "only supports a single indep variable"
-   Fixed `n_range2()` for `int_plot_html` plots to report N range per dependent variable instead of total count across all variables. Now correctly calculates sample size separately for each variable and reports the range (e.g., [250-299] when variables have different amounts of missing data)
-   Fixed `guess_legend_ncols()` in `girafe()` to properly handle fill aesthetics using expressions like `fill = factor(cyl)`. The function now uses `get_fill_levels()` to evaluate expressions in data context instead of direct column access, preventing "no non-missing arguments to max; returning -Inf" warnings

## Internal Improvements
-   **Major refactoring of validation infrastructure** (implements refactoring opportunities #1 and #4):
    -   All validation functions now use consistent `validate_*` prefix for better discoverability via auto-complete (e.g., `validate_bool()`, `validate_integerish()`, `validate_double()`, `validate_string()`)
    -   Added `validate_params()` helper function that consolidates multiple parameter validations into a single declarative call, reducing code duplication from 30+ lines to ~5 lines in `fig_height_h_barchart()`
    -   Created validation rule builders (`validate_integerish_rule()`, `validate_double_rule()`, `validate_bool_rule()`, `validate_string_rule()`) for more expressive and maintainable validation specifications in `validate_makeme_options()`
    -   Maintained backwards compatibility with `check_*` aliases for existing code
    -   Improved code maintainability and readability across validation-heavy functions
-   Added `is_int_plot_html()` helper function to centralize detection logic for int_plot_html data structures, improving code maintainability and consistency across `fig_height_h_barchart2()` and `n_rng2()`


# saros 1.6.1

## New Features
-   Added `quiet` parameter to `global_settings_reset()` to optionally suppress informational messages when resetting global settings to package defaults
-   **Addressed issue #510**: `makeme()` now defaults to `type = "auto"` which intelligently detects the appropriate output type based on dependent variable classes:
    -   Numeric variables -> `int_plot_html`
    -   Single character variable -> `chr_table_html`
    -   Factor/ordered or multiple character variables -> `cat_plot_html`
    -   Unsupported types (Date, POSIXct, POSIXt, list, complex) produce clear error messages identifying the problematic variables
    -   This eliminates the uninformative "arguments must have same length" error when accidentally providing numeric variables without specifying type. Mixed variable types produce a clear error message suggesting the correct type to use

## Bug Fixes
-   Fixed issue #518 where `crowd_plots_as_tabset()` with `save = NULL` produced cryptic "object 'caption' not found" error. Added proper validation to ensure `save` parameter is a single logical value (TRUE or FALSE)
-   Fixed `txt_from_cat_mesos_plots()` where second group (others) proportions incorrectly became zero. Refactored to process each variable separately, ensuring both groups' proportions are correctly calculated per variable
-   Fixed `txt_from_cat_mesos_plots()` where `n_highest_categories=2` with binary (2-category) variables summed all categories to 1.0, producing uninformative results. Now only applies `n_highest_categories` when the variable has more categories than the threshold, otherwise uses only the single highest/lowest category
-   Fixed issue #511 where `x_axis_label_width` parameter had no effect in `int_plot_html` when no independent variable was present. The `apply_label_wrapping()` function now correctly wraps `.variable_label` when `indep_length == 0`
-   Fixed issue #512 where `makeme()` with multiple crowds produced identical plots instead of crowd-specific filtered data. The `process_crowd_data()` function now correctly passes filtered `subset_data` to `make_content()` for each crowd, ensuring each plot displays statistics computed from only that crowd's data subset
-   Fixed `txt_from_cat_mesos_plots()` to handle cases where `.category_order` contains NA values by using `na.rm = TRUE` when calculating max values, preventing "NA/NaN argument" errors
-   Fixed `n_rng2()` to correctly calculate sample sizes for `int_plot_html` plots by only checking complete cases on relevant variables (`.value` and independent variables) instead of all columns in the dataset. Also modified `make_content.int_plot_html()` to only include necessary columns in the plot object, reducing memory footprint

# saros 1.6.0

## Breaking Changes
-   `makeme()` returns an empty data.frame instead of `NULL` when no plot or table can be created, simplifying downstream code (e.g. `gt::gt()` fails if served `NULL`)
-   Resolved issue #372 - `descend` parameter now works correctly with ordered factors while preserving their inherent level ordering. Ordered factors maintain their natural order as the base, but `descend` can reverse the display order
-   Enhanced `custom_palette()` to allow `priority_palette_codes` to supplement base palette when insufficient colors are available
-   Improved color palette handling for explicit NA factor levels when using `showNA = "always"`

## New Features

### New Functions and Output Types
-   **New output type:** `makeme(type = "int_plot_html")` for interactive interval plots with violin and box plots
-   **New function**: `txt_from_cat_mesos_plots()` to generate textual summaries from two categorical mesos-scale plots, highlighting significant differences in selected categories between groups. Supports global settings via `global_settings_set()` (closes #506)
-   **New function**: `crowd_plots_as_tabset()` to convert a list of ggplot2 objects (typically from `makeme(crowd = ...)`) into Quarto tabsets with automatic height calculation and optional download links. Supports both categorical and interval plots with intelligent auto-detection based on plot layers
-   **New exported function**: `get_fig_title_suffix_from_ggplot()` generates figure title suffixes with N range and optional download links for plots

### Enhanced Functionality
-   Added support for `data_label = "mean"` and `data_label = "median"` in `makeme()` for `type = "cat_*_html"` outputs (addresses issue #460)
-   Added `data_label_position` argument to `makeme()` allowing data labels to be positioned at "center", "bottom", "top", or "above" bars in categorical plots (addresses issue #365)
-   Enhanced `chr_table_html` to support multiple independent variables for displaying background context with open-ended text responses
-   Added `na_colour` parameter to `hex_bw()` function to allow customization of text color for NA fills (default: "#ffffff")
-   Added `colour_2nd_binary_cat` parameter to `girafe()` for checkbox plots - when set with `checked`/`not_checked`, reverses category order so the second category receives the specified color

## Sorting Improvements
-   Independent-variable ordering is now computed per dependent variable, allowing indep order to vary per dep; `descend_indep` now consistently reverses indep order across tables and plots; plots use centralized `.indep_order` when indep is on the x-axis; ordered indep factors take precedence over `sort_indep_by` (reversed only when `descend_indep=TRUE`); legends preserve unused response levels in `.category`
-   Implemented B1 strategy for direct column-based sorting via whitelists. Allowed keys for dependent variables are now centrally defined and enforced; independent variables use a similar whitelist (including `.count_total_indep`). This prevents accidental sorting on arbitrary or missing columns and clarifies supported behavior
-   `sort_indep_by` now explicitly defaults to `".factor_order"` and accepts `NULL` (treated as `".factor_order"`). When no independent variable is provided (`indep = NULL`), specifying `sort_indep_by` or `descend_indep` no longer errors; they are simply ignored
-   Improved sorting stability for dependent variables with missing/duplicate labels — internal joins for computing `.dep_order` now key by `.variable_name` (unique) instead of `.variable_label`, preventing many-to-one join errors in edge cases while preserving displayed labels

## Bug Fixes & Improvements

### Critical Fixes
-   **CRITICAL:** Resolved bug in `makeme()` where combinations of valid factor variables with all-NA factor variables incorrectly threw "mix of categorical and continuous variables" error. Variable type checking now uses filtered variable lists instead of original lists, preventing premature type validation errors
-   Resolved issue #464 - `makeme()` failures for sigtest_table when dep and indep variables overlap. Now automatically excludes indep variables from dep selection to prevent conflicts

### Robustness Improvements
-   Added vector length validation in `get_fig_title_suffix_from_ggplot()` to prevent subscript-out-of-bounds errors when vectorized parameters have mismatched lengths
-   `n_rng2()` now gracefully handles plots not created by `makeme()` by calculating N from complete cases instead of aborting, and properly handles all-NA and empty `.count_per_indep_group` values by returning "0" with clear warnings instead of producing infinite values
-   `crowd_plots_as_tabset()` now validates each plot individually in the loop, preventing invalid objects from causing failures
-   Improved empty data frame handling throughout - using `is.data.frame()` and `nrow()` checks instead of `length()` for more reliable validation

### Other Fixes
-   Resolved faceting issue in `int_plot_html` where `label_separator = NULL` with independent variables caused violin/boxplot and label geoms to appear in separate facets due to inconsistent string wrapping
-   Removed unnecessary "multiple main questions" warning when using `label_separator = NULL`, as having different main questions is expected behavior
-   Corrected double NA check logic in `check_bool()` function - removed redundant condition that made validation always pass for NA values
-   Improved NULL and NA handling in `glue_together_range()` to prevent edge case failures with empty or invalid data ranges
-   Improved robustness of `check_no_duplicated_label_suffix()` to handle empty data frames and missing columns gracefully
-   Enhanced `check_sort_by()` validation to properly handle empty character vectors with clear error messages
-   Improved `keep_subitem()` to handle character inputs and use factor levels for better NA handling
-   Simplified `arrange_table_data()` sorting logic for better reliability
-   Updated documentation reference from `ggplot2::theme_set()` to `ggplot2::set_theme()` due to ggplot2 4.0.0
-   Fixed `custom_palette()` in `girafe()` to properly handle unnamed elements in `priority_palette_codes` as colors for "NA" category. Previously, unnamed elements were filtered out, causing NA categories to receive no color assignment (appearing as white fill with white text)

## Performance Enhancements
-   Optimized `makeme()` examples for 73.8% faster execution (6.6s → 1.7s total). Reduced variable counts and crowd configurations while maintaining educational value
-   Updated `fig_height_h_barchart2()` example for consistency with optimized examples
-   Completely rewrote the `.spread` algorithm in `subset_vector()` for better spread maximization using evenly spaced positions

## Internal Refactoring
-   Substantially modularized internal implementation of `makeme()` into focused helper functions (argument setup, crowd processing, output assembly, validation). Improves readability, testability, and robustness without changing public API (closes #368)
-   Split `post_process_makeme_data()` into focused single-responsibility functions: `process_indep_factor_levels()` for general factor reversal and `process_binary_category_colors()` for cat_plot_html-specific binary category processing
-   Moved `process_binary_category_colors()` logic from `make_content.cat_plot_html()` to `girafe()`, consolidating all color-related logic in one place
-   Refactored checkbox plot handling in `girafe()` to integrate `colour_2nd_binary_cat` parameter with `convert_to_checkbox_plot()` function
-   Modularized tabular I/O functionality - renamed `pretty_tabular.R` to `tabular_write.R` and extracted `tabular_read()` function into separate file
-   Centralized global constants (sorting whitelists) in `zzz.R` under `.saros.env` for easier maintenance
-   Extracted duplicate label wrapping logic into `apply_label_wrapping()` helper function, improving maintainability and consistency
-   Added explicit return statement in `make_content.int_plot_html()` for clarity
-   Added `.onUnload()` function to clean up global options when package is unloaded, preventing option pollution in user's R session
-   Using `air` for consistent code formatting
-   **CRAN Compliance:** Reduced string lengths in documentation for `girafe()` and `makeme()` functions to meet CRAN's character limit per line requirement

## Development & Testing
-   Added comprehensive test coverage for `makeme()` helper functions with full roxygen2 documentation and @keywords internal annotation for internal API clarity
-   Added comprehensive visual regression testing for `int_plot_html` using vdiffr snapshot tests covering various scenarios
-   Added comprehensive test coverage for utility validation functions and edge cases including all-NA variables
-   Added test coverage for global settings support in `txt_from_cat_mesos_plots()` with proper cleanup using `withr::defer()`
-   Added VS Code configuration for improved development experience
-   Added `survey` package to Suggests for enhanced testing capabilities
-   Updated build ignore patterns for coverage reports and library files


# saros 1.5.4
-   Fix: Corrected error introduced in 1.5.3

# saros 1.5.3
-   Fix: `makeme` now handles arguments in parent frame scopes correctly.
-   Fix: internal function `get_common_levels` now works with no provided `col_pos`.

# saros 1.5.1
-   Feature: Added `".percentage"` as valid (sorting) column
-   Feature: `cat_table_html` returns NULL if nothing to display
-   Fix: `scale_x_reordered` now only sorts when needed
-   Fix: `cat_table_html` takes showNA properly
-   Minor: refactoring of `cat_plot_html`

# saros 1.5.0

## Major changes
- Lots of settings for `makeme()` useful for creating plots comparing a group with all other groups combined, or everyone. 
- `fig_height_h_barchart()` finally works adequately for 90 percent of figure combinations.
- Global settings inheritance for `makeme()`, `make_link()`, `girafe()`, `n_range()`, `n_range2()`
- New function `makeme(type="int_plot_html")` for violin plots

## Minor changes
- `makeme(type="cat_plot_html")` no longer shows a line one the y axis for single variable plots.
- Refactored `crosstable.data.frame()` and `crosstable.survey()` methods.
- Many more tests.
- Countless bugfixes.

# saros 1.2.0

## Major changes
- `embed_*`-functions are replaced by `makeme(type="cat_plot")` etc
    - `makeme()` takes S3-methods which eases expansions.
    - `embed_*` are thus lifecycle soft-deprecated.

## New features
- `n_range()` gives the n_range for a given dataset, dependent and independent variables.
- `n_range2()` same as `n_range()` but takes a `makeme()`-object for convenience.
- `make_link()` for generating downloadable figures, tables, data or anything else, on the spot.
- `ggsaver()` is a minor wrapper to ease using make_link with ggplot2::ggsave().
- `fig_height_h_barchart()` estimates the optimal figure height for a horizontal barchart.
- `fig_height_h_barchart2()` same as above, but takes a makeme()-object for convenience.
- `makeme(type="cat_plot_html")` allows sorting within each dependent variable-by-independent variable (facet-sort)

## Bugfixes
- Too many to list, but see saros 1.1.0 for some of them.

# saros 1.1.0
<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

-feat: Removed colour_palette  (#328)
-fix: Argument `chapter_qmd_start_section_filepath` now has effect. (#321)
-fix: simplified `create_email_credentials()` (#305)
-fix: pkgdown for recode_checkbox_sets (#304)
-fix: cat_*_plot now displays keys for unused categories in legend (#301)
-fix: `single_y_bivariates_if_deps_above()` no longer affects univariates (#300)
-fix: More robust estimate_plot_height if dep only has one NA category (#299)
-fix: Significance test now works in edge cases (#297)
-feat: chr_table now ignores NA and empty strings. (#296)
-fix: `serialized_format` now actually supports `{qs}`, if installed. (#293)
-feat: Added default to replace_heading_for_group (#291)
-feat: Removed flexi-app  (#289)
-feat: Removed unnecessary rendering-assistance tools. (#287)
-feat: `gen_qmd_index` has now been refactored (#286)
- simplified to `gen_qmd_file`, which better handles index and report
- Also allows separate qmd_snippets at start and end for index and
- Extract formats from report_header_yaml which is used to create links
-fix: insert_obj_in_qmd now uses conv_to_valid_obj_name() again (#284)
-feat: Removed saving png files as it is rarely used. (#283)
-fix: `crosstable3()` now handles character vectors. (#278)


# saros 1.0.5

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# saros 1.0.3

- Added more arguments for plot text sizes, with more useful defaults for Word and HTML layouts.
- `attach_qualtrics_labels` is now more flexible.
- Minor fix in `sanitize_labels`.
- Internal change to accommodate ggplot2 3.5.0.
- Improved performance for argument validation checks.
- `setup_access_restrictions` now accepts NULL password input.
- `setup_access_restrictions` now can warn instead of error if `rel_parent_path` does not exist.
- `sort_by` now correctly sorts with `descend=TRUE` for ".upper", etc.
- Fixed `list_available_element_types`
- Set one long-running test for `draft_report` to only run on maintainer's local computer.
- `sanitize_labels` now has option to translate ASCII HTML-escaped characters to unicode.

# saros 1.0.1

* Minor fixes for CRAN manual review

# saros 1.0.0

* Submitted to CRAN

# saros 0.9.1

* First version ready
