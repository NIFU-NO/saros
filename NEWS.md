# saros 1.6.0

## Breaking Changes
-   **Major change:** `makeme()` returns an empty data.frame instead of `NULL` when no plot or table can be created, simplifying downstream code (e.g. `gt::gt()` fails if served `NULL`)
-   **Major change:** Resolved issue #372 - `descend` parameter now works correctly with ordered factors while preserving their inherent level ordering. Ordered factors maintain their natural order as the base, but `descend` can reverse the display order
-   Enhanced `custom_palette()` to allow `priority_palette_codes` to supplement base palette when insufficient colors are available
-   Improved color palette handling for explicit NA factor levels when using `showNA = "always"`

## New Features
-   **New output type:** `makeme(type = "int_plot_html")` for interactive interval plots with violin and box plots
-   **New function**: `txt_from_cat_mesos_plots()` to generate textual summaries from two categorical mesos-scale plots, highlighting significant differences in selected categories between groups.
-   **New function**: `crowd_plots_as_tabset()` to convert a list of ggplot2 objects (typically from `makeme(crowd = ...)`) into Quarto tabsets with automatic height calculation and optional download links. Supports both categorical and interval plots with intelligent auto-detection based on plot layers
-   **New exported function**: `get_fig_title_suffix_from_ggplot()` generates figure title suffixes with N range and optional download links for plots.
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


## Bug Fixes
-   **CRITICAL:** Resolved bug in `makeme()` where combinations of valid factor variables with all-NA factor variables incorrectly threw "mix of categorical and continuous variables" error. Variable type checking now uses filtered variable lists instead of original lists, preventing premature type validation errors
-   Resolved faceting issue in `int_plot_html` where `label_separator = NULL` with independent variables caused violin/boxplot and label geoms to appear in separate facets due to inconsistent string wrapping
-   Removed unnecessary "multiple main questions" warning when using `label_separator = NULL`, as having different main questions is expected behavior
-   Resolved issue #464 - `makeme()` failures for sigtest_table when dep and indep variables overlap. Now automatically excludes indep variables from dep selection to prevent conflicts
-   Corrected double NA check logic in `check_bool()` function - removed redundant condition that made validation always pass for NA values
-   Improved NULL and NA handling in `glue_together_range()` to prevent edge case failures with empty or invalid data ranges
-   Improved robustness of `check_no_duplicated_label_suffix()` to handle empty data frames and missing columns gracefully
-   Enhanced `check_sort_by()` validation to properly handle empty character vectors with clear error messages
-   Improved `keep_subitem()` to handle character inputs and use factor levels for better NA handling
-   Simplified `arrange_table_data()` sorting logic for better reliability
-   Updated documentation reference from `ggplot2::theme_set()` to `ggplot2::set_theme()` due to ggplot2 4.0.0
-   Fixed `custom_palette()` in `girafe()` to properly handle unnamed elements in `priority_palette_codes` as colors for "NA" category. Previously, unnamed elements were filtered out, causing NA categories to receive no color assignment (appearing as white fill with white text)
-   `n_rng2()` now gracefully handles plots not created by `makeme()` by calculating N from complete cases instead of aborting
-   Added handling for all-NA and empty `.count_per_indep_group` values, returning "0" with clear warnings instead of producing infinite values

## Performance Enhancements
-   Optimized `makeme()` examples for 73.8% faster execution (6.6s → 1.7s total). Reduced variable counts and crowd configurations while maintaining educational value
-   Updated `fig_height_h_barchart2()` example for consistency with optimized examples
-   Completely rewrote the `.spread` algorithm in `subset_vector()` for better spread maximization using evenly spaced positions
-   Added `.onUnload()` function to clean up global options when package is unloaded, preventing option pollution in user's R session

## Internal Refactoring
-   Moved `process_binary_category_colors()` logic from `make_content.cat_plot_html()` to `girafe()`, consolidating all color-related logic in one place
-   Refactored checkbox plot handling in `girafe()` to integrate `colour_2nd_binary_cat` parameter with `convert_to_checkbox_plot()` function
-   Substantially modularized internal implementation of `makeme()` into focused helper functions (argument setup, crowd processing, output assembly, validation). Improves readability, testability, and robustness without changing public API (closes #368)
-   Split `post_process_makeme_data()` into focused single-responsibility functions: `process_indep_factor_levels()` for general factor reversal and `process_binary_category_colors()` for cat_plot_html-specific binary category processing
-   Modularized tabular I/O functionality - renamed `pretty_tabular.R` to `tabular_write.R` and extracted `tabular_read()` function into separate file
-   Centralized global constants (sorting whitelists) in `zzz.R` under `.saros.env` for easier maintenance
-   Using `air` for consistent code formatting
-   **CRAN Compliance:** Reduced string lengths in documentation for `girafe()` and `makeme()` functions to meet CRAN's character limit per line requirement
-   Added explicit return statement in `make_content.int_plot_html()` for clarity
-   Extracted duplicate label wrapping logic into `apply_label_wrapping()` helper function, improving maintainability and consistency


## Development & Testing
-   Added comprehensive test coverage for `makeme()` helper functions with full roxygen2 documentation and @keywords internal annotation for internal API clarity
-   Added comprehensive visual regression testing for `int_plot_html` using vdiffr snapshot tests covering various scenarios
-   Added comprehensive test coverage for utility validation functions
-   Added comprehensive unit tests for `makeme()` variable type checking edge cases including all-NA variables
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
