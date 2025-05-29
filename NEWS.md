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
