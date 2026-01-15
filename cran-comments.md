## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Windows 11 x64, R 4.5.2
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R (devel, release, oldrel)

## Package updates

This is an update to saros 1.6.1 with the following key changes:

### New Features
* **Intelligent type auto-detection (issue #510)**: `makeme()` now defaults to `type = "auto"` which automatically detects the appropriate output type based on dependent variable classes (numeric → `int_plot_html`, factor/character → `cat_plot_html`). This eliminates cryptic error messages and provides a better user experience.
* Added `quiet` parameter to `global_settings_reset()` to optionally suppress informational messages when resetting global settings to package defaults.

### Bug Fixes
* Fixed issue #518 where `crowd_plots_as_tabset()` with `save = NULL` produced cryptic "object 'caption' not found" error. Added proper validation to ensure `save` parameter is a single logical value.
* Fixed `txt_from_cat_mesos_plots()` where second group (others) proportions incorrectly became zero. Refactored to process each variable separately.
* Fixed `txt_from_cat_mesos_plots()` where `n_highest_categories=2` with binary variables summed all categories to 1.0, producing uninformative results.
* Fixed issue #511 where `x_axis_label_width` parameter had no effect in `int_plot_html` when no independent variable was present.
* Fixed issue #512 where `makeme()` with multiple crowds produced identical plots instead of crowd-specific filtered data.
* Fixed `txt_from_cat_mesos_plots()` to handle cases where `.category_order` contains NA values.
* Fixed `n_rng2()` to correctly calculate sample sizes for `int_plot_html` plots by only checking complete cases on relevant variables.
* Removed invalid relative URL from vignette for CRAN compliance.

### Testing
* Added comprehensive test coverage for auto-type detection with 11 new test scenarios.
* Added test coverage for `txt_from_cat_mesos_plots()` edge cases.
* All examples run successfully, all tests pass, and the package builds cleanly on multiple platforms.

## Downstream dependencies

There are no reverse dependencies for this package.

