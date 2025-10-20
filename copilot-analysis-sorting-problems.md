# Analysis: Sorting Features in saros Package

**Date**: October 17, 2025  
**Branch**: fix/improve-sorting-functionality  
**File**: R/sorting_utils.R

## Overview

This document analyzes the current implementation of `sort_dep_by` and `sort_indep_by` features in the makeme function, identifying improvement areas to ensure all kinds of sorting are possible and correct.

<!-- Implemented status details removed to keep this document focused on pending work and planning. -->

## Key Improvement Areas (Remaining)

### 1. Missing Sorting Options [MEDIUM PRIORITY]

**Currently Supported**:
- ✅ Positional: `.top`, `.bottom`, `.upper`, `.lower`, `.mid_upper`, `.mid_lower`
- ✅ Column-based: `.count`, `.proportion`, `.mean`, `.median`, `.sum_value`
- ✅ Category-based: Single category (e.g., "Not at all") or multiple categories
- ✅ Alphabetical: `.variable_label`, `.variable_name`
- ✅ Original order: `.variable_position`

**Potentially Missing**:
- ❓ `.count_per_indep_group` - partially implemented but may need testing
- ❓ Custom sorting by other columns in the data
- ❓ Sorting by statistical measures (variance, std dev, range, etc.)

**Proposed Solution**:
- Audit current test coverage to identify which options are fully tested
- Document which options are supported and provide examples
- Consider adding statistical measures if there's user demand

**Impact**: Improves feature completeness, clarifies capabilities

---

## Options Analysis: Key Improvement Area 1 — “Potentially Missing” (Open)

Goal: Decide scope, API details, and validation/ordering semantics for the missing sorting options before any implementation.

Scope candidates (from above):
- A) .count_per_indep_group (totals-based ordering of independent categories)
- B) Custom sorting by other columns in the data
- C) Statistical measures (variance, std dev, range, IQR)

General contract (for all new sort modes):
- Inputs: dep (one or more categorical variables), optional indep (categorical), sort_dep_by, sort_indep_by, descend/descend_indep
- Outputs: explicit order columns (.dep_order and/or .indep_order) computed in a single, stable pass using arrange_with_order(); special-case precedence for ordered indep factors remains intact.
- Error modes: clear cli errors if requested column(s)/measure(s) not available or inapplicable with provided data; NA handling must be defined.
- Success criteria: deterministic order with documented tie-breaks; compatible with showNA handling; does not alter existing public defaults.

Cross-cutting behaviors to preserve:
- Ordered indep factors take precedence over sort_indep_by where documented (.factor_order path).
- descend/descend_indep only change order direction; they do not change the chosen metric.
- NA handling: exclude NAs from metric computations unless the metric is explicitly about NA share; still show row/column as per showNA setting.
- Ties: break using (1) .variable_position for deps, factor-level for indep if applicable, else lexical by label, then by name — document this order.

— A) .count_per_indep_group (Open)
Definition: For each indep category (row), compute total count across all selected deps (or across the dataset when dep is length 1) and order indep by that total.

Design options:
- A1. Use existing aggregate .count_total_indep if present; otherwise compute a per-indep group total within the indep-ordering stage.
- A2. Scope of total:
  - A2a. Sum across all dep variables included in the call (cat_table_html path): ensures row-stable ordering regardless of which dep section is displayed.
  - A2b. Sum within each dep independently: would vary by dep and is not compatible with a unified row order; reject for tables with multiple deps.

Recommendation: A1 + A2a. Ensure a single per-indep total (row-wise) available wherever indep ordering executes. Name: .count_total_indep. If not available, compute in add_indep_order prerequisites.

Edge cases:
- indep = NULL: mode is ignored, no error.
- No dep or a single dep: still compute totals across that single dep’s rows; order is well-defined.
- All NA in relevant cells: treat NA as zero for totals; document.
- Ties: break by indep factor order, then label, then name.

Tests to add:
- Single dep vs multiple deps produce same indep order when using .count_per_indep_group.
- descend_indep reverses the indep order based on totals.
- Ties across two indep categories fall back to factor level order.

— B) Custom sorting by other columns (Implement B1 now; keep room for B2 later)
Definition: Allow sort_dep_by/sort_indep_by to reference arbitrary computed columns (present in the pre-render data).

Design options:
- B1. Whitelist: only allow a documented set of safe columns (e.g., .count, .proportion, .sum_value, .mean, .median, .count_total_indep, etc.).
- B2. Open reference: accept any column name string; validate existence and numeric/ordering compatibility.

Recommendation: Implement B1 now (whitelist), because schema differs by path (table vs plot) and arbitrary columns could be missing. Keep room to support B2 later under the hood (no UX exposure yet) once validation and schema guarantees are stronger.

Validation:
- validate_sort_column(data, column_name) to fail fast if missing.
- For non-numeric columns, use lexical ordering with a warning unless explicitly allowed (e.g., .variable_label).

Tests to add:
- Attempt sorting on a missing column -> cli error with helpful message.
- Sorting on a non-numeric column outside the allowed set -> warning or error per policy.

— C) Statistical measures (variance, std dev, range) (Open)
Definition: Compute variability metrics across categories or across indep groups to rank deps (typical use) or indep (less typical).

Design options:
- C1. Per-dep variability across its categories (e.g., range of category proportions), then order deps by that metric.
- C2. Per-dep variability across indep groups for a target category or combined categories.

Recommendation: Start with C1 because it does not require indep and is interpretable for cat_table_html; explicitly define which measure is computed (range by default; optionally std dev later).

Details:
- Specify which categories are included (all non-NA) and whether they’re weighted by counts. Initial version: unweighted, on proportions.
- desc behavior: higher variability first when descend = TRUE.

Tests to add:
- Synthetic data with known ranges verifies ordering.
- Ties fall back to label/name tie-breaks.


Proposed acceptance criteria (phase 1):
- Implement A) .count_per_indep_group for indep ordering in cat_table_html path using .count_total_indep with documented tie-breaks; add tests; no change to public defaults.
- Add validation helpers for missing columns/categories and wire them to fail fast for B–D.
- Document scope and examples for A) and planned B–D in the vignette; postpone implementation of B–D pending feedback.

Open questions for maintainers (carry over to GH Issues):
- Should .count_total_indep be materialized upstream (e.g., in summarize_cat_cat_data) to keep ordering helpers pure, or computed on demand inside add_indep_order?
- For C) and D), do we foresee plot variants requiring the same metrics? If yes, should we add a shared metrics module to avoid duplication?
- Preferred default target category when none is specified for D)?

Next (planning only; no code yet):
- Decide A1/A2 materialization location and finalize the whitelist for B1.
- Draft test skeletons (skipped) to lock expected behavior before implementation.
---

### 2. Default Sorting Behavior Not Clear

(Implementation details removed; revisit if behavior changes in future.)

---

### 3. Descending Logic Consistency

(Implementation details removed; revisit if behavior changes in future.)

---

### 4. Error Handling and Validation [MEDIUM PRIORITY]

**Missing Validation**:
- No check that requested sorting columns exist
- No check that requested categories exist in the data
- No clear error messages when sorting fails
- No validation of data types

**Proposed Validation**:
```r
validate_sort_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    cli::cli_abort("Column {.field {column_name}} not found in data")
  }
}

validate_sort_category <- function(data, category_value) {
  if (!category_value %in% levels(data$.category)) {
    available <- paste(levels(data$.category), collapse = ", ")
    cli::cli_abort(
      "Category {.val {category_value}} not found. Available: {available}"
    )
  }
}
```

**Impact**: Better user experience, easier debugging

---

### 5. Testing Coverage for Edge Cases [MEDIUM PRIORITY]

**Need to Ensure Tests Cover**:
- ✅ Single dependent variable (no sorting needed)
- ✅ Multiple dependent variables with various sorting options
- ✅ Independent variables with various sorting options
- ❓ Data with missing values (NA handling in sorting)
- ❓ Data with ties (multiple variables/categories with same values)
- ❓ Ordered vs unordered factors
- ❓ Combinations of sorting (e.g., `sort_dep_by = ".top"` + `sort_indep_by = "Not at all"`)
- ❓ Edge cases like single category, all NAs, etc.

**Proposed Action**:
- Audit current test suite for coverage
- Add tests for identified gaps
- Document expected behavior for edge cases

**Impact**: Increases reliability, catches regression bugs

---

### 6. Documentation Improvements [LOW PRIORITY]

**Needed Documentation**:
- Clear explanation of what each sorting option does
- Examples showing all supported sorting combinations
- Explanation of when to use each sorting method
- Behavior with missing values
- Interaction between sort_dep_by and sort_indep_by

**Proposed Action**:
- Add comprehensive examples to function documentation
- Create vignette explaining sorting options
- Add decision tree for choosing sorting method

**Impact**: Improves usability, reduces support burden

---

<!-- Implementation plan sections removed per request; this document now focuses on pending analysis and open options only. -->
