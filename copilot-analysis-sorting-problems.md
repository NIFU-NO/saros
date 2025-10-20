# Analysis: Sorting Features in saros Package

**Date**: October 17, 2025  
**Branch**: fix/improve-sorting-functionality  
**File**: R/sorting_utils.R

## Overview

This document analyzes the current implementation of `sort_dep_by` and `sort_indep_by` features in the makeme function, identifying improvement areas to ensure all kinds of sorting are possible and correct.

## Status Update (Oct 19, 2025)

Completed and verified (tests and R CMD check passing):
- Replaced custom positional logic with `subset_vector()` via `get_target_categories()`
- Standardized aggregation rules:
  - `.count` (dep and indep) uses sum across groups
  - `.count_total_indep` uses sum
  - `.sum_value` uses sum
  - Other numeric/statistical columns retain prior semantics (e.g., mean/median where applicable)
- Removed redundant numeric conversions (`.proportion`, `.count` already numeric)
- Standardized descending logic for independent-variable sorting via `arrange_with_order()`

Pending/partial:
- Dependent-variable descending logic still uses `descend_if_descending()`; needs migration to `arrange_with_order()` for full consistency
- Default behavior for `sort_indep_by = NULL` remains implicit; consider explicit `.original` option
- Validation, documentation, and edge-case tests to be improved

---

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
- ❓ Sorting by difference between groups (e.g., gender gap)

**Proposed Solution**:
- Audit current test coverage to identify which options are fully tested
- Document which options are supported and provide examples
- Consider adding statistical measures if there's user demand

**Impact**: Improves feature completeness, clarifies capabilities

---

### 2. Default Sorting Behavior Not Clear [RESOLVED]

**Change**:
- `sort_indep_by` now explicitly defaults to `".factor_order"` in the public API.
- Passing `NULL` is accepted and treated as `".factor_order"`.
- If `indep = NULL`, specifying `sort_indep_by` or `descend_indep` is ignored (no errors).

**Impact**: Clear and predictable behavior; avoids errors when indep is not provided.

---

### 3. Descending Logic Consistency [RESOLVED]

Status (Oct 20, 2025):
- Dependent-variable ordering is now fully unified via `arrange_with_order()` across helpers:
  - `calculate_proportion_order()`, `calculate_multiple_category_order()`, `calculate_category_order()`, `calculate_column_order()`, and `calculate_sum_value_order()` thread `descend` and sort within the helper.
  - No post-hoc `descend_if_descending()` is applied for dependent ordering anymore.
- Independent-variable ordering remains unified; `arrange_with_order()` is used across indep helpers, while `descend_if_descending()` is intentionally retained only for the special `.factor_order`/ordered-factor precedence path.

Impact: Single, uniform approach for descending dependent-variable sorting reduces drift. The indep special-case preserves intended semantics for ordered factors.

Notes:
- Category-related helpers in `summarize_cat_cat_data()` now use `sort_dep_by` (not `sort_indep_by`), and when all deps are ordered, category collapsing/exception flipping is disabled by passing `sort_by = NULL` to those helpers.
- Public API: `sort_indep_by` defaults to `.factor_order`; `NULL` is accepted and treated as `.factor_order`. If `indep = NULL`, specifying `sort_indep_by`/`descend_indep` is ignored.

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

## Specific Refactoring Recommendations

### Updated Steps (focused on remaining work)

1) Migrate dependent-variable helpers to `arrange_with_order()`
- Add `descend` parameter to: `calculate_proportion_order`, `calculate_category_order`, `calculate_multiple_category_order`, `calculate_column_order` (already accepts), and `calculate_sum_value_order` (already accepts)
- Use `arrange_with_order()` internally instead of fixed ascending
- Remove `descend_if_descending()` from `add_dep_order()` once all callers migrated

2) Clarify default indep sorting behavior
- Add explicit `.original` (or `.factor_order`) option to preserve factor-level order
- Document that `NULL` means "no sorting" (keep incoming order)

3) Validation utilities
- `validate_sort_column(data, column)` and `validate_sort_category(data, value)` as proposed earlier
- Fail fast with clear cli errors

4) Tests and docs
- Add regression tests covering ties, NAs, ordered vs unordered factors, and combined dep+indep sorting
- Expand docs and vignette to document all sorting modes and their interactions

---

## Priority Order for Implementation

### Updated Priority Order

Phase 1:
1. Standardize descending logic for dependent variables via `arrange_with_order()`; remove `descend_if_descending()`

Phase 2:
2. Clarify/document default indep sorting (`NULL` vs `.original`)
3. Add validation helpers and integrate

Phase 3:
4. Edge-case tests for ties/NA/ordered factors and combined sorting
5. Documentation/vignette updates

---

## Implementation Strategy

### Iterative Approach
- Make ONE small change at a time
- Test after each change
- Commit when tests pass
- Document what changed and why

### Testing Protocol
After each change:
1. Run `devtools::test()` to ensure all tests pass
2. Run `devtools::check()` to catch any new issues
3. Manually test a few common use cases
4. Review code for any unintended side effects

### Risk Mitigation
- Keep changes small and focused
- Maintain backward compatibility
- Add tests before refactoring if coverage is insufficient
- Document any behavior changes

---

## Next Steps

1. Migrate dep helpers to `arrange_with_order(descend)` and drop `descend_if_descending()`
2. Add `.original` indep option; document `NULL` semantics
3. Introduce validation helpers and wire them into sorting entry points
4. Add edge-case tests and update docs/vignette

---

## Notes

- This analysis is based on the current state of R/sorting_utils.R
- Some issues may be partially addressed in other parts of the codebase
- User feedback and actual usage patterns should guide priority of feature additions
- Maintain focus on reliability and consistency over adding new features
