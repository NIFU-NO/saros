# Analysis: Sorting Features in saros Package

**Date**: October 17, 2025  
**Branch**: fix/improve-sorting-functionality  
**File**: R/sorting_utils.R

## Overview

This document analyzes the current implementation of `sort_dep_by` and `sort_indep_by` features in the makeme function, identifying improvement areas to ensure all kinds of sorting are possible and correct.

## Key Improvement Areas

### 1. **Inconsistent Use of `subset_vector()` [HIGH PRIORITY]**

**Issue**: The `calculate_proportion_order()` and `calculate_indep_proportion_order()` functions contain duplicated complex logic for determining target categories (`.top`, `.bottom`, `.upper`, `.lower`, etc.)

**Current State**:
- Both functions manually implement if-else chains to determine which categories to include
- This logic is already implemented and tested in `subset_vector()` utility function
- Code duplication makes maintenance harder and increases risk of inconsistencies

**Proposed Solution**:
- Extract a `get_target_categories()` helper function that uses `subset_vector()`
- Replace manual if-else logic in both proportion ordering functions
- Ensures consistency and reduces code duplication

**Impact**: Reduces ~50 lines of duplicated code, improves maintainability

---

### 2. **Inconsistent Aggregation Methods [HIGH PRIORITY]**

**Issue**: Different functions use different aggregation approaches without clear documentation:
- `calculate_column_order()` uses `max()` for dependent variables
- `calculate_indep_column_order()` uses `mean()` for independent variables
- Some use `sum()`, others use `mean()` or `max()`

**Questions to Answer**:
1. Should sorting by `.count` use max, mean, or sum across groups?
2. Should this be consistent between dep and indep variables?
3. Should users be able to specify the aggregation method?

**Current Behavior**:
```r
# For dependent variables (calculate_column_order)
aggregated <- data |>
  dplyr::group_by(.data$.variable_name) |>
  dplyr::summarise(order_value = max(as.numeric(.data[[column_name]]), na.rm = TRUE))

# For independent variables (calculate_indep_column_order)  
aggregated <- data |>
  dplyr::group_by(.data[[indep_col]]) |>
  dplyr::summarise(order_value = mean(as.numeric(.data[[column_name]]), na.rm = TRUE))
```

**Proposed Solution**:
- Document the rationale for each aggregation method
- Standardize where appropriate
- Consider making aggregation method configurable if needed

**Impact**: Clarifies expected behavior, ensures consistency

---

### 3. **Missing Sorting Options [MEDIUM PRIORITY]**

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

### 4. **Default Sorting Behavior Not Clear [MEDIUM PRIORITY]**

**Issue**: When `sort_indep_by = NULL`, the behavior is unclear:
- Current: Uses default ordering (1 for all rows)
- Question: Should independent variables maintain their original factor order or be sorted alphabetically?

**Current Implementation**:
```r
if (is.null(sort_by)) {
  data$.indep_order <- 1L
  return(data)
}
```

**Proposed Solution**:
- Add explicit `.original` or `.factor_order` option to maintain original factor order
- Document default behavior clearly
- Consider whether NULL should mean "no sorting" or "use default ordering"

**Impact**: Clarifies API, improves user experience

---

### 5. **Character vs Numeric Conversion [MEDIUM PRIORITY]**

**Issue**: Inconsistent handling of numeric conversion:
- Some functions convert `.proportion` to numeric with `as.numeric()`
- Proportions stored as character strings with "%" symbols need consistent handling
- Different columns may require different conversion approaches

**Current State**:
```r
# Some functions do this:
order_value = as.numeric(gsub("%", "", .data[[column_name]]))

# Others do this:
order_value = as.numeric(.data[[column_name]])
```

**Proposed Solution**:
- Create centralized helper function: `as_numeric_safe(x)` that:
  - Removes "%" if present
  - Handles NA values appropriately
  - Provides consistent numeric conversion
- Use this helper consistently across all ordering functions

**Impact**: Reduces errors, ensures consistency

---

### 6. **Descending Logic Confusion [HIGH PRIORITY]**

**Issue**: The relationship between `descend`/`descend_indep` and actual sort order is implemented inconsistently:

**Current Behavior for Dependent Variables**:
```r
# Always arrange ascending first
data |>
  dplyr::arrange(.data$order_value) |>
  dplyr::mutate(order_rank = dplyr::row_number())

# Then reverse if descend=TRUE
descend_if_descending(data$.dep_order, descend)
```

**Current Behavior for Independent Variables**:
```r
# Check descend_indep in each calculate function
if (descend_indep) {
  dplyr::arrange(dplyr::desc(.data$order_value))
} else {
  dplyr::arrange(.data$order_value)
}
```

**Problems**:
1. Two different approaches for the same logical operation
2. Harder to understand and maintain
3. Risk of inconsistent behavior

**Proposed Solution**:
- Standardize to use ONE approach consistently:
  - Option A: Always arrange ascending, then apply descend transformation
  - Option B: Apply descend logic during arrange step
- Use the same `descend_if_descending()` helper for both dep and indep
- Document the chosen approach clearly

**Impact**: Simplifies code, reduces confusion, ensures consistency

---

### 7. **Error Handling and Validation [MEDIUM PRIORITY]**

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

### 8. **Testing Coverage for Edge Cases [MEDIUM PRIORITY]**

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

### 9. **Documentation Improvements [LOW PRIORITY]**

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

### Step 1: Extract `get_target_categories()` Helper

```r
#' Get target categories for positional sorting
#' 
#' Uses subset_vector to determine which categories to include based on
#' positional methods like .top, .bottom, .upper, .lower, etc.
#'
#' @param data Dataset with .category column
#' @param method Positional method (.top, .bottom, etc.)
#' @return Character vector of target category names
#' @keywords internal
get_target_categories <- function(data, method) {
  all_categories <- levels(data$.category)
  subset_vector(all_categories, method)
}
```

### Step 2: Simplify `calculate_proportion_order()`

Replace the complex if-else logic with:
```r
target_categories <- get_target_categories(data, method)
```

### Step 3: Simplify `calculate_indep_proportion_order()`

Same approach as Step 2, using the same helper function.

### Step 4: Unify Descending Logic

Consider using a single consistent approach:
```r
apply_descending <- function(x, descend) {
  if (descend) {
    max(x, na.rm = TRUE) - x + 1
  } else {
    x
  }
}
```

### Step 5: Centralize Numeric Conversion

```r
#' Convert value to numeric, handling common formats
#' @param x Vector to convert (may contain % symbols, etc.)
#' @return Numeric vector
#' @keywords internal
as_numeric_safe <- function(x) {
  x_clean <- gsub("%", "", as.character(x))
  as.numeric(x_clean)
}
```

---

## Priority Order for Implementation

### Phase 1: Critical Improvements (Do First)
1. **Use `subset_vector()` in proportion ordering** ⭐ HIGH IMPACT
   - Reduces duplication
   - Uses tested code
   - Easy to implement

2. **Standardize descending logic** ⭐ HIGH IMPACT  
   - Fixes inconsistency
   - Simplifies maintenance
   - Moderate effort

### Phase 2: Quality Improvements
3. **Add validation and error handling**
   - Improves user experience
   - Easier debugging
   - Moderate effort

4. **Centralize numeric conversion**
   - Reduces errors
   - Minor effort

5. **Document aggregation methods**
   - Clarifies behavior
   - Low effort

### Phase 3: Feature Enhancements
6. **Clarify default behavior**
   - Improves API clarity
   - Low-medium effort

7. **Add more sorting options** (if needed)
   - Based on user demand
   - Varies by feature

8. **Improve documentation**
   - Always valuable
   - Ongoing effort

### Phase 4: Test Coverage
9. **Add edge case tests**
   - Increases reliability
   - Ongoing effort

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

1. ✅ Store this analysis in copilot-analysis-sorting-problems.md
2. ⏭️ Proceed with Priority 1: Use `subset_vector()` in proportion ordering
3. Test thoroughly
4. Continue with remaining priorities based on results

---

## Notes

- This analysis is based on the current state of R/sorting_utils.R
- Some issues may be partially addressed in other parts of the codebase
- User feedback and actual usage patterns should guide priority of feature additions
- Maintain focus on reliability and consistency over adding new features
