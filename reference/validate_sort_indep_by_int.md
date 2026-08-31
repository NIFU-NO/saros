# Validate a `sort_indep_by` key against what an integer summary can honor

`summarize_int_cat_data()` produces one row per dependent variable and
independent category, with no `.category` or `.proportion` column. The
proportion-based keys, `.sum_value` and category vectors therefore have
no meaning here. They used to be accepted and then silently discarded,
along with `descend_indep`; they now abort, in the spirit of \#600
(#608).

## Usage

``` r
validate_sort_indep_by_int(sort_by, call = rlang::caller_env())
```

## Arguments

- sort_by:

  The `sort_indep_by` value supplied by the user.

- call:

  Calling environment for the error message.

## Value

`invisible(TRUE)`, or aborts.
