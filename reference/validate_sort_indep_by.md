# Validate a `sort_indep_by` key against what `add_indep_order()` implements

[`add_indep_order()`](https://nifu-no.github.io/saros/reference/add_indep_order.md)
previously fell back to an unsorted `seq_len(nrow(data))` for anything
it did not recognize, so a typo silently produced unsorted output with
no indication that the argument had been ignored (#600).

## Usage

``` r
validate_sort_indep_by(sort_by, data, call = rlang::caller_env())
```

## Arguments

- sort_by:

  The `sort_indep_by` value supplied by the user.

- data:

  Dataset, used to read the available `.category` levels.

- call:

  Calling environment for the error message.

## Value

`invisible(TRUE)`, or aborts.
