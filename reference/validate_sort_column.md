# Validate that a column-based sort references an existing column

Fails fast with an informative error when `sort_by` names a column (from
the allowed whitelist) that is not present in `data`. Only performs
validation for scalar `sort_by`; non-scalar values (e.g. category
vectors) are skipped since they are validated by
[`validate_sort_category()`](https://nifu-no.github.io/saros/reference/validate_sort_category.md)
instead.

## Usage

``` r
validate_sort_column(
  sort_by,
  data,
  allowed = .saros.env$allowed_dep_sort_columns,
  call = rlang::caller_env()
)
```

## Arguments

- sort_by:

  Character scalar, or `NULL`. The column name to sort by. Non-scalar
  values are silently accepted (no column validation).

- data:

  Data frame to check.

- allowed:

  Character vector of whitelisted column names.

- call:

  Calling environment for error reporting.

## Value

`TRUE` invisibly if valid.
