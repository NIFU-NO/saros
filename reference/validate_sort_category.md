# Validate that category-based sort references categories present in data

Fails fast with an informative error when `sort_by` names one or more
categories that do not exist in the `.category` column of `data`.

## Usage

``` r
validate_sort_category(sort_by, data, call = rlang::caller_env())
```

## Arguments

- sort_by:

  Character vector of category names to sort by.

- data:

  Data frame containing a `.category` column.

- call:

  Calling environment for error reporting.

## Value

`TRUE` invisibly if valid.
