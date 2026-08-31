# Add independent variable ordering to an integer summary

The counterpart to
[`add_indep_order()`](https://nifu-no.github.io/saros/reference/add_indep_order.md)
for `summarize_int_cat_data()` output, which has one row per dependent
variable and independent category rather than one per response category.
Sets `.indep_order`, so that
[`arrange_table_data()`](https://nifu-no.github.io/saros/reference/arrange_table_data.md)
and
[`get_indep_level_order()`](https://nifu-no.github.io/saros/reference/get_indep_level_order.md)
pick the order up the same way they do for a categorical dependent
variable (#608).

## Usage

``` r
add_indep_order_int(
  data,
  indep,
  sort_by = ".factor_order",
  descend = FALSE,
  call = rlang::caller_env()
)
```

## Arguments

- data:

  An integer summary

- indep:

  Name of the independent variable, or `NULL`

- sort_by:

  How to sort the independent variable categories

- descend:

  Whether to reverse the resulting order

- call:

  Calling environment for error reporting

## Value

`data` with an `.indep_order` column
