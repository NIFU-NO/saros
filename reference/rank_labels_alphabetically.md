# Rank category labels alphabetically

The ordering value behind `sort_indep_by = ".variable_label"`.
[`rank()`](https://rdrr.io/r/base/rank.html) on a factor ranks by level
code, which made the key indistinguishable from `".factor_order"`
(#617). The labels are compared as text instead, in the C-locale
collation
[`add_dep_order()`](https://nifu-no.github.io/saros/reference/add_dep_order.md)
gets from
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
so the key means the same thing on both sides. An `"NA"` category –
which is how the summaries carry missing values when `showNA` keeps them
– stays last, as it does under every other key, rather than being filed
under N.

## Usage

``` r
rank_labels_alphabetically(x)
```

## Arguments

- x:

  Factor or character vector of category labels

## Value

Integer vector with one rank per element, tied within a category; `NA`
where `x` is `NA`
