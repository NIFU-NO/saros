# Read the intended order of the independent variable's categories

Content types that build their own tables rather than arranging
`data_summary` still need the category order the rest of the package
settled on. `.indep_order` is the authoritative source, because
[`add_indep_order()`](https://nifu-no.github.io/saros/reference/add_indep_order.md)
has already resolved `sort_indep_by` and `descend_indep` into it. It is
only present for categorical dependent variables;
`summarize_int_cat_data()` does no sorting, so a numeric dependent
variable falls back to the factor's own levels – which is what
[`arrange_table_data()`](https://nifu-no.github.io/saros/reference/arrange_table_data.md)
does in the same situation.

## Usage

``` r
get_indep_level_order(data_summary, data, indep)
```

## Arguments

- data_summary:

  Summarized data, possibly carrying `.indep_order`

- data:

  Raw dataset, used for the factor-level fallback

- indep:

  Name of the independent variable, or `NULL`

## Value

Character vector of category labels in order, or `NULL`
