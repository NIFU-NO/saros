# Create sorting order variables for output dataframe

This module provides centralized sorting functionality to ensure
consistent ordering across all output types (tables, plots) by using
explicit order columns instead of relying on factor levels that can be
overridden. Apply comprehensive sorting order to survey data

## Usage

``` r
add_sorting_order_vars(
  data,
  sort_dep_by = ".variable_position",
  sort_indep_by = ".factor_order",
  sort_category_by = NULL,
  descend = FALSE,
  descend_indep = FALSE
)
```

## Arguments

- data:

  Dataset with survey results

- sort_dep_by:

  How to sort dependent variables

- sort_indep_by:

  How to sort independent variable categories

- sort_category_by:

  How to sort response categories

- descend:

  Whether to reverse the dependent variable order

## Value

Dataset with added order columns: .dep_order, .indep_order,
.category_order
