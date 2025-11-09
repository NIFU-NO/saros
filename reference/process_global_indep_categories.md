# Process Independent Categories for Global Hiding Logic

Internal helper function that applies global hiding logic to independent
variable categories based on the hide_for_all_crowds_if_hidden_for_crowd
setting.

## Usage

``` r
process_global_indep_categories(
  kept_indep_cats_list,
  hide_for_all_crowds_if_hidden_for_crowd
)
```

## Arguments

- kept_indep_cats_list:

  Named list of kept independent categories for each crowd

- hide_for_all_crowds_if_hidden_for_crowd:

  Character vector of crowd identifiers that determine global category
  exclusions

## Value

Modified kept_indep_cats_list with global hiding logic applied:

- For crowds not in hide_for_all_crowds_if_hidden_for_crowd: only
  categories that were kept in the priority crowds are retained

- For priority crowds: original category lists are preserved
