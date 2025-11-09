# Summarize a survey dataset for use in tables and graphs

Summarize a survey dataset for use in tables and graphs

## Usage

``` r
summarize_cat_cat_data(
  data,
  dep = colnames(data),
  indep = NULL,
  ...,
  showNA = c("ifany", "always", "never"),
  totals = FALSE,
  sort_by = ".upper",
  sort_dep_by = NULL,
  sort_indep_by = ".factor_order",
  data_label = c("percentage_bare", "percentage", "proportion", "count", "mean",
    "median"),
  digits = 0,
  add_n_to_dep_label = FALSE,
  add_n_to_indep_label = FALSE,
  add_n_to_label = FALSE,
  add_n_to_category = FALSE,
  hide_label_if_prop_below = 0.01,
  data_label_decimal_symbol = ".",
  categories_treated_as_na = NULL,
  label_separator = NULL,
  descend = FALSE,
  descend_indep = FALSE,
  labels_always_at_bottom = NULL,
  labels_always_at_top = NULL,
  translations = list(),
  call = rlang::caller_env()
)
```

## Arguments

- data:

  *Your data.frame/tibble or srvyr-object (experimental)*

  `data.frame` // *required*

  The data to be used for plotting.

- dep, indep:

  *Variable selections*

  \<`tidyselect`\> // *Default:* `NULL`, meaning everything for dep,
  nothing for indep.

  Columns in `data`. `dep` is compulsory.

- ...:

  *Dynamic dots*

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>

  Arguments forwarded to the corresponding functions that create the
  elements.

- showNA:

  *Show NA categories*

  `vector<character>` // *default:* `c("ifany", "always", "never")`
  (`optional`)

  Choose whether to show NA categories in the results.

- totals:

  *Include totals*

  `scalar<logical>` // *default:* `FALSE` (`optional`)

  Whether to include totals in the output.

- sort_by:

  *What to sort output by (legacy)*

  `vector<character>` // *default:* `NULL` (`optional`)

  **DEPRECATED:** Use `sort_dep_by` and `sort_indep_by` instead for
  clearer control. When specified, this parameter will be used for both
  dependent and independent sorting. If `NULL` (default), dependent
  variables will be sorted by `.variable_position`.

  NULL

  :   Uses `.variable_position` for dependent variables, no sorting for
      independent.

  ".top"

  :   The proportion for the highest category available in the variable.

  ".upper"

  :   The sum of the proportions for the categories above the middle
      category.

  ".mid_upper"

  :   The sum of the proportions for the categories including and above
      the middle category.

  ".mid_lower"

  :   The sum of the proportions for the categories including and below
      the middle category.

  ".lower"

  :   The sum of the proportions for the categories below the middle
      category.

  ".bottom"

  :   The proportions for the lowest category available in the variable.

  ".variable_label"

  :   Sort by the variable labels.

  ".variable_name"

  :   Sort by the variable names.

  ".variable_position"

  :   Sort by the variable position in the supplied data frame.

  ".by_group"

  :   The groups of the by argument.

  character()

  :   Character vector of category labels to sum together.

- sort_dep_by:

  *What to sort dependent variables by*

  `vector<character>` // *default:* `".variable_position"` (`optional`)

  Sort dependent variables in output. When using `indep`-argument,
  sorting differs between ordered factors and unordered factors:
  Ordering of ordered factors is always respected in output (their
  levels define the base order). Unordered factors will be reordered by
  `sort_dep_by`.

  NULL or ".variable_position"

  :   Sort by variable position in the supplied data frame (default).

  ".variable_label"

  :   Sort by the variable labels.

  ".variable_name"

  :   Sort by the variable names.

  ".top"

  :   The proportion for the highest category available in the variable.

  ".upper"

  :   The sum of the proportions for the categories above the middle
      category.

  ".mid_upper"

  :   The sum of the proportions for the categories including and above
      the middle category.

  ".mid_lower"

  :   The sum of the proportions for the categories including and below
      the middle category.

  ".lower"

  :   The sum of the proportions for the categories below the middle
      category.

  ".bottom"

  :   The proportions for the lowest category available in the variable.

- sort_indep_by:

  *What to sort independent variable categories by*

  `vector<character>` // *default:* `".factor_order"` (`optional`)

  Sort independent variable categories in output. When
  `".factor_order"`, preserves the original factor level order for the
  independent variable. Passing `NULL` is accepted and treated as
  `".factor_order"`.

  NULL

  :   No sorting - preserves original factor level order (default).

  ".top"

  :   The proportion for the highest category available.

  ".upper"

  :   The sum of the proportions for the categories above the middle
      category.

  ".mid_upper"

  :   The sum of the proportions for the categories including and above
      the middle category.

  ".mid_lower"

  :   The sum of the proportions for the categories including and below
      the middle category.

  ".lower"

  :   The sum of the proportions for the categories below the middle
      category.

  ".bottom"

  :   The proportions for the lowest category available.

  character()

  :   Character vector of category labels to sum together.

- data_label:

  *Data label*

  `scalar<character>` // *default:* `"proportion"` (`optional`)

  One of "proportion", "percentage", "percentage_bare", "count", "mean",
  or "median".

- digits:

  *Decimal places*

  `scalar<integer>` // *default:* `0L` (`optional`)

  Number of decimal places.

- add_n_to_dep_label, add_n_to_indep_label:

  *Add N= to the variable label*

  `scalar<logical>` // *default:* `FALSE` (`optional`)

  For some plots and tables it is useful to attach the `"N="` to the end
  of the label of the dependent and/or independent variable. Whether it
  is `N` or `N_valid` depends on your `showNA`-setting. See also
  `translations$add_n_to_dep_label_prefix`,
  `translations$add_n_to_dep_label_suffix`,
  `translations$add_n_to_indep_label_prefix`,
  `translations$add_n_to_indep_label_suffix`.

- add_n_to_label:

  *Add N= to the variable label of both dep and indep*

  `scalar<logical>` // *default:* `FALSE` (`optional`)

  For some plots and tables it is useful to attach the `"N="` to the end
  of the label. Whether it is `N` or `N_valid` depends on your
  `showNA`-setting. See also `translations$add_n_to_label_prefix` and
  `translations$add_n_to_label_suffix`.

- add_n_to_category:

  *Add N= to the category*

  `scalar<logical>` // *default:* `FALSE` (`optional`)

  For some plots and tables it is useful to attach the `"N="` to the end
  of the category. This will likely produce a range across the
  variables, hence an infix (comma) between the minimum and maximum can
  be specified. Whether it is `N` or `N_valid` depends on your
  `showNA`-setting. See also `translations$add_n_to_category_prefix`,
  `translations$add_n_to_category_infix`, and
  `translations$add_n_to_category_suffix`.

- hide_label_if_prop_below:

  *Hide label threshold*

  `scalar<numeric>` // *default:* `NULL` (`optional`)

  Whether to hide label if below this value.

- data_label_decimal_symbol:

  *Decimal symbol*

  `scalar<character>` // *default:* `"."` (`optional`)

  Decimal marker, some might prefer a comma ',' or something else
  entirely.

- categories_treated_as_na:

  *NA categories*

  `vector<character>` // *default:* `NULL` (`optional`)

  Categories that should be treated as NA.

- label_separator:

  *How to separate main question from sub-question*

  `scalar<character>` // *default:* `NULL` (`optional`)

  Separator for main question from sub-question.

- descend:

  *Sorting order*

  `scalar<logical>` // *default:* `FALSE` (`optional`)

  Reverse sorting of `sort_by` in figures and tables. Works with both
  ordered and unordered factors - for ordered factors, it reverses the
  display order while preserving the inherent level ordering. See
  `arrange_section_by` for sorting of report sections.

- descend_indep:

  *Sorting order for independent variables*

  `scalar<logical>` // *default:* `FALSE` (`optional`)

  Reverse sorting of `sort_indep_by` in figures and tables. Works with
  both ordered and unordered factors - for ordered factors, it reverses
  the display order while preserving the inherent level ordering. See
  `arrange_section_by` for sorting of report sections.

- labels_always_at_top, labels_always_at_bottom:

  *Top/bottom variables*

  `vector<character>` // *default:* `NULL` (`optional`)

  Column names in `data` that should always be placed at the top or
  bottom of figures/tables.

- translations:

  *Localize your output*

  `list<character>`

  A list of translations where the name is the code and the value is the
  translation. See the examples.

- call:

  *Internal call*

  `obj:<call>` // *Default:*
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html)
  (`optional`)

  Both the absolute and relative folderpaths are required, as strings.

## Value

Dataset with the columns: `.variable_name`, `.variable_label`,
`.category`, `.count`, `.count_se`, `.count_per_dep`,
`.count_per_indep_group`, `.proportion`, `.proportion_se`, `.mean`,
`.mean_se`, `.median`, indep-variable(s), `.data_label`,
`.comb_categories`, `.sum_value`, `.variable_label_prefix`
