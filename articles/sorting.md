# 

``` r

library(saros)
```

## Scope

This article covers the ordering of rows, bars and categories *inside* a
single figure or table produced by
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md).

It does not cover the ordering of chapters, sections or files across a
report. That belongs to the scaffolding stage and lives in
[saros.base](https://nifu-no.github.io/saros.base/) — see
`arrange_section_by` in `refine_chapter_overview()` and [Prepare the
chapter
overview](https://nifu-no.github.io/saros.base/articles/vig_06_prepare_chapter_overview.html).

As a rule of thumb: **saros.base decides which output appears where in
the report; saros decides what the output looks like once it is there.**

## The three controls

Every [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)
call exposes three sorting arguments:

| Argument | Controls | Default |
|----|----|----|
| `sort_dep_by` | The order of the dependent variables (the rows/bars) | `".variable_position"` |
| `sort_indep_by` | The order of the independent variable’s categories (the groups/facets) | `".factor_order"` |
| `descend` / `descend_indep` | The direction of each of the above | `TRUE` / `FALSE` |

``` r

makeme(
  data = ex_survey,
  dep = b_1:b_3,
  type = "cat_table_html",
  sort_dep_by = ".top",
  descend = TRUE
)
```

## Ordered factors take precedence

If a dependent variable is an **ordered** factor, its level order
defines the base order and `sort_dep_by` will not override it — the
levels are the authoritative scale. `descend` still applies, reversing
the display order while preserving the inherent ordering.

Unordered factors are reordered freely by `sort_dep_by`.

The same holds for the independent variable: an ordered indep factor
takes precedence over `sort_indep_by`, and is reversed only when
`descend_indep = TRUE`.

## `sort_dep_by`: ordering the dependent variables

### Structural keys

These ignore the responses and sort on the variables themselves.

| Key                    | Orders by                                         |
|------------------------|---------------------------------------------------|
| `".variable_position"` | Position in the supplied data frame (the default) |
| `".variable_label"`    | The variable labels, alphabetically               |
| `".variable_name"`     | The variable names, alphabetically                |

### Response-pattern keys

These sort on where the mass of the responses sits. They are the usual
choice for “show me the items respondents were most positive about”.

| Key            | Orders by                                                 |
|----------------|-----------------------------------------------------------|
| `".top"`       | Proportion in the highest category                        |
| `".upper"`     | Summed proportion above the middle category               |
| `".mid_upper"` | Summed proportion including and above the middle category |
| `".mid_lower"` | Summed proportion including and below the middle category |
| `".lower"`     | Summed proportion below the middle category               |
| `".bottom"`    | Proportion in the lowest category                         |

``` r

# The items with the largest share in the top category first
makeme(
  data = ex_survey,
  dep = b_1:b_3,
  type = "cat_plot_html",
  sort_dep_by = ".top"
)
```

### Spread

`".range"` sorts by how *unevenly* the answers are distributed within
each variable: the difference between the largest and the smallest
category proportion.

- A variable where one category dominates has a **large** range.
- A variable whose answers spread evenly across all categories has a
  range near zero.

It is a measure of **consensus, not of direction**. A large range tells
you the responses concentrated in one category, but nothing about
*which* category — `".top"` and `".bottom"` are the keys for that. Reach
for `".range"` when the question is “which items did respondents
converge on, and which split them?” rather than “which items scored
highest”.

``` r

# Most concentrated items first (one category dominates)
makeme(
  data = ex_survey,
  dep = b_1:b_3,
  type = "cat_plot_html",
  sort_dep_by = ".range",
  descend = TRUE
)

# Most evenly split items first
makeme(
  data = ex_survey,
  dep = b_1:b_3,
  type = "cat_plot_html",
  sort_dep_by = ".range",
  descend = FALSE
)
```

`".range"` works on the proportions, so it applies to the categorical
output types. Types that never compute proportions (the `int_*` types,
for example) ignore it rather than reordering.

### Direct column keys

These sort on a computed summary column, when the output type provides
it.

| Key             | Orders by                            |
|-----------------|--------------------------------------|
| `".count"`      | Cell count                           |
| `".proportion"` | Proportion                           |
| `".mean"`       | Mean of the ordinal category codes   |
| `".median"`     | Median of the ordinal category codes |
| `".sum_value"`  | Summed value                         |

Note that `".mean"` and `".median"` are computed over the factor’s
*level codes* (1, 2, 3, …), not over the category labels, which are
normally text. This is deliberate: it is the usual way to summarize an
ordinal survey scale.

### Custom category keys

Passing a character vector of category labels sorts on those categories.
This is the escape hatch when none of the named keys expresses the
ordering you want.

Be aware that the two forms sort on different bases:

- A **single** label orders by that category’s `.count`.
- **Several** labels order by the summed `.sum_value`, which sums the
  *proportions* — or the counts when `data_label = "count"`.

When the dependent variables have equal numbers of respondents this
makes no difference. When they do not, adding a second label can change
the ordering of the variables you had already sorted, because the basis
switched from counts to proportions.

``` r

makeme(
  data = ex_survey,
  dep = b_1:b_3,
  type = "cat_plot_html",
  sort_dep_by = c("A bit", "A lot")
)
```

## `sort_indep_by`: ordering the independent categories

| Key | Orders by |
|----|----|
| `".factor_order"` (or `NULL`) | The indep factor’s own level order (the default) |
| `".variable_label"` | The category labels, alphabetically |
| `".top"`, `".upper"`, `".mid_upper"`, `".mid_lower"`, `".lower"`, `".bottom"` | The same response-pattern aggregates as for `sort_dep_by` |
| `".count"` | Cell count |
| `".count_per_indep_group"` | Total valid responses in each independent group |
| `".mean"`, `".median"`, `".sum_value"` | The corresponding summary column |
| [`character()`](https://rdrr.io/r/base/character.html) | Summed proportions of the named categories |

`".count_per_indep_group"` is the key for ordering groups by **size**
rather than by response pattern — putting the largest organization,
region or team first regardless of how it answered.

``` r

makeme(
  data = ex_survey,
  dep = b_1:b_3,
  indep = x1_sex,
  type = "cat_plot_html",
  sort_indep_by = ".count_per_indep_group",
  descend_indep = TRUE
)
```

Independent ordering is computed *per dependent variable*, so the group
order may legitimately differ between the rows of a single figure.

### When the dependent variable is numeric

An `int_*` or `sigtest_*` output built on a numeric dependent variable
has no response categories, so the keys that aggregate over them cannot
be honored. Only `".factor_order"`, `".variable_label"`, `".count"`,
`".count_per_indep_group"`, `".mean"` and `".median"` apply there.
`".mean"` and `".median"` are the most useful of these: they order the
groups by the dependent variable’s own values, putting the
lowest-scoring group first.

``` r

makeme(
  data = ex_survey,
  dep = c_1,
  indep = x1_sex,
  type = "int_table_html",
  sort_indep_by = ".mean",
  descend_indep = TRUE
)
```

The remaining keys raise an error naming what is supported, rather than
being accepted and quietly dropped.

## Direction

`descend` reverses `sort_dep_by`; `descend_indep` reverses
`sort_indep_by`.

Note the asymmetry in the defaults: **`descend` defaults to `TRUE`**
(largest first, which is what you normally want for a response-pattern
sort), while `descend_indep` defaults to `FALSE`.

## When a key is not recognized

Sort keys are validated against a whitelist. Passing a key that is
neither a recognized `.`-prefixed option nor a category present in the
data raises an error that lists both sets:

``` r

makeme(
  data = ex_survey,
  dep = b_1:b_3,
  type = "cat_table_html",
  sort_dep_by = ".nonexistent"
)
#> Error:
#> x Invalid `sort_by`: .nonexistent
#> i `sort_by` must be either NULL (no sorting), a single string from the set
#>   options `.top`, `.upper`, `.mid_upper`, `.lower`, `.mid_lower`, `.bottom`,
#>   `.range`, `.variable_name`, `.variable_position`, `.variable_label`, ...
#>   or all valid categories in the data (`Not at all`, `A bit`, and `A lot`).
```

Misspelling a category label produces the same error, so the valid
category names are always visible at the point of failure.

Two caveats are worth knowing:

- A key that *is* whitelisted but whose column the output type never
  computed reports that separately, naming the columns that can be
  sorted on.
- A key that cannot apply to the output at all — a proportion-based key
  on a numeric dependent variable, say — is rejected the same way,
  naming the keys that do apply. Earlier versions accepted and silently
  ignored these, so a sort could appear to have had no effect; they now
  fail loudly instead.

## Setting a sort order once for the whole report

Rather than repeating the arguments in every chunk, set them once with
[`global_settings_set()`](https://nifu-no.github.io/saros/reference/global_settings_set.md):

``` r

global_settings_set(
  fn_name = "makeme",
  new = list(
    sort_dep_by = ".top",
    descend = TRUE
  )
)
```

Every later
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) call
in the document inherits these unless it overrides them explicitly.
