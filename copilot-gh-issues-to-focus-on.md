# NA

495 Validation helpers for sorting inputs Clear acceptance criteria: add
validate_sort_column() / validate_sort_category() with cli messages and
tests. 524 cat_table\_\* should consistently display 0, NA and “” as the
same cell Narrow scope — can inspect cat_table\_\* rendering logic and
normalize these values.

Issues That Are Actionable but Require Design Decisions 529 Refactoring
Massive scope (9 sub-items). Individual items are implementable, but
needs prioritization decision from maintainer. 493 Whitelist direct
column sorting: docs + validation Clear criteria, but “supported sort
keys” need to be defined by maintainer. 494 Variability-based dep
sorting (range of proportions) New feature with criteria, but needs API
naming decision (sort_dep_by = ‘.range’?). 492 Support indep totals
ordering Complex feature, needs materialization-point decision (upstream
vs on-demand). 462 girafe should return ggobj for non-HTML Requires
decision on adding knitr as a dependency. 498 Global option for
link/link_plot only in project Feature requiring design on how to detect
“project” context.
