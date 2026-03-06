# Convert List of Tables to Quarto Tabset

Creates a Quarto tabset from a named list of data frames, rendering each
as a table in its own tab. Designed to be called within a Quarto
document code chunk with `results='asis'`.

## Usage

``` r
crowd_tables_as_tabset(tbl_list, table_fn = knitr::kable)
```

## Arguments

- tbl_list:

  A named list of data frames. Names become tab labels.

- table_fn:

  A function that converts a data frame to a printable table object.
  Defaults to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html). Other
  options include `gt::gt()`, `tinytable::tt`, etc. Can be set globally
  via
  `global_settings_set(new = list(table_fn = gt::gt), fn_name = "crowd_tables_as_tabset")`.

## Value

Called for its side effects (printing tabset markdown and tables).
Returns `NULL` invisibly.

## Details

This function outputs raw Quarto markdown (level-5 headings) interleaved
with printed tables. The enclosing chunk should use the Quarto `tabset`
panel layout and `results: asis`.

## See also

[`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
for the plot equivalent.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl_list <- list(
  "Group A" = head(mtcars),
  "Group B" = tail(mtcars)
)
crowd_tables_as_tabset(tbl_list)

# Use gt::gt instead
crowd_tables_as_tabset(tbl_list, table_fn = gt::gt)
} # }
```
