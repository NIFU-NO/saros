# Insert Text from a Data Frame by Chunk Name

Looks up a text string from a data frame based on a chunk name and
position (before/after). Optionally expands knitr templating syntax
(`{{...}}`) found in the text.

## Usage

``` r
insert_text(data, chunk, before = TRUE, error_on_empty = NULL, enabled = TRUE)
```

## Arguments

- data:

  A data frame with columns `chunk`, `before`, and `text`.

- chunk:

  Character. The chunk name to look up in `data`. If the file extension
  is `"rmarkdown"`, it is stripped automatically.

- before:

  Logical. Whether to retrieve the text marked as "before" (`TRUE`,
  default) or "after" (`FALSE`) the chunk.

- error_on_empty:

  Controls behaviour when no matching text is found:

  - `TRUE`: throws an error via
    [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

  - `FALSE`: issues a warning via
    [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html).

  - `NULL` (default): silently returns an empty `AsIs` object.

- enabled:

  Logical. If `FALSE`, the function returns `I(character(0))`
  immediately without any lookup. Can be set globally via
  `global_settings_set(new = list(enabled = FALSE), fn_name = "insert_text")`.

## Value

An [`I()`](https://rdrr.io/r/base/AsIs.html)-wrapped character string.
If no match is found and `error_on_empty` is `NULL`, an empty `AsIs`
character vector.

## Details

The function filters `data` for rows matching the given `chunk` and
`before` values. If exactly one row matches, its `text` column is
returned. If the text contains knitr templating delimiters (`{{`), it is
expanded with
[`knitr::knit_expand()`](https://rdrr.io/pkg/knitr/man/knit_expand.html).

## Examples

``` r
if (FALSE) { # \dontrun{
texts <- data.frame(
  chunk = c("intro", "intro"),
  before = c(TRUE, FALSE),
  text = c("Before the chunk.", "After the chunk.")
)
insert_text(texts, "intro", before = TRUE)
insert_text(texts, "intro", before = FALSE)
} # }
```
