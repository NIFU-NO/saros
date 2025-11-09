# Generate Figure Title Suffix with N Range and Optional Download Links

Creates a formatted suffix for figure titles that includes the sample
size (N) range from a ggplot object. Optionally generates markdown
download links for both the plot data and the plot image.

## Usage

``` r
get_fig_title_suffix_from_ggplot(
  plot,
  save = FALSE,
  n_equals_string = "N = ",
  file_suffixes = c(".csv", ".png"),
  link_prefixes = c("[CSV](", "[PNG]("),
  save_fns = list(utils::write.csv, saros::ggsaver),
  sep = ", "
)
```

## Arguments

- plot:

  A `ggplot2` object, typically created by
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md).

- save:

  Logical flag. If `TRUE`, generates download links for the plot data
  (CSV) and plot image (PNG). If `FALSE` (default), only returns the N
  range text.

- n_equals_string:

  String. Prefix text for the sample size display (default: `"N = "`).

- file_suffixes:

  Character vector. File extensions for the saved plot images (default:
  `".png"`). Should include the dot.

- link_prefixes:

  Character vector. Markdown link text prefixes for the plot download
  links (default: `"[PNG]("`).

- save_fns:

  List of functions. Functions to save the plot data and images.

- sep:

  String. Separator between N range text and download links (default:
  `", "`).

## Value

An `AsIs` object (using [`I()`](https://rdrr.io/r/base/AsIs.html))
containing a character string with:

- Sample size range formatted as "{n_equals_string}{range}"

- If `save = TRUE`: additional download links for plot data and image,
  separated by `sep`

- Empty string if `plot` is not a valid ggplot object or has no data

## Details

This function is particularly useful for adding informative captions to
plots in reports. The N range is calculated using
[`n_range2()`](https://nifu-no.github.io/saros/reference/n_range2.md),
which extracts the sample size from the plot data. When `save = TRUE`,
the function creates downloadable files using
[`make_link()`](https://nifu-no.github.io/saros/reference/make_link.md):

- Plot data as CSV (via
  [`utils::write.csv`](https://rdrr.io/r/utils/write.table.html))

- Plot image as PNG (via
  [`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md))

The function returns an `AsIs` object to prevent automatic character
escaping in markdown/HTML contexts.

## See also

- [`n_range2()`](https://nifu-no.github.io/saros/reference/n_range2.md)
  for extracting N range from ggplot objects

- [`make_link()`](https://nifu-no.github.io/saros/reference/make_link.md)
  for creating download links

- [`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md)
  for saving ggplot objects

## Examples

``` r
# Create a sample plot
plot <- makeme(data = ex_survey, dep = b_1:b_3)

# Get just the N range text
get_fig_title_suffix_from_ggplot(plot)
#> [1] "N = 300"

# Custom N prefix
get_fig_title_suffix_from_ggplot(plot, n_equals_string = "Sample size: ")
#> [1] "Sample size: 300"

if (FALSE) { # \dontrun{
# Generate with download links (saves files to disk)
get_fig_title_suffix_from_ggplot(plot, save = TRUE)

# Custom separator and link prefix
get_fig_title_suffix_from_ggplot(
  plot,
  save = TRUE,
  sep = " | ",
  link_prefix = "[Download PNG]("
)
} # }
```
