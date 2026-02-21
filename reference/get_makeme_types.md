# Get all registered options for the type-argument in the `makeme`-function

The
[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)-function
take for the argument `type` one of several strings to indicate content
type and output type. This function collects all registered
alternatives. Extensions are possible, see further below.

Built-in types:

Whereas the names of the types can be arbitrary, a pattern is pursued in
the built-in types. Prefix indicates what dependent data type it is
intended for

- "cat":

  Categorical (ordinal and nominal) data.

- "chr":

  Open ended responses and other character data.

- "int":

  Integer and numeric data.

Suffix indicates output

- "html":

  Interactive html, usually what you want for Quarto, as Quarto can
  usually convert to other formats when needed

- "docx":

  However, Quarto's and Pandoc's docx-support is currently still
  limited, for instance as vector graphics are converted to raster
  graphics for docx output. Hence, `saros` offers some types that
  outputs into MS Chart vector graphics. Note that this is experimental
  and not actively developed.

- "pdf":

  This is basically just a shortcut for "html" with `interactive=FALSE`

## Usage

``` r
get_makeme_types()
```

## Value

Character vector

## Further details about some of the built-in types:

- "cat_plot\_":

  A Likert style plot for groups of categorical variables sharing the
  same categories.

- "cat_table\_":

  A Likert style table.

- "chr_table\_":

  A single-column table listing unique open ended responses.

- "sigtest_table\_":

  See below

sigtest_table\_\\: Make Table with All Combinations of
Univariate/Bivariate Significance Tests Based on Variable Types

Although there are hundreds of significance tests for associations
between two variables, depending upon the distributions, variables types
and assumptions, most fall into a smaller set of popular tests. This
function runs for all combinations of dependent and independent
variables in data, with a suitable test (but not the only possible) for
the combination. Also supports univariate tests, where the assumptions
are that of a mean of zero for continuous variables or all equal
proportions for binary/categorical.

This function does not allow any adjustments - use the original
underlying functions for that (chisq.test, t.test, etc.)

## Expanding with custom types

[`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) calls
the generic
[`make_content()`](https://nifu-no.github.io/saros/reference/make_content.md),
which uses the S3-method system to dispatch to the relevant method
(i.e., `paste0("make_content.", type)`). makeme forwards all its
arguments to make_content, with the following exceptions:

1.  dep and indep are converted from
    [`dplyr::dplyr_tidy_select()`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)-syntax
    to simple character vectors, for simplifying building your own
    functions.

2.  data_summary is attached, which contains many useful pieces of info
    for many (categorical) displays.

## Examples

``` r
get_makeme_types()
#> [1] "cat_plot_docx"      "cat_plot_html"      "cat_table_docx"    
#> [4] "cat_table_html"     "chr_table_docx"     "chr_table_html"    
#> [7] "int_plot_html"      "int_table_html"     "sigtest_table_html"
```
