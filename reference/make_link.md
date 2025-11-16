# Save data to a file and return a Markdown link

The file is automatically named by a hash of the object, removing the
need to come up with unique file names inside a Quarto report. This has
the added benefit of reducing storage needs if the objects needing
linking to are identical, and all are stored in the same folder. It also
allows the user to download multiple files without worrying about
accidentally overwriting them.

## Usage

``` r
make_link(
  data,
  folder = NULL,
  file_prefix = NULL,
  file_suffix = ".csv",
  save_fn = utils::write.csv,
  link_prefix = "[download figure data](",
  link_suffix = ")",
  ...
)
```

## Arguments

- data:

  *Data or object*

  `<data.frame|tbl|obj>`

  Data frame if using a tabular data `save_fn`, or possibly any R
  object, if a serializing `save_fn` is provided (e.g.
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)).

- folder:

  *Where to store file*

  `scalar<character>` // *default:* `"."` (`optional`)

  Defaults to same folder.

- file_prefix, file_suffix:

  *File prefix/suffix*

  `scalar<character>` // *default:* `""` and `".csv"` (`optional`)

  `file_suffix` should include the dot before the extension.

- save_fn:

  *Saving function*

  `function` // *default:*
  [`utils::write.csv`](https://rdrr.io/r/utils/write.table.html)

  Can be any saving/writing function. However, first argument must be
  the object to be saved, and the second must be the path. Hence,
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  must be wrapped in another function with `filename` and `object`
  swapped. See
  [`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md)
  for an example of such a wrapper function.

- link_prefix, link_suffix:

  *Link prefix/suffix*

  `scalar<character>` // *default:* `"[download data]("` and `")"`

  The stuff that is returned.

- ...:

  *Dynamic dots*

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>

  Arguments forwarded to the corresponding functions that create the
  elements.

## Value

String.

## Examples

``` r
make_link(mtcars, folder = tempdir())
#> [1] "[download figure data](/tmp/Rtmp8xE3ng/d0487363db4e6cc64fdb740cb6617fc0.csv)"
```
