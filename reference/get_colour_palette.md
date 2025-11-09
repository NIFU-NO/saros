# Provide A Colour Set for A Number of Requested Colours

Possibly using colour_palette_nominal if available. If not sufficient,
uses a set palette from RColorBrewer.

## Usage

``` r
get_colour_palette(
  data,
  col_pos,
  colour_palette_nominal = NULL,
  colour_palette_ordinal = NULL,
  colour_na = NULL,
  categories_treated_as_na = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- data:

  *Your data.frame/tibble or srvyr-object (experimental)*

  `data.frame` // *required*

  The data to be used for plotting.

- col_pos:

  Character vector of column names for which colours will be found.

- colour_palette_nominal, colour_palette_ordinal:

  *User specified colour set*

  `vector<character>` // *default:* `NULL` (`optional`)

  User-supplied default palette, excluding `colour_na`.

- colour_na:

  *Colour for NA category*

  `scalar<character>` // *default:* `NULL` (`optional`)

  Colour as a single string for NA values, if showNA is "ifany" or
  "always".

- categories_treated_as_na:

  *NA categories*

  `vector<character>` // *default:* `NULL` (`optional`)

  Categories that should be treated as NA.

- call:

  *Internal call*

  `obj:<call>` // *Default:*
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html)
  (`optional`)

  Both the absolute and relative folderpaths are required, as strings.

## Value

A colour set as character vector, where `NA` has the `colour_na`, and
the rest are taken from colour_palette_nominal if available.
