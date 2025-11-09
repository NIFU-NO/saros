# Provide A Colour Set for A Number of Requested Colours

Possibly using colour_palette_nominal if available. If not sufficient,
uses a set palette from RColorBrewer.

## Usage

``` r
get_colour_set(
  x,
  common_data_type = "factor",
  colour_palette_nominal = NULL,
  colour_palette_ordinal = NULL,
  colour_na = NULL,
  colour_2nd_binary_cat = NULL,
  ordinal = FALSE,
  categories_treated_as_na = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- x:

  Vector for which colours will be found.

- common_data_type:

  *factor or ordered data type*

  `scalar<character>` // *default:* `factor` (`optional`)

  Currently only supports factor and ordered.

- colour_palette_nominal, colour_palette_ordinal:

  *User specified colour set*

  `vector<character>` // *default:* `NULL` (`optional`)

  User-supplied default palette, excluding `colour_na`.

- colour_na:

  *Colour for NA category*

  `scalar<character>` // *default:* `NULL` (`optional`)

  Colour as a single string for NA values, if showNA is "ifany" or
  "always".

- colour_2nd_binary_cat:

  *Colour for second binary category*

  `scalar<character>` // *default:* `"#ffffff"` (`optional`)

  Colour for the second category in binary variables. Often useful to
  hide this.

- ordinal:

  `scalar<logical>` // *default:* `FALSE` (`optional`)

  Is palette ordinal?

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
