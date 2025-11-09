# Post-process Makeme Data (Legacy)

Legacy function that combines both factor level processing and binary
category color processing. Use the individual functions for new code.

## Usage

``` r
post_process_makeme_data(
  data,
  indep = NULL,
  showNA = "never",
  colour_2nd_binary_cat = NULL
)
```

## Arguments

- data:

  Data frame containing the data

- indep:

  Character string naming the independent variable (or NULL)

- showNA:

  Character indicating how to handle NA values

- colour_2nd_binary_cat:

  Color specification for second binary category

## Value

Modified data frame
