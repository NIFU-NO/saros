# Read tabular data from various formats

A wrapper function to read data from different file formats

## Usage

``` r
tabular_read(path, format, ...)
```

## Arguments

- path:

  Character string specifying the file path

- format:

  Character string specifying the format: "delim", "xlsx", "csv",
  "csv2", "tsv", "sav", "dta"

- ...:

  Additional arguments passed to the underlying read functions

## Value

A data frame containing the loaded data
