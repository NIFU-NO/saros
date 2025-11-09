# Write tabular data to various formats

A wrapper function to write data frames to different file formats

## Usage

``` r
tabular_write(object, path, format)
```

## Arguments

- object:

  A data frame to write

- path:

  Character string specifying the output file path

- format:

  Character string specifying the format: "delim", "xlsx", "csv",
  "csv2", "tsv", "sav", "dta"

## Value

Invisibly returns TRUE on success, used for side effects

## Examples

``` r
data <- data.frame(x = 1:3, y = letters[1:3])

# Write as CSV
tabular_write(data, tempfile(fileext = ".csv"), format = "csv")

# Write as Excel
tabular_write(data, tempfile(fileext = ".xlsx"), format = "xlsx")

# Write as SPSS
tabular_write(data, tempfile(fileext = ".sav"), format = "sav")
```
