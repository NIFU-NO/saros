# Check Quarto Website Folders for Missing index.qmd

Scans a Quarto website project directory for folders that contain `.qmd`
files (directly or in subfolders) but are missing an `index.qmd` file.
Such folders often cause a malfunctioning navigation menu in the
rendered Quarto website.

Folders whose names start with `_` or `.` are excluded, as these are
typically Quarto internal or hidden directories.

## Usage

``` r
check_quarto_website_index(path = ".", quiet = FALSE)
```

## Arguments

- path:

  *Path to project root*

  `scalar<character>` // default: `"."` (`optional`)

  The root directory of the Quarto website project to check.

- quiet:

  *Suppress warnings*

  `scalar<logical>` // default: `FALSE` (`optional`)

  If `TRUE`, no cli warnings are issued. The affected paths are still
  returned invisibly.

## Value

A character vector of folder paths (relative to `path`) that contain
`.qmd` files but lack an `index.qmd`. Returned invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check the current project
check_quarto_website_index()

# Check a specific directory
check_quarto_website_index("path/to/quarto-project")
} # }
```
