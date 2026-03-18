# Wrap a file-writing expression with informative error handling

Catches errors from file-writing operations and re-throws with
actionable diagnostics (permissions, long OneDrive/SharePoint paths,
etc.).

## Usage

``` r
safe_file_write(expr, path, call = rlang::caller_env())
```

## Arguments

- expr:

  An expression that writes a file.

- path:

  Character scalar. The file path being written to.

- call:

  The calling environment for error reporting.

## Value

The result of `expr`, invisibly.
