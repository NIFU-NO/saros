# Create Markdown Links to Files with Document Titles

Scans a folder for files matching a pattern and generates a markdown
list with links to each file. The link text is extracted from the
document's title metadata (for DOCX, PPTX, PDF files) or uses the
filename as fallback.

## Usage

``` r
make_file_links(
  folder = ".",
  pattern = "",
  bullet_style = "-",
  recurse = FALSE,
  relative_links = TRUE
)
```

## Arguments

- folder:

  String. Path to the folder containing files. Defaults to current
  directory (`"."`).

- pattern:

  String. Regular expression pattern for file matching (e.g.,
  `"\\.pptx$"`, `"\\.pdf$"`, `"^report_.*\\.docx$"`). Defaults to `""`
  (all files).

- bullet_style:

  String. Markdown list style. One of `"-"` (unordered), `"*"`
  (unordered), or `"1."` (ordered). Defaults to `"-"`.

- recurse:

  Logical. Whether to search recursively in subdirectories. Defaults to
  `FALSE`.

- relative_links:

  Logical. Whether to use relative or absolute paths in links. If
  `TRUE`, paths are relative to `folder`. If `FALSE`, uses absolute
  paths. Defaults to `TRUE`.

## Value

A character string containing a markdown-formatted list of links. Each
line contains a bullet point and a link with the document title as link
text. Returns empty string if no files found.

## Details

The function attempts to extract document titles from file metadata:

- **DOCX files**: Extracts title from document properties (requires
  `officer` package)

- **PPTX files**: Extracts title from presentation properties (requires
  `officer` package)

- **PDF files**: Extracts title from PDF metadata (requires `pdftools`
  package)

If title extraction fails or the file type is unsupported, the filename
(without extension) is used as the link text.

The function preserves the order of files as returned by
[`fs::dir_ls()`](https://fs.r-lib.org/reference/dir_ls.html), which
typically sorts alphabetically.

## Optional Packages

The `pdftools` package is optional (in Suggests) and only needed for PDF
title extraction.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create links to all PowerPoint files in a folder
make_file_links(folder = "presentations", pattern = "\\.pptx$")

# Create links to PDF reports with numbered list
make_file_links(
  folder = "reports",
  pattern = "^report_.*\\.pdf$",
  bullet_style = "1."
)

# Recursively find all Office documents
make_file_links(
  folder = "documents",
  pattern = "\\.(docx|pptx)$",
  recurse = TRUE
)
} # }
```
