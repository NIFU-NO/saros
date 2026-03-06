# Find a Matching DOCX File for a PDF (Case-Insensitive)

Looks in the same directory as the PDF for a `.docx` file with the same
base name, matching case-insensitively on the extension.

## Usage

``` r
find_matching_docx(pdf_path)
```

## Arguments

- pdf_path:

  Path to the PDF file.

## Value

Path to the matching DOCX file, or `NULL` if not found.
