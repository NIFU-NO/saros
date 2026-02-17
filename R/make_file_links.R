#' Create Markdown Links to Files with Document Titles
#'
#' @description
#' Scans a folder for files matching a pattern and generates a markdown list
#' with links to each file. The link text is extracted from the document's title
#' metadata (for DOCX, PPTX, PDF files) or uses the filename as fallback.
#'
#' @param folder String. Path to the folder containing files. Defaults to current
#'   directory (`"."`).
#' @param pattern String. Regular expression pattern for file matching (e.g., `"\\.pptx$"`,
#'   `"\\.pdf$"`, `"^report_.*\\.docx$"`). Defaults to `""` (all files).
#' @param bullet_style String. Markdown list style. One of `"-"` (unordered),
#'   `"*"` (unordered), or `"1."` (ordered). Defaults to `"-"`.
#' @param recurse Logical. Whether to search recursively in subdirectories.
#'   Defaults to `FALSE`.
#' @param relative_links Logical. Whether to use relative or absolute paths in links.
#'   If `TRUE`, paths are relative to `folder`. If `FALSE`, uses absolute paths.
#'   Defaults to `TRUE`.
#'
#' @return A character string containing a markdown-formatted list of links.
#'   Each line contains a bullet point and a link with the document title as
#'   link text. Returns empty string if no files found.
#'
#' @details
#' The function attempts to extract document titles from file metadata:
#' - **DOCX files**: Extracts title from document properties (requires `officer` package)
#' - **PPTX files**: Extracts title from presentation properties (requires `officer` package)
#' - **PDF files**: Extracts title from PDF metadata (requires `pdftools` package)
#'
#' If title extraction fails or the file type is unsupported, the filename
#' (without extension) is used as the link text.
#'
#' The function preserves the order of files as returned by `fs::dir_ls()`,
#' which typically sorts alphabetically.
#'
#' @section Optional Packages:
#' The `pdftools` package is optional (in Suggests) and only needed for PDF title extraction.
#'
#' @importFrom rlang %||%
#' @export
#'
#' @examples
#' \dontrun{
#' # Create links to all PowerPoint files in a folder
#' make_file_links(folder = "presentations", pattern = "\\.pptx$")
#'
#' # Create links to PDF reports with numbered list
#' make_file_links(
#'   folder = "reports",
#'   pattern = "^report_.*\\.pdf$",
#'   bullet_style = "1."
#' )
#'
#' # Recursively find all Office documents
#' make_file_links(
#'   folder = "documents",
#'   pattern = "\\.(docx|pptx)$",
#'   recurse = TRUE
#' )
#' }
make_file_links <- function(
  folder = ".",
  pattern = "",
  bullet_style = "-",
  recurse = FALSE,
  relative_links = TRUE
) {
  # Validate inputs
  validate_string(folder, null_allowed = FALSE)
  validate_string(pattern, null_allowed = FALSE)
  validate_string(bullet_style, null_allowed = FALSE)
  validate_bool(recurse)
  validate_bool(relative_links)

  if (!bullet_style %in% c("-", "*", "1.")) {
    cli::cli_abort(c(
      "{.arg bullet_style} must be one of {.val -}, {.val *}, or {.val 1.}",
      "x" = "Got {.val {bullet_style}}"
    ))
  }

  if (!fs::dir_exists(folder)) {
    cli::cli_abort(c(
      "Folder does not exist: {.path {folder}}"
    ))
  }

  # Find files matching pattern
  files <- fs::dir_ls(
    path = folder,
    regexp = if (pattern == "") NULL else pattern,
    type = "file",
    recurse = recurse
  )

  if (length(files) == 0) {
    cli::cli_warn(c(
      "No files found matching pattern {.val {pattern}} in {.path {folder}}"
    ))
    return("")
  }

  # Check for missing optional packages once per call
  missing_packages <- character(0)
  file_exts <- unique(tolower(fs::path_ext(files)))
  if ("pdf" %in% file_exts && !requireNamespace("pdftools", quietly = TRUE)) {
    missing_packages <- c(
      missing_packages,
      "pdftools (for PDF title extraction)"
    )
  }
  if (length(missing_packages) > 0) {
    cli::cli_warn(c(
      "Optional packages not installed:",
      "i" = paste(missing_packages, collapse = ", "),
      "i" = "Filenames will be used instead of document titles"
    ))
  }

  # Generate links
  links <- vapply(
    files,
    function(filepath) {
      # Extract title from document
      title <- extract_document_title(filepath)

      # Determine link path
      if (relative_links) {
        link_path <- fs::path_rel(filepath, start = folder)
      } else {
        link_path <- fs::path_abs(filepath)
      }

      # Escape markdown special characters for security
      title_escaped <- escape_markdown(title)
      path_escaped <- escape_markdown_link(link_path)

      # Format as markdown list item
      if (bullet_style == "1.") {
        # Numbered lists need index, will be added later
        sprintf("[%s](%s)", title_escaped, path_escaped)
      } else {
        sprintf("%s\t[%s](%s)", bullet_style, title_escaped, path_escaped)
      }
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  # Handle numbered lists
  if (bullet_style == "1.") {
    links <- sprintf("%d.\t%s", seq_along(links), links)
  }

  # Combine into single string
  paste0(links, collapse = "\n")
}


#' Extract Document Title from File
#'
#' @description
#' Internal helper to extract title metadata from DOCX, PPTX, and PDF files.
#'
#' @param filepath Path to the file.
#'
#' @return Character string with the document title, or filename if extraction fails.
#' @keywords internal
extract_document_title <- function(filepath) {
  ext <- tolower(fs::path_ext(filepath))
  filename_without_ext <- fs::path_ext_remove(fs::path_file(filepath))

  # Only attempt extraction for supported file types
  if (!ext %in% c("docx", "pptx", "pdf")) {
    return(filename_without_ext)
  }

  title <- tryCatch(
    {
      switch(
        ext,
        docx = extract_docx_title(filepath),
        pptx = extract_pptx_title(filepath),
        pdf = extract_pdf_title(filepath),
        filename_without_ext
      )
    },
    error = function(e) {
      cli::cli_warn(c(
        "Could not extract title from {.path {fs::path_file(filepath)}}",
        "i" = "Using filename instead"
      ))
      filename_without_ext
    }
  )

  # Fallback to filename if title is empty or just whitespace
  if (is.null(title) || trimws(title) == "") {
    title <- filename_without_ext
  }

  title
}


#' Escape Markdown Special Characters
#'
#' @param text Character string to escape
#' @return Escaped character string safe for markdown link text
#' @keywords internal
escape_markdown <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return("")
  }
  # Escape characters that have special meaning in markdown link text
  text <- gsub("[", "&#91;", text, fixed = TRUE)
  text <- gsub("]", "&#93;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("`", "&#96;", text, fixed = TRUE)
  text
}


#' Escape Markdown Link Path
#'
#' @param path Character string path to escape
#' @return Escaped path safe for markdown link URLs
#' @keywords internal
escape_markdown_link <- function(path) {
  if (is.null(path) || length(path) == 0) {
    return("")
  }
  # Escape characters that have special meaning in markdown link URLs
  # Brackets need to be percent-encoded
  path <- gsub("[", "%5B", path, fixed = TRUE)
  path <- gsub("]", "%5D", path, fixed = TRUE)
  # Parentheses need to be percent-encoded
  path <- gsub("(", "%28", path, fixed = TRUE)
  path <- gsub(")", "%29", path, fixed = TRUE)
  # Spaces should be percent-encoded for URLs
  path <- gsub(" ", "%20", path, fixed = TRUE)
  path
}


#' Extract Title from DOCX File
#'
#' @param filepath Path to DOCX file
#' @return Character string with title or NULL
#' @keywords internal
extract_docx_title <- function(filepath) {
  doc <- officer::read_docx(filepath)
  props <- officer::doc_properties(doc)

  # Extract title from data frame structure
  title <- if ("title" %in% props$tag) {
    props[props$tag == "title", "value"][1]
  } else if ("subtitle" %in% props$tag) {
    props[props$tag == "subtitle", "value"][1]
  } else {
    NULL
  }

  title
}


#' Extract Title from PPTX File
#'
#' @param filepath Path to PPTX file
#' @return Character string with title or NULL
#' @keywords internal
extract_pptx_title <- function(filepath) {
  pres <- officer::read_pptx(filepath)
  props <- officer::doc_properties(pres)

  # Extract title from data frame structure
  title <- if ("title" %in% props$tag) {
    props[props$tag == "title", "value"][1]
  } else if ("subtitle" %in% props$tag) {
    props[props$tag == "subtitle", "value"][1]
  } else {
    NULL
  }

  title
}


#' Extract Title from PDF File
#'
#' @param filepath Path to PDF file
#' @return Character string with title or NULL
#' @keywords internal
extract_pdf_title <- function(filepath) {
  # pdftools is optional (Suggests), warning handled at call site
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    return(NULL)
  }

  info <- pdftools::pdf_info(filepath)
  title <- info$keys$Title %||% NULL

  title
}
