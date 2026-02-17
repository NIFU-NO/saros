#' Create Markdown Links to Files with Document Titles
#'
#' @description
#' Scans a folder for files matching a pattern and generates a markdown list
#' with links to each file. The link text is extracted from the document's title
#' metadata (for DOCX, PPTX, PDF files) or uses the filename as fallback.
#'
#' @param folder String. Path to the folder containing files. Defaults to current
#'   directory (`"."`).
#' @param pattern String. Glob pattern for file matching (e.g., `"*.pptx"`,
#'   `"*.pdf"`, `"report_*.docx"`). Defaults to `"*"` (all files).
#' @param bullet_style String. Markdown list style. One of `"-"` (unordered),
#'   `"*"` (unordered), or `"1."` (ordered). Defaults to `"-"`.
#' @param recurse Logical. Whether to search recursively in subdirectories.
#'   Defaults to `FALSE`.
#' @param relative_links Logical. Whether to use relative paths in links.
#'   If `TRUE`, paths are relative to `folder`. If `FALSE`, uses basename only.
#'   Defaults to `TRUE`.
#'
#' @return A character string containing a markdown-formatted list of links.
#'   Each line contains a bullet point and a link with the document title as
#'   link text. Returns empty string if no files found.
#'
#' @details
#' The function attempts to extract document titles from file metadata:
#' - **DOCX files**: Extracts title from document properties using `officer`
#' - **PPTX files**: Extracts title from presentation properties using `officer`
#' - **PDF files**: Extracts title from PDF metadata using `pdftools`
#'
#' If title extraction fails or the file type is unsupported, the filename
#' (without extension) is used as the link text.
#'
#' The function preserves the order of files as returned by `fs::dir_ls()`,
#' which typically sorts alphabetically.
#'
#' @section Dependencies:
#' - Requires `officer` package for DOCX/PPTX title extraction
#' - Requires `pdftools` package for PDF title extraction
#' - These packages are suggested dependencies and not required if you're
#'   only linking to files without title extraction
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create links to all PowerPoint files in a folder
#' make_file_links(folder = "presentations", pattern = "*.pptx")
#'
#' # Create links to PDF reports with numbered list
#' make_file_links(
#'   folder = "reports",
#'   pattern = "report_*.pdf",
#'   bullet_style = "1."
#' )
#'
#' # Recursively find all Office documents
#' make_file_links(
#'   folder = "documents",
#'   pattern = "*.{docx,pptx}",
#'   recurse = TRUE
#' )
#' }
make_file_links <- function(
  folder = ".",
  pattern = "*",
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
    glob = pattern,
    type = "file",
    recurse = recurse
  )

  if (length(files) == 0) {
    cli::cli_warn(c(
      "No files found matching pattern {.val {pattern}} in {.path {folder}}"
    ))
    return("")
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
        link_path <- fs::path_file(filepath)
      }

      # Format as markdown list item
      if (bullet_style == "1.") {
        # Numbered lists need index, will be added later
        sprintf("[%s](%s)", title, link_path)
      } else {
        sprintf("%s\t[%s](%s)", bullet_style, title, link_path)
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

  title <- tryCatch(
    {
      if (ext == "docx") {
        extract_docx_title(filepath)
      } else if (ext == "pptx") {
        extract_pptx_title(filepath)
      } else if (ext == "pdf") {
        extract_pdf_title(filepath)
      } else {
        filename_without_ext
      }
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


#' Extract Title from DOCX File
#'
#' @param filepath Path to DOCX file
#' @return Character string with title or NULL
#' @keywords internal
extract_docx_title <- function(filepath) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    cli::cli_warn(c(
      "{.pkg officer} package required to extract DOCX titles",
      "i" = "Install with: {.code install.packages('officer')}"
    ))
    return(NULL)
  }

  doc <- officer::read_docx(filepath)
  props <- officer::doc_properties(doc)

  # Try different title fields
  title <- props$title %||% props$subject %||% NULL

  title
}


#' Extract Title from PPTX File
#'
#' @param filepath Path to PPTX file
#' @return Character string with title or NULL
#' @keywords internal
extract_pptx_title <- function(filepath) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    cli::cli_warn(c(
      "{.pkg officer} package required to extract PPTX titles",
      "i" = "Install with: {.code install.packages('officer')}"
    ))
    return(NULL)
  }

  pres <- officer::read_pptx(filepath)
  props <- officer::doc_properties(pres)

  # Try different title fields
  title <- props$title %||% props$subject %||% NULL

  title
}


#' Extract Title from PDF File
#'
#' @param filepath Path to PDF file
#' @return Character string with title or NULL
#' @keywords internal
extract_pdf_title <- function(filepath) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    cli::cli_warn(c(
      "{.pkg pdftools} package required to extract PDF titles",
      "i" = "Install with: {.code install.packages('pdftools')}"
    ))
    return(NULL)
  }

  info <- pdftools::pdf_info(filepath)
  title <- info$keys$Title %||% NULL

  title
}
