#' Quarto Post-Render: Enrich PDF Files with DOCX Titles
#'
#' @description
#' A post-render function for Quarto projects that processes rendered PDF
#' output files. For each PDF, it checks if a corresponding DOCX file with
#' the same base name exists, extracts the title from the DOCX document
#' properties, sets it as the PDF metadata title, and updates the link text
#' in the accompanying `index.html`.
#'
#' @param output_files Character vector of output file paths from Quarto.
#'   Defaults to the `QUARTO_PROJECT_OUTPUT_FILES` environment variable
#'   (newline-separated paths set by Quarto during project render). If
#'   that variable is empty, falls back to reading from stdin as provided
#'   by Quarto's post-render hook.
#'
#' @return Invisible `NULL`. Called for side effects.
#'
#' @details
#' To use as a Quarto post-render script, add to `_quarto.yml`:
#'
#' ```yaml
#' project:
#'   post-render:
#'     - "Rscript -e 'saros::quarto_pdf_post_render()'"
#' ```
#'
#' **Processing steps for each `.pdf` file:**
#' 1. Checks if a `.docx` with the same base name exists in the same directory
#' 2. Extracts the title from the DOCX document properties (via `officer`)
#' 3. Sets the extracted title as the PDF file's metadata title (requires
#'    Ghostscript)
#' 4. Locates `index.html` in the same directory as the PDF
#' 5. Replaces the `<a>` link text for that PDF with the extracted title
#'
#' @section System Requirements:
#' Setting PDF metadata title requires Ghostscript to be installed and
#' available on the system PATH:
#' - **Linux/macOS**: `gs`
#' - **Windows**: `gswin64c` or `gswin32c`
#'
#' If Ghostscript is not found, a warning is issued and only the HTML link
#' text update is performed.
#'
#' @seealso [extract_docx_title()] for the DOCX title extraction logic.
#'
#' @export
#' @examples
#' \dontrun{
#' # Called automatically by Quarto post-render, or manually:
#' quarto_pdf_post_render(c("_site/report/report.pdf"))
#' }
quarto_pdf_post_render <- function(
  output_files = strsplit(
    Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"),
    "\n"
  )[[1L]]
) {
  if (length(output_files) == 0L || all(output_files == "")) {
    # Avoid blocking on stdin in interactive / TTY contexts where Quarto
    # is not piping output file paths.
    if (interactive() || isatty(stdin())) {
      cli::cli_inform(c(
        "No Quarto output files were detected from the environment or stdin.",
        "i" = "When calling {.fn quarto_pdf_post_render} interactively,",
        "i" = "please provide {.arg output_files} explicitly."
      ))
      return(invisible(NULL))
    }
    output_files <- read_quarto_post_render_input()
  }

  # Keep only PDF files
  pdf_files <- output_files[grepl("\\.pdf$", output_files, ignore.case = TRUE)]

  if (length(pdf_files) == 0L) {
    cli::cli_inform("No PDF files found in Quarto output.")
    return(invisible(NULL))
  }

  gs_bin <- detect_ghostscript()
  if (is.null(gs_bin)) {
    cli::cli_warn(c(
      "Ghostscript not found on PATH.",
      "i" = "PDF metadata titles will not be set.",
      "i" = "Install Ghostscript for full functionality."
    ))
  }

  for (pdf_path in pdf_files) {
    process_single_pdf_post_render(pdf_path, gs_bin)
  }

  invisible(NULL)
}


#' Process a Single PDF for Post-Render Title Enrichment
#'
#' @param pdf_path Path to the PDF file.
#' @param gs_bin Ghostscript binary name, or `NULL` if not available.
#' @return Invisible `NULL`.
#' @keywords internal
process_single_pdf_post_render <- function(pdf_path, gs_bin) {
  pdf_path <- normalizePath(pdf_path, mustWork = FALSE)

  if (!file.exists(pdf_path)) {
    cli::cli_warn("PDF file not found: {.path {pdf_path}}")
    return(invisible(NULL))
  }

  # Check for corresponding DOCX with same base name (case-insensitive)
  docx_path <- find_matching_docx(pdf_path)

  if (is.null(docx_path)) {
    cli::cli_inform(
      "No matching DOCX for {.path {basename(pdf_path)}}, skipping."
    )
    return(invisible(NULL))
  }

  # Extract title from DOCX
  title <- tryCatch(
    extract_docx_title(docx_path),
    error = function(e) {
      cli::cli_warn(
        "Failed to extract title from {.path {basename(docx_path)}}: {e$message}"
      )
      NULL
    }
  )

  if (is.null(title) || trimws(title) == "") {
    cli::cli_inform(
      "Empty title in {.path {basename(docx_path)}}, skipping."
    )
    return(invisible(NULL))
  }

  cli::cli_inform(
    "Processing {.path {basename(pdf_path)}} with title {.val {title}}"
  )

  # Set PDF metadata title via Ghostscript
  if (!is.null(gs_bin)) {
    set_pdf_metadata_title(pdf_path, title, gs_bin)
  }

  # Update link text in index.html
  index_html <- file.path(dirname(pdf_path), "index.html")
  if (file.exists(index_html)) {
    update_html_pdf_link_text(index_html, basename(pdf_path), title)
  } else {
    cli::cli_inform("No index.html found in {.path {dirname(pdf_path)}}")
  }

  invisible(NULL)
}


#' Read Quarto Post-Render Input from stdin
#'
#' Parses the JSON array of output file paths that Quarto passes to
#' post-render scripts via stdin.
#'
#' @return Character vector of file paths.
#' @keywords internal
read_quarto_post_render_input <- function() {
  input <- paste(readLines(stdin(), warn = FALSE), collapse = "")
  input <- trimws(input)

  if (nchar(input) == 0L) {
    return(character(0L))
  }

  # Quarto passes a JSON array: ["file1.pdf", "file2.html", ...]
  if (startsWith(input, "[")) {
    matches <- regmatches(input, gregexpr('"([^"\\\\]|\\\\.)*"', input))[[1L]]
    paths <- gsub('^"|"$', "", matches)
    # Unescape JSON string escapes
    paths <- gsub("\\\\(.)", "\\1", paths, perl = TRUE)
    return(paths)
  }

  # Fallback: newline-separated paths
  strsplit(input, "\n")[[1L]]
}


#' Find a Matching DOCX File for a PDF (Case-Insensitive)
#'
#' Looks in the same directory as the PDF for a `.docx` file with the same
#' base name, matching case-insensitively on the extension.
#'
#' @param pdf_path Path to the PDF file.
#' @return Path to the matching DOCX file, or `NULL` if not found.
#' @keywords internal
find_matching_docx <- function(pdf_path) {
  dir_path <- dirname(pdf_path)
  base_name <- fs::path_ext_remove(basename(pdf_path))

  # List all files in the directory and find a .docx match (case-insensitive)
  candidates <- list.files(dir_path, full.names = TRUE)
  for (f in candidates) {
    f_base <- fs::path_ext_remove(basename(f))
    f_ext <- tolower(fs::path_ext(f))
    if (f_ext == "docx" && tolower(f_base) == tolower(base_name)) {
      return(f)
    }
  }
  NULL
}


#' Detect Ghostscript Binary on System PATH
#'
#' @return String with the Ghostscript command name, or `NULL` if not found.
#' @keywords internal
detect_ghostscript <- function() {
  candidates <- if (.Platform$OS.type == "windows") {
    c("gswin64c", "gswin32c", "gs")
  } else {
    "gs"
  }

  for (cmd in candidates) {
    if (nzchar(Sys.which(cmd))) {
      return(cmd)
    }
  }

  NULL
}


#' Set PDF Metadata Title Using Ghostscript
#'
#' Overwrites the PDF file with an updated version containing the specified
#' title in its document information dictionary. Uses a temporary PostScript
#' pdfmark file to avoid shell escaping issues.
#'
#' @param pdf_path Path to the PDF file (modified in place).
#' @param title Title string to embed.
#' @param gs_bin Ghostscript binary name or path.
#' @return Invisible logical; `TRUE` on success, `FALSE` on failure.
#' @keywords internal
set_pdf_metadata_title <- function(pdf_path, title, gs_bin) {
  temp_out <- tempfile(fileext = ".pdf")
  temp_marks <- tempfile(fileext = ".ps")
  on.exit(unlink(c(temp_out, temp_marks)), add = TRUE)

  # Convert text to a PDF Unicode string: UTF-16BE bytes with BOM, hex-encoded.
  # This avoids locale/codepage corruption for non-ASCII characters.
  to_pdf_unicode_hex <- function(x) {
    utf16_raw <- tryCatch(
      iconv(enc2utf8(x), from = "UTF-8", to = "UTF-16BE", toRaw = TRUE)[[1L]],
      error = function(e) NULL
    )

    if (is.null(utf16_raw)) {
      return(NULL)
    }

    bytes <- c(as.raw(0xFE), as.raw(0xFF), utf16_raw)
    hex <- paste(sprintf("%02X", as.integer(bytes)), collapse = "")
    sprintf("<%s>", hex)
  }

  # Normalize title: replace newlines/carriage returns with spaces and
  # strip other non-printable control characters to avoid invalid pdfmark
  title_clean <- gsub("[\r\n]+", " ", title)
  title_clean <- gsub("[[:cntrl:]]", "", title_clean)

  title_pdf <- to_pdf_unicode_hex(title_clean)
  if (is.null(title_pdf)) {
    cli::cli_warn("Failed to encode PDF title as Unicode.")
    return(invisible(FALSE))
  }

  # Write pdfmark to a temp PostScript file (avoids shell escaping issues)
  writeLines(
    sprintf("[/Title %s /DOCINFO pdfmark", title_pdf),
    temp_marks
  )

  args <- c(
    "-dBATCH",
    "-dNOPAUSE",
    "-dQUIET",
    "-sDEVICE=pdfwrite",
    paste0("-sOutputFile=", temp_out),
    pdf_path,
    temp_marks
  )

  result <- tryCatch(
    system2(gs_bin, args, stdout = TRUE, stderr = TRUE),
    error = function(e) {
      cli::cli_warn("Ghostscript invocation failed: {e$message}")
      NULL
    }
  )

  status <- attr(result, "status")
  if (
    !is.null(result) &&
      (is.null(status) || status == 0L) &&
      file.exists(temp_out)
  ) {
    copy_ok <- tryCatch(
      file.copy(temp_out, pdf_path, overwrite = TRUE),
      warning = function(e) FALSE,
      error = function(e) FALSE
    )
    if (isTRUE(copy_ok)) {
      cli::cli_inform("Set PDF title for {.path {basename(pdf_path)}}")
      return(invisible(TRUE))
    }
  }

  cli::cli_warn("Failed to set PDF title for {.path {basename(pdf_path)}}")
  invisible(FALSE)
}


#' Escape a String for Literal Use in a Regular Expression
#'
#' @param x Character string to escape.
#' @return String with regex metacharacters escaped.
#' @keywords internal
escape_for_regex <- function(x) {
  chars <- c(
    "\\",
    ".",
    "|",
    "(",
    ")",
    "{",
    "}",
    "[",
    "]",
    "^",
    "$",
    "*",
    "+",
    "?"
  )
  for (ch in chars) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x
}


#' Update Link Text for a PDF in an HTML File
#'
#' Finds `<a>` elements whose `href` points to the given PDF filename
#' and replaces their inner text with the specified title.
#'
#' @param html_path Path to the HTML file.
#' @param pdf_filename PDF filename (basename only, e.g. `"report.pdf"`).
#' @param title New link text.
#' @return Invisible logical; `TRUE` if replacements were made, `FALSE` otherwise.
#' @keywords internal
update_html_pdf_link_text <- function(html_path, pdf_filename, title) {
  lines <- readLines(html_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")

  # Escape pdf_filename for use in regex (use fixed replacements to avoid
  # regex engine issues with complex character classes)
  pdf_regex <- escape_for_regex(pdf_filename)

  # Match <a> tags whose href contains the PDF filename
  a_pattern <- sprintf(
    '(?s)(<a\\s[^>]*href\\s*=\\s*"[^"]*%s"[^>]*>)(.*?)(</a>)',
    pdf_regex
  )

  m <- gregexpr(a_pattern, content, perl = TRUE, ignore.case = TRUE)
  full_matches <- regmatches(content, m)[[1L]]

  if (length(full_matches) == 0L) {
    cli::cli_inform(
      "No links to {.path {pdf_filename}} found in {.path {basename(html_path)}}"
    )
    return(invisible(FALSE))
  }

  # HTML-escape the title for safe insertion
  title_html <- gsub("&", "&amp;", title, fixed = TRUE)
  title_html <- gsub("<", "&lt;", title_html, fixed = TRUE)
  title_html <- gsub(">", "&gt;", title_html, fixed = TRUE)
  title_html <- gsub('"', "&quot;", title_html, fixed = TRUE)

  for (match_str in full_matches) {
    m_parts <- regexec(a_pattern, match_str, perl = TRUE)
    parts <- regmatches(match_str, m_parts)[[1L]]
    # parts: [1] full match, [2] opening <a> tag, [3] old text, [4] </a>
    new_link <- paste0(parts[2L], title_html, " (PDF)", parts[4L])
    content <- sub(match_str, new_link, content, fixed = TRUE)
  }

  writeLines(strsplit(content, "\n", fixed = TRUE)[[1L]], html_path)
  cli::cli_inform(
    "Updated link text in {.path {basename(html_path)}} for {.path {pdf_filename}}"
  )
  invisible(TRUE)
}
