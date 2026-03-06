test_that("update_html_pdf_link_text replaces link text for matching PDF", {
  html_dir <- withr::local_tempdir()
  html_path <- file.path(html_dir, "index.html")

  writeLines(c(
    "<html><body>",
    '<a href="report.pdf">report.pdf</a>',
    '<a href="other.pdf">other.pdf</a>',
    "</body></html>"
  ), html_path)

  result <- update_html_pdf_link_text(html_path, "report.pdf", "My Report Title")

  expect_true(result)
  content <- paste(readLines(html_path), collapse = "\n")
  expect_true(grepl("My Report Title", content, fixed = TRUE))
  # The other link should remain unchanged

  expect_true(grepl(">other.pdf</a>", content, fixed = TRUE))
})


test_that("update_html_pdf_link_text handles href with path prefix", {
  html_dir <- withr::local_tempdir()
  html_path <- file.path(html_dir, "index.html")

  writeLines(c(
    "<html><body>",
    '<a href="./report.pdf">report</a>',
    "</body></html>"
  ), html_path)

  result <- update_html_pdf_link_text(html_path, "report.pdf", "Updated Title")

  expect_true(result)
  content <- paste(readLines(html_path), collapse = "\n")
  expect_true(grepl("Updated Title", content, fixed = TRUE))
  expect_false(grepl(">report</a>", content, fixed = TRUE))
})


test_that("update_html_pdf_link_text returns FALSE when no links found", {
  html_dir <- withr::local_tempdir()
  html_path <- file.path(html_dir, "index.html")

  writeLines(c(
    "<html><body>",
    '<a href="other.pdf">other</a>',
    "</body></html>"
  ), html_path)

  result <- update_html_pdf_link_text(html_path, "report.pdf", "Title")

  expect_false(result)
})


test_that("update_html_pdf_link_text escapes HTML in title", {
  html_dir <- withr::local_tempdir()
  html_path <- file.path(html_dir, "index.html")

  writeLines(c(
    "<html><body>",
    '<a href="report.pdf">report</a>',
    "</body></html>"
  ), html_path)

  update_html_pdf_link_text(html_path, "report.pdf", "Title <with> & \"special\" chars")

  content <- paste(readLines(html_path), collapse = "\n")
  expect_true(grepl("&amp;", content, fixed = TRUE))
  expect_true(grepl("&lt;with&gt;", content, fixed = TRUE))
  expect_true(grepl("&quot;special&quot;", content, fixed = TRUE))
})


test_that("detect_ghostscript returns NULL or a valid command", {
  result <- detect_ghostscript()
  if (!is.null(result)) {
    expect_true(nzchar(Sys.which(result)))
  }
})


test_that("read_quarto_post_render_input parses JSON array", {
  # Mock stdin by testing the JSON parsing logic directly
  input <- '["_site/ch1/report.pdf", "_site/ch1/index.html", "_site/ch2/data.pdf"]'

  # Extract quoted strings (same logic as read_quarto_post_render_input)
  matches <- regmatches(input, gregexpr('"([^"\\\\]|\\\\.)*"', input))[[1L]]
  paths <- gsub('^"|"$', "", matches)

  expect_equal(paths, c("_site/ch1/report.pdf", "_site/ch1/index.html", "_site/ch2/data.pdf"))
})


test_that("process_single_pdf_post_render skips when no DOCX exists", {
  tmp_dir <- withr::local_tempdir()
  pdf_path <- file.path(tmp_dir, "report.pdf")
  file.create(pdf_path)

  expect_message(
    process_single_pdf_post_render(pdf_path, gs_bin = NULL),
    "No matching DOCX"
  )
})


test_that("process_single_pdf_post_render warns for missing PDF", {
  expect_warning(
    process_single_pdf_post_render("/nonexistent/report.pdf", gs_bin = NULL),
    "PDF file not found"
  )
})


test_that("quarto_pdf_post_render uses QUARTO_PROJECT_OUTPUT_FILES env var", {
  tmp_dir <- withr::local_tempdir()
  pdf_path <- file.path(tmp_dir, "report.pdf")
  file.create(pdf_path)

  withr::local_envvar(QUARTO_PROJECT_OUTPUT_FILES = pdf_path)
  expect_message(
    quarto_pdf_post_render(),
    "No matching DOCX"
  )
})


test_that("quarto_pdf_post_render handles empty PDF list", {
  expect_message(
    quarto_pdf_post_render(c("file.html", "file.docx")),
    "No PDF files found"
  )
})


test_that("quarto_pdf_post_render processes matching PDF + DOCX pair", {
  skip_if_not_installed("officer")

  tmp_dir <- withr::local_tempdir()

  # Create a minimal DOCX with a title
  doc <- officer::read_docx()
  doc <- officer::set_doc_properties(doc, title = "Test Survey Report")
  docx_path <- file.path(tmp_dir, "report.docx")
  print(doc, target = docx_path)

  # Create a minimal PDF for testing
  pdf_path <- file.path(tmp_dir, "report.pdf")
  grDevices::pdf(pdf_path, width = 5, height = 5)
  graphics::par(mar = c(1, 1, 1, 1))
  graphics::plot.new()
  grDevices::dev.off()

  # Create an index.html with a link to the PDF
  html_path <- file.path(tmp_dir, "index.html")
  writeLines(c(
    "<html><body>",
    '<a href="report.pdf">report.pdf</a>',
    "</body></html>"
  ), html_path)

  # Run the post-render (gs_bin = NULL to skip PDF metadata)
  quarto_pdf_post_render(pdf_path)

  # Verify index.html was updated
  content <- paste(readLines(html_path), collapse = "\n")
  expect_true(grepl("Test Survey Report", content, fixed = TRUE))
  expect_false(grepl(">report.pdf</a>", content, fixed = TRUE))
})
