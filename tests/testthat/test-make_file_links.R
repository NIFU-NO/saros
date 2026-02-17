test_that("make_file_links creates markdown list for files", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    # Create test folder structure
    test_folder <- "test_docs"
    dir.create(test_folder)

    # Create some test files
    file.create(file.path(test_folder, "file1.txt"))
    file.create(file.path(test_folder, "file2.txt"))
    file.create(file.path(test_folder, "report.pdf"))

    # Test basic functionality
    result <- make_file_links(folder = test_folder, pattern = "\\.txt$")

    expect_type(result, "character")
    expect_true(grepl("file1", result))
    expect_true(grepl("file2", result))
    expect_false(grepl("report.pdf", result))

    # Test markdown formatting
    expect_true(grepl("^-\\t\\[", result))
    expect_true(grepl("\\]\\(", result))
    expect_true(grepl("\n", result))
  })
})

test_that("make_file_links validates inputs", {
  expect_error(
    make_file_links(folder = NULL),
    "must be a string"
  )

  expect_error(
    make_file_links(bullet_style = "invalid"),
    "must be one of"
  )

  expect_error(
    make_file_links(folder = "nonexistent_folder_xyz"),
    "Folder does not exist"
  )
})

test_that("make_file_links handles different bullet styles", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    test_folder <- "docs"
    dir.create(test_folder)
    file.create(file.path(test_folder, "file1.txt"))
    file.create(file.path(test_folder, "file2.txt"))

    # Test dash (default)
    result_dash <- make_file_links(folder = test_folder, bullet_style = "-")
    expect_true(grepl("^-\\t", result_dash))

    # Test asterisk
    result_ast <- make_file_links(folder = test_folder, bullet_style = "*")
    expect_true(grepl("^\\*\\t", result_ast))

    # Test numbered
    result_num <- make_file_links(folder = test_folder, bullet_style = "1.")
    expect_true(grepl("^1\\.\\t", result_num))
    expect_true(grepl("\n2\\.\\t", result_num))
  })
})

test_that("make_file_links warns when no files found", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    test_folder <- "empty"
    dir.create(test_folder)

    expect_warning(
      result <- make_file_links(folder = test_folder, pattern = "\\.xyz$"),
      "No files found"
    )
    expect_equal(result, "")
  })
})

test_that("make_file_links handles recursive search", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    # Create nested structure
    dir.create("docs")
    dir.create("docs/subfolder")
    file.create("docs/file1.txt")
    file.create("docs/subfolder/file2.txt")

    # Non-recursive should only find file1
    result_non_rec <- make_file_links(
      folder = "docs",
      pattern = "\\.txt$",
      recurse = FALSE
    )
    expect_true(grepl("file1", result_non_rec))
    expect_false(grepl("file2", result_non_rec))

    # Recursive should find both
    result_rec <- make_file_links(
      folder = "docs",
      pattern = "\\.txt$",
      recurse = TRUE
    )
    expect_true(grepl("file1", result_rec))
    expect_true(grepl("file2", result_rec))
  })
})

test_that("make_file_links handles relative_links parameter", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    dir.create("docs")
    file.create("docs/report.txt")

    # With relative links (default)
    result_rel <- make_file_links(folder = "docs", relative_links = TRUE)
    expect_true(grepl("\\[report\\]\\(report\\.txt\\)", result_rel))

    # With absolute links
    result_abs <- make_file_links(folder = "docs", relative_links = FALSE)
    # Should contain full path
    expect_true(grepl("\\[report\\]\\(", result_abs))
    expect_true(
      grepl("docs", result_abs) ||
        grepl("/", result_abs) ||
        grepl("\\\\", result_abs)
    )
  })
})

test_that("extract_document_title falls back to filename for plain files", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    filepath <- "test_file.txt"
    file.create(filepath)

    title <- saros:::extract_document_title(filepath)
    expect_equal(title, "test_file")
  })
})

test_that("extract_document_title handles missing packages gracefully", {
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("pdftools")

  withr::with_tempdir({
    # Create a fake PDF file (won't have valid metadata)
    filepath <- "test.pdf"
    file.create(filepath)

    # Should warn about extraction failure and fall back to filename
    expect_warning(
      title <- saros:::extract_document_title(filepath),
      "Could not extract title"
    )

    # Should still return something (filename as fallback)
    expect_type(title, "character")
    expect_true(nchar(title) > 0)
    expect_equal(title, "test")
  })
})

test_that("make_file_links works with DOCX files if officer is available", {
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("officer")

  withr::with_tempdir({
    test_folder <- "word_docs"
    dir.create(test_folder)

    # Create a DOCX with title
    doc <- officer::read_docx()
    doc <- officer::set_doc_properties(
      doc,
      title = "My Test Document",
      subject = "Testing",
      creator = "Test Suite"
    )
    docx_path <- file.path(test_folder, "test_doc.docx")
    print(doc, target = docx_path)

    result <- make_file_links(folder = test_folder, pattern = "\\.docx$")

    expect_type(result, "character")
    expect_true(grepl("My Test Document", result) || grepl("test_doc", result))
    expect_true(grepl("test_doc\\.docx", result))
  })
})

test_that("make_file_links works with PPTX files if officer is available", {
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("officer")

  withr::with_tempdir({
    test_folder <- "presentations"
    dir.create(test_folder)

    # Create a PPTX with title
    pres <- officer::read_pptx()
    pres <- officer::set_doc_properties(
      pres,
      title = "My Presentation",
      subject = "Test Presentation"
    )
    pptx_path <- file.path(test_folder, "slides.pptx")
    print(pres, target = pptx_path)

    result <- make_file_links(folder = test_folder, pattern = "\\.pptx$")

    expect_type(result, "character")
    expect_true(grepl("My Presentation", result) || grepl("slides", result))
    expect_true(grepl("slides\\.pptx", result))
  })
})

test_that("make_file_links handles empty or whitespace titles", {
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("officer")

  withr::with_tempdir({
    test_folder <- "docs"
    dir.create(test_folder)

    # Create a DOCX with empty title
    doc <- officer::read_docx()
    doc <- officer::set_doc_properties(doc, title = "   ", subject = "")
    docx_path <- file.path(test_folder, "empty_title.docx")
    print(doc, target = docx_path)

    result <- make_file_links(folder = test_folder, pattern = "\\.docx$")

    # Should fall back to filename
    expect_true(grepl("empty_title", result))
  })
})

test_that("make_file_links formats output correctly for single file", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    dir.create("docs")
    file.create("docs/single.txt")

    result <- make_file_links(folder = "docs")

    # Should not have trailing newline for single file
    expect_false(grepl("\n$", result))
    expect_true(grepl("^-\\t\\[single\\]\\(single\\.txt\\)$", result))
  })
})

test_that("escape_markdown escapes special characters", {
  # Test bracket escaping
  expect_equal(saros:::escape_markdown("[test]"), "&#91;test&#93;")

  # Test angle bracket escaping
  expect_equal(saros:::escape_markdown("<script>"), "&lt;script&gt;")

  # Test backtick escaping
  expect_equal(saros:::escape_markdown("`code`"), "&#96;code&#96;")

  # Test NULL handling
  expect_equal(saros:::escape_markdown(NULL), "")

  # Test empty string
  expect_equal(saros:::escape_markdown(""), "")
})

test_that("escape_markdown_link escapes URL special characters", {
  # Test brackets escaping
  expect_equal(saros:::escape_markdown_link("file[1].pdf"), "file%5B1%5D.pdf")

  # Test parentheses escaping
  expect_equal(saros:::escape_markdown_link("file(1).pdf"), "file%281%29.pdf")

  # Test space escaping
  expect_equal(saros:::escape_markdown_link("my file.pdf"), "my%20file.pdf")

  # Test combined
  expect_equal(
    saros:::escape_markdown_link("my [test] file.pdf"),
    "my%20%5Btest%5D%20file.pdf"
  )

  # Test NULL handling
  expect_equal(saros:::escape_markdown_link(NULL), "")
})

test_that("make_file_links escapes malicious filenames and titles", {
  skip_on_cran()
  skip_if_not_installed("withr")

  withr::with_tempdir({
    dir.create("docs")
    # Create file with special characters in name
    malicious_file <- "docs/[injection].txt"
    file.create(malicious_file)

    result <- make_file_links(folder = "docs")

    # Should escape brackets in title (HTML entities)
    expect_true(grepl("&#91;injection&#93;", result))
    # Should escape brackets in path (percent-encoding)
    expect_true(grepl("%5Binjection%5D\\.txt", result))
    # Should NOT have unescaped brackets in the path portion
    expect_false(grepl("]\\(\\[injection\\]", result))
  })
})
