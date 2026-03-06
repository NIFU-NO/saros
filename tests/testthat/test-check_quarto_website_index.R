test_that("detects folders missing index.qmd", {
  dir <- withr::local_tempdir()

  # Rapporter/index.qmd
  dir.create(file.path(dir, "Rapporter"), recursive = TRUE)
  writeLines("", file.path(dir, "Rapporter", "index.qmd"))

  # Rapporter/2025/GroupA/index.qmd + about.qmd
  dir.create(file.path(dir, "Rapporter", "2025", "GroupA"), recursive = TRUE)
  writeLines("", file.path(dir, "Rapporter", "2025", "GroupA", "index.qmd"))
  writeLines("", file.path(dir, "Rapporter", "2025", "GroupA", "about.qmd"))

  # Rapporter/2025/GroupB/about.qmd (missing index.qmd)
  dir.create(file.path(dir, "Rapporter", "2025", "GroupB"), recursive = TRUE)
  writeLines("", file.path(dir, "Rapporter", "2025", "GroupB", "about.qmd"))

  result <- check_quarto_website_index(
    file.path(dir, "Rapporter"),
    quiet = TRUE
  )

  expect_true("2025" %in% result)
  expect_true(any(grepl("GroupB", result)))
  expect_false(any(grepl("GroupA", result)))
})

test_that("returns empty character when all folders have index.qmd", {
  dir <- withr::local_tempdir()

  dir.create(file.path(dir, "site"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "index.qmd"))

  dir.create(file.path(dir, "site", "chapter1"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "chapter1", "index.qmd"))
  writeLines("", file.path(dir, "site", "chapter1", "intro.qmd"))

  result <- check_quarto_website_index(file.path(dir, "site"), quiet = TRUE)
  expect_length(result, 0L)
})

test_that("excludes folders starting with _ or .", {
  dir <- withr::local_tempdir()

  dir.create(file.path(dir, "site"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "index.qmd"))

  # _freeze and .quarto should be ignored even without index.qmd
  dir.create(file.path(dir, "site", "_freeze"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "_freeze", "report.qmd"))

  dir.create(file.path(dir, "site", ".quarto"), recursive = TRUE)
  writeLines("", file.path(dir, "site", ".quarto", "report.qmd"))

  result <- check_quarto_website_index(file.path(dir, "site"), quiet = TRUE)
  expect_length(result, 0L)
})

test_that("ignores folders with no qmd files", {
  dir <- withr::local_tempdir()

  dir.create(file.path(dir, "site"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "index.qmd"))

  # A folder with only non-qmd files should not be flagged
  dir.create(file.path(dir, "site", "data"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "data", "notes.txt"))

  result <- check_quarto_website_index(file.path(dir, "site"), quiet = TRUE)
  expect_length(result, 0L)
})

test_that("issues cli warning when quiet = FALSE", {
  dir <- withr::local_tempdir()

  dir.create(file.path(dir, "site", "chapter"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "chapter", "page.qmd"))

  expect_warning(
    check_quarto_website_index(file.path(dir, "site"), quiet = FALSE),
    "index\\.qmd"
  )
})

test_that("checks root directory for index.qmd", {
  dir <- withr::local_tempdir()

  dir.create(file.path(dir, "site"), recursive = TRUE)
  # Root has a qmd but no index.qmd
  writeLines("", file.path(dir, "site", "about.qmd"))

  result <- check_quarto_website_index(file.path(dir, "site"), quiet = TRUE)
  expect_true("." %in% result)
})

test_that("does not flag parent folder when qmd files only exist in excluded subfolders", {
  dir <- withr::local_tempdir()

  dir.create(file.path(dir, "site"), recursive = TRUE)

  # Only .qmd files are inside _freeze — parent should NOT be flagged

  dir.create(file.path(dir, "site", "_freeze"), recursive = TRUE)
  writeLines("", file.path(dir, "site", "_freeze", "cached.qmd"))

  dir.create(file.path(dir, "site", ".quarto"), recursive = TRUE)
  writeLines("", file.path(dir, "site", ".quarto", "internal.qmd"))

  result <- check_quarto_website_index(file.path(dir, "site"), quiet = TRUE)
  expect_length(result, 0L)
})
