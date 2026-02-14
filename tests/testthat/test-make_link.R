testthat::test_that("make_link generates correct link for default settings", {
  result <- saros::make_link(mtcars, folder = tempdir())
  testthat::expect_true(grepl(
    x = as.character(result),
    "\\[download figure data\\]\\(.+/d0487363db4e6cc64fdb740cb6617fc0\\.csv\\)$"
  ))
})

testthat::test_that("make_link generates correct link with custom prefix and suffix", {
  result <- saros::make_link(
    mtcars,
    folder = tempdir(),
    file_prefix = "data_",
    file_suffix = ".txt",
    link_prefix = "[clicking me](",
    link_suffix = ") is the way to go"
  )
  testthat::expect_true(grepl(
    x = as.character(result),
    "\\[clicking me\\]\\(.+/data_d0487363db4e6cc64fdb740cb6617fc0\\.txt\\) is the way to go"
  ))
})

testthat::test_that("make_link uses custom save function", {
  result <- saros::make_link(
    mtcars,
    folder = tempdir(),
    save_fn = saveRDS,
    file_suffix = ".rds"
  )
  testthat::expect_true(grepl(
    x = as.character(result),
    "\\[download figure data\\]\\(.+/d0487363db4e6cc64fdb740cb6617fc0\\.rds\\)$"
  ))
})


testthat::test_that("make_link generates unique filename based on object hash", {
  hash1 <- rlang::hash(mtcars)
  hash2 <- rlang::hash(iris)
  result1 <- saros::make_link(mtcars, folder = tempdir())
  result2 <- saros::make_link(iris, folder = tempdir())
  testthat::expect_true(grepl(paste0(hash1, "\\.csv"), result1))
  testthat::expect_true(grepl(paste0(hash2, "\\.csv"), result2))
})

testthat::test_that("make_link creates file in specified folder", {
  path <- fs::path(tempdir(), paste0(rlang::hash(mtcars), ".csv"))
  if (fs::file_exists(path)) {
    fs::file_delete(path)
  }
  saros::make_link(mtcars, folder = tempdir())
  testthat::expect_true(fs::file_exists(path))
})


testthat::test_that("make_link throws error if save function fails", {
  save_fn <- function(data, path) stop("Intentional error")
  testthat::expect_error(saros::make_link(
    mtcars[1:8, ],
    folder = tempdir(),
    save_fn = save_fn
  ))
})

testthat::test_that("make_link respects save_fn argument and ignores global settings", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("withr")

  test_data <- data.frame(x = 1:5, y = 6:10)

  withr::with_tempdir({
    test_folder <- file.path(getwd(), "make_link_test")
    dir.create(test_folder, showWarnings = FALSE)

    # Global save_fn writes "global"
    global_save_fn <- function(data, path) {
      writeLines("global", path)
    }

    # Direct save_fn writes "direct"
    direct_save_fn <- function(data, path) {
      writeLines("direct", path)
    }

    # Set global settings for make_link
    saros::global_settings_set(
      fn_name = "make_link",
      new = list(
        folder = test_folder,
        save_fn = global_save_fn,
        file_suffix = ".txt"
      ),
      quiet = TRUE
    )

    # Call make_link with explicit save_fn - should use direct, not global
    result <- saros::make_link(
      test_data,
      folder = test_folder,
      save_fn = direct_save_fn,
      file_suffix = ".txt"
    )

    testthat::expect_type(result, "character")
    testthat::expect_s3_class(result, "AsIs")

    # Check that direct function was used
    files <- list.files(test_folder, pattern = "\\.txt$", full.names = TRUE)
    testthat::expect_true(length(files) >= 1)
    testthat::expect_equal(readLines(files[1]), "direct")

    # Reset
    saros::global_settings_reset(
      fn_name = "make_link",
      quiet = TRUE
    )
  })
})
