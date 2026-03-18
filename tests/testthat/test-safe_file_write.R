testthat::test_that("safe_file_write returns result invisibly on success", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  result <- withVisible(
    saros:::safe_file_write(utils::write.csv(mtcars, tmp), path = tmp)
  )
  testthat::expect_false(result$visible)
  testthat::expect_true(file.exists(tmp))
})

testthat::test_that("safe_file_write includes missing directory hint", {
  bad_path <- file.path(tempdir(), "nonexistent_dir_abc123", "file.csv")
  suppressWarnings(testthat::expect_error(
    saros:::safe_file_write(utils::write.csv(mtcars, bad_path), path = bad_path),
    regexp = "does not exist"
  ))
  suppressWarnings(testthat::expect_error(
    saros:::safe_file_write(utils::write.csv(mtcars, bad_path), path = bad_path),
    regexp = "Cannot save to"
  ))
})

testthat::test_that("safe_file_write includes long path hint", {
  long_path <- paste0(tempdir(), "/", paste(rep("a", 300), collapse = ""), ".csv")
  testthat::expect_error(
    saros:::safe_file_write(stop("simulated failure"), path = long_path),
    regexp = "characters long"
  )
  testthat::expect_error(
    saros:::safe_file_write(stop("simulated failure"), path = long_path),
    regexp = "SharePoint/OneDrive"
  )
})

testthat::test_that("safe_file_write includes OneDrive hint for OneDrive paths", {
  onedrive_path <- "C:/Users/test/OneDrive/some/file.csv"
  testthat::expect_error(
    saros:::safe_file_write(stop("simulated failure"), path = onedrive_path),
    regexp = "OneDrive/SharePoint"
  )
})

testthat::test_that("safe_file_write always includes write access hint", {
  bad_path <- file.path(tempdir(), "nonexistent_xyz", "file.csv")
  suppressWarnings(testthat::expect_error(
    saros:::safe_file_write(utils::write.csv(mtcars, bad_path), path = bad_path),
    regexp = "write access"
  ))
})

testthat::test_that("safe_file_write preserves parent error", {
  bad_path <- file.path(tempdir(), "nonexistent_xyz2", "file.csv")
  err <- suppressWarnings(tryCatch(
    saros:::safe_file_write(utils::write.csv(mtcars, bad_path), path = bad_path),
    error = function(e) e
  ))
  testthat::expect_s3_class(err$parent, "error")
})
