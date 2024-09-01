testthat::test_that("make_link generates correct link for default settings", {
  result <- saros::make_link(mtcars, folder = tempdir())
  testthat::expect_true(grepl(x = as.character(result), "\\[download figure data\\]\\(.+/d0487363db4e6cc64fdb740cb6617fc0\\.csv\\)$"))
})

testthat::test_that("make_link generates correct link with custom prefix and suffix", {
  result <- saros::make_link(mtcars, folder = tempdir(), file_prefix = "data_", file_suffix = ".txt",
                                      link_prefix = "[clicking me](", link_suffix = ") is the way to go")
  testthat::expect_true(grepl(x = as.character(result), "\\[clicking me\\]\\(.+/data_d0487363db4e6cc64fdb740cb6617fc0\\.txt\\) is the way to go"))
})

testthat::test_that("make_link uses custom save function", {
  result <- saros::make_link(mtcars, folder = tempdir(), save_fn = saveRDS, file_suffix=".rds")
  testthat::expect_true(grepl(x = as.character(result), "\\[download figure data\\]\\(.+/d0487363db4e6cc64fdb740cb6617fc0\\.rds\\)$"))
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
  if (fs::file_exists(path)) fs::file_delete(path)
  saros::make_link(mtcars, folder = tempdir())
  testthat::expect_true(fs::file_exists(path))
})


testthat::test_that("make_link throws error if save function fails", {
  save_fn <- function(data, path) stop("Intentional error")
  testthat::expect_error(saros::make_link(mtcars,
                                                   folder = tempdir(),
                                                   save_fn = save_fn))
})
