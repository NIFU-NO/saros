testthat::test_that("if no packages, shows nothing", {
  testthat::expect_snapshot(cat(saros:::saros_attach_message(character())))
})

testthat::test_that("message lists all core saros packages", {
  testthat:::local_mocked_bindings(package_version_h = function(x) "1.0.0")
  testthat::expect_snapshot(cat(saros:::saros_attach_message(core)))
})

testthat::test_that("highlights dev versions in red", {
  testthat:::local_reproducible_output(crayon = TRUE)

  testthat::expect_snapshot({
    saros:::highlight_version(c("1.0.0", "1.0.0.9000", "0.9000.0.9000", "1.0.0-rc"))
  })
})

# testthat::test_that("useful conflicts message", {
#   expect_snapshot({
#     saros:::saros_conflicts(c("base", "stats"))
#   })
# })


testthat::test_that("saros packages returns character vector of package names", {
  out <- saros:::saros_packages()
  testthat::expect_type(out, "character")
  testthat::expect_true("saros.base" %in% out)
})
