testthat::test_that("rename_by_labels", {
  suppressMessages(library(dplyr))
  library(labelled)
  testthat::expect_warning(object = {
  tmp <-
    mtcars %>%
    labelled::set_variable_labels(mpg="MPG", cyl=NULL, disp="DISP",
                                  hp="HP", drat="DRAT", wt="WT", qsec="QSEC",
                                  vs="VS", am="AM", gear="GEAR", carb="CARB") %>%
    swap_label_colnames() %>%
    labelled::lookfor() %>%
    dplyr::select(variable, label) %>%
    as.data.frame()
  }, regexp = "Missing labels for columns: `cyl`")
  testthat::expect_equal(dplyr::slice(tmp, 1), data.frame(variable="MPG", label="mpg"))
  testthat::expect_equal(dplyr::slice(tmp, 2), data.frame(variable="cyl", label="cyl"))
})
