testthat::test_that("draft_report", {

  tmpdir <- file.path(tempdir(), "test-draft_report")
  saros::draft_report(
      chapter_overview = saros::ex_survey_ch_overview[1:3, ],
      data = saros::ex_survey,
      path = tmpdir)

  output_files <-
    list.files(pattern = "\\.qmd$", path = tmpdir,
               full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
  output_files <-
    gsub(x=output_files, pattern = "\\", replacement = "/", fixed=TRUE)
  testthat::expect_equal(
      object = length(output_files),
      expected = 5)
  if(!is.null(quarto::quarto_path()) && nchar(quarto::quarto_path())>1) {
    testthat::expect_lt(file.size(output_files[1]), 3600)
    testthat::expect_gt(file.size(output_files[4]), 3600)
    testthat::expect_no_error(
      withr::with_dir(new = tmpdir,
                      code = quarto::quarto_render(input = output_files[3])))
  }


  ##############################

  if(Sys.getenv("USERNAME") == "py128") { # Run expensive >10 min test only for maintainer
  tmpdir <- file.path(tempdir(), "test-draft_report2")
  data <- saros::ex_survey |>
    dplyr::filter(f_uni %in% c("Uni of A", "Uni of B"))
  saros::draft_report(
     chapter_overview = saros::ex_survey_ch_overview[1:3, ],
     data = data,
     mesos_report = TRUE,
     mesos_var = "f_uni",
     path = tmpdir)
  output_files <-
    list.files(pattern = "\\.qmd", path = tmpdir,
               full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  testthat::expect_equal(
    object = length(output_files),
    expected = (nrow(saros::ex_survey_ch_overview[1:3, ])+2) * dplyr::n_distinct(data$f_uni))

  }
})
