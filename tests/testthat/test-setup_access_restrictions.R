testthat::test_that("refer_main_password_file", {

  file <- tempfile(fileext = ".htpasswd_private")
  saros:::write_htpasswd_file(x=data.frame(username="test", password="test"), file = file)
  if(interactive()) {
    saros:::refer_main_password_file(x=file, usernames="test2", append_users = TRUE, password_input = "prompt") |>
    testthat::expect_equal(expected = NULL)


  }
  saros:::refer_main_password_file(x=file, usernames="test3", append_users = TRUE, password_input = "12") |>
    testthat::expect_type("character")

})
