testthat::test_that("get_colour_set", {
  testthat::expect_warning(
      get_colour_set(x = 1:4,
                     user_colour_set = c("#444444", "#dddddd", "#123414"),
                     colour_na = "#ffffff"),
    regexp = "Fewer colours in user-provided colour palette than needed.")
  # testthat::expect_equal(x, c("#CAB2D6", "#33A02C", "#FDBF6F", "#A6CEE3"))

  testthat::expect_no_message(
    x <-
      get_colour_set(x = 1:3,
                     user_colour_set = c("#444444", "#dddddd", "#123414")))
  testthat::expect_equal(x, c("1" = "#444444", "2" = "#dddddd", "3" = "#123414"))


  testthat::expect_warning(
    x <-
      get_colour_set(x = 1:14,
                     user_colour_set = c("#444444", "#dddddd", "#123414")),
    regexp = "Fewer colours in user-provided colour palette than needed.")
  testthat::expect_equal(x,
                         c(`1` = "#F8766D", `2` = "#E38900", `3` = "#C49A00", `4` = "#99A800",
                           `5` = "#53B400", `6` = "#00BC56", `7` = "#00C094", `8` = "#00BFC4",
                           `9` = "#00B6EB", `10` = "#06A4FF", `11` = "#A58AFF", `12` = "#DF70F8",
                           `13` = "#FB61D7", `14` = "#FF66A8"))

  x <-
    get_colour_set(x = 1:2,
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"))
  testthat::expect_equal(x, c("1" = "#440154FF", "2" = "#40BC72FF"))

  x <-
    get_colour_set(x = 1:2,
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"))
  testthat::expect_equal(x, c("1" = "#440154FF", "2" = "#40BC72FF"))


  x <-
    get_colour_set(x = c(1:2, NA),
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"),
                   colour_na = "gray")
  testthat::expect_equal(x, c("1" = "#440154FF", "2" = "#40BC72FF", "NA" = "gray"))

  x <-
    get_colour_set(x = c(1:2, NA),
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"),
                   colour_na = "gray",
                   colour_2nd_binary_cat = "white")
  testthat::expect_equal(x, c("1" = "#440154FF", "2" = "white", "NA" = "gray"))

})
