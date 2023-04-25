testthat::test_that("get_colour_set", {
  testthat::expect_warning(
      get_colour_set(x = 1:4,
                     user_colour_set = c("#444444", "#dddddd", "#123414"),
                     colour_na = "#ffffff",
                     seed = 1),
    regexp = "Fewer colours in user-provided colour palette than needed.")
  # testthat::expect_equal(x, c("#CAB2D6", "#33A02C", "#FDBF6F", "#A6CEE3"))

  testthat::expect_no_message(
    x <-
      get_colour_set(x = 1:3,
                     user_colour_set = c("#444444", "#dddddd", "#123414"),
                     seed = 1))
  testthat::expect_equal(x, c("1" = "#444444", "2" = "#dddddd", "3" = "#123414"))


  testthat::expect_warning(
    x <-
      get_colour_set(x = 1:14,
                     user_colour_set = c("#444444", "#dddddd", "#123414"),
                     seed = 1),
    regexp = "Fewer colours in user-provided colour palette than needed.")
  testthat::expect_equal(x,
                         c(`1` = "#440154FF", `2` = "#481D6FFF", `3` = "#453581FF", `4` = "#3D4D8AFF",
                           `5` = "#34618DFF", `6` = "#2B748EFF", `7` = "#24878EFF", `8` = "#1F998AFF",
                           `9` = "#25AC82FF", `10` = "#40BC72FF", `11` = "#67CC5CFF", `12` = "#97D83FFF",
                           `13` = "#CBE11EFF", `14` = "#FDE725FF"))

  x <-
    get_colour_set(x = 1:2,
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"),
                   seed = 1)
  testthat::expect_equal(x, c("1" = "#440154FF", "2" = "#CBE11EFF"))

  x <-
    get_colour_set(x = 1:2,
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"),
                   colour_2nd_binary_cat = "#ffffff",
                   seed = 1)
  testthat::expect_equal(x, c("1" = "#40BC72FF", "2" = "#ffffff"))


  x <-
    get_colour_set(x = c(1:2, NA),
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"),
                   colour_na = "gray",
                   seed = 1)
  testthat::expect_equal(x, c("1" = "#440154FF", "2" = "#CBE11EFF", "NA" = "gray"))

  x <-
    get_colour_set(x = c(1:2, NA),
                   user_colour_set = c("#440154FF", "#40BC72FF", "#CBE11EFF"),
                   colour_na = "gray",
                   colour_2nd_binary_cat = "white",
                   seed = 1)
  testthat::expect_equal(x, c("1" = "#40BC72FF", "2" = "white", "NA" = "gray"))

})
