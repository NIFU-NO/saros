ex_survey_ch_overview <-
  data.frame(
   chapter = c("Introduction",
               "Ambivalence",
               "Big mysteries",
               "Confidence"),
   author = c("Ernst Hemmingway", "Agatha Christie", "Mark Twain", "Stephen King"),
   dep = c("", "matches('^c_'), a_1:a_6, a_9", "p_1:p_4, b_1:b_3", "starts_with('d_'), matches('^e_')"),
   indep = c("", "x1_sex", "x2_human", "f_uni"),
   irrelevant_col = "abc")
saveRDS(ex_survey_ch_overview, file = paste0("inst", .Platform$file.sep, "extdata", .Platform$file.sep, "ex_survey_ch_overview.RDS"), compress = FALSE)
usethis::use_data(ex_survey_ch_overview, overwrite = TRUE)
