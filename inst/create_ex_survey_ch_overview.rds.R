ex_survey_ch_overview <-
  data.frame(
   chapter = c("2 Ambivalence",
               "3 Big mysteries",
               "4 Confidence"),
   author = c("Agatha Christie", "Mark Twain", "Stephen King"),
   dep_int = c("matches('^c_')", "p_1:p_4", "starts_with('d_')"),
   dep_cat = c("a_1:a_6, a_9", "b_1:b_3", "matches('^e_')"),
   indep_cat = c("x1_sex", "x2_human", "f_uni"),
   irrelevant_col = "abc")
saveRDS(object = ex_survey_ch_overview, file = "inst/extdata/ex_survey_ch_overview.RDS", compress = FALSE)
usethis::use_data(ex_survey_ch_overview, overwrite = TRUE)
