ex_survey1_inf <-
	create_infoframe(df=ex_survey1,
				 y_var=grep("^[abcdefgp]_", names(ex_survey1), value=TRUE),
				 x_var=grep("^x[12]_", names(ex_survey1), value=TRUE),
				 ordinal_var=grep("^[abdegp]_", names(ex_survey1), value=TRUE),
				 nominal_var=c("x1_sex", "x2_human", "f_uni", "resp_status"),
				 add_constructs = TRUE,
				 add_x_univariates=TRUE,
				 add_y_univariates=TRUE)
usethis::use_data(ex_survey1_inf, overwrite = TRUE)
