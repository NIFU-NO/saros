testthat::test_that("infoframe from ex_data, w/o constructs or univariates", {
	ex_data <- readRDS(system.file("extdata", "ex_survey1.RDS",
	                   package="saros", mustWork=TRUE))
	obj_wo_con_uni <-
		create_infoframe(df=ex_survey1,
						 y_var=grep("^[abdefgp]_", names(ex_survey1), value=TRUE),
						 x_var=grep("^x", names(ex_survey1), value=TRUE),
						 ordinal_var=grep("^[abdegp]_", names(ex_survey1), value=TRUE),
						 nominal_var=c("x1_sex", "x2_human", "f_uni"),
						 add_constructs = FALSE,
						 add_x_univariates=FALSE,
						 add_y_univariates=FALSE)
	testthat::expect_identical(obj_wo_con_uni$df, ex_data)
	assert_valid_infoframe(obj_wo_con_uni)
})

testthat::test_that("infoframe from ex_data, w/o constructs", {
	ex_data <- readRDS(system.file("extdata", "ex_survey1.RDS",
								   package="saros", mustWork=TRUE))
	obj_wo_con <-
		create_infoframe(df=ex_survey1,
						 y_var=grep("^[abdefgp]_", names(ex_survey1), value=TRUE),
						 x_var=grep("^x", names(ex_survey1), value=TRUE),
						 ordinal_var=grep("^[abdegp]_", names(ex_survey1), value=TRUE),
						 nominal_var=c("x1_sex", "x2_human", "f_uni"),
						 add_constructs = FALSE,
						 add_x_univariates=TRUE,
						 add_y_univariates=TRUE)
	testthat::expect_identical(obj_wo_con$df, ex_data)
	assert_valid_infoframe(obj_wo_con)
})

testthat::test_that("infoframe from ex_data, w/o univariates", {
	ex_data <- readRDS(system.file("extdata", "ex_survey1.RDS",
								   package="saros", mustWork=TRUE))
	obj_wo_uni <-
		create_infoframe(df=ex_survey1,
						 y_var=grep("^[abdefgp]_", names(ex_survey1), value=TRUE),
						 x_var=grep("^x", names(ex_survey1), value=TRUE),
						 ordinal_var=grep("^[abdegp]_", names(ex_survey1), value=TRUE),
						 nominal_var=c("x1_sex", "x2_human", "f_uni"),
						 add_constructs = TRUE,
						 add_x_univariates=FALSE,
						 add_y_univariates=FALSE)
	testthat::expect_identical(obj_wo_uni$df, ex_data)
	assert_valid_infoframe(obj_wo_uni)
	testthat::expect_identical(obj_wo_uni$var_frame, unique(obj_wo_uni$var_frame))
	testthat::expect_identical(obj_wo_uni$design_frame, unique(obj_wo_uni$design_frame))
})

testthat::test_that("infoframe from ex_data", {
	ex_data <- readRDS(system.file("extdata", "ex_survey1.RDS",
								   package="saros", mustWork=TRUE))
	obj <-
		create_infoframe(df=ex_survey1,
						 y_var=grep("^[abdefgp]_", names(ex_survey1), value=TRUE),
						 x_var=grep("^x", names(ex_survey1), value=TRUE),
						 ordinal_var=grep("^[abdegp]_", names(ex_survey1), value=TRUE),
						 nominal_var=c("x1_sex", "x2_human", "f_uni"),
						 add_constructs = TRUE,
						 add_x_univariates=TRUE,
						 add_y_univariates=TRUE)
	testthat::expect_identical(obj$df, ex_data)
	assert_valid_infoframe(obj=obj)
	testthat::expect_identical(obj$design_frame, unique(obj$design_frame))
})
