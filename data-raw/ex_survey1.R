
set.seed(4321)
data_nrow <- 300
ex_survey <-
	tibble::tibble(
		x1_sex = factor(sample(x=1:2, replace = TRUE, size = data_nrow), labels = c("Males", "Females")),
		x2_human = factor(sample(x=0:1, replace = TRUE, size = data_nrow), labels = c("Robot?", "Definitely humanoid")),
		x3_nationality = factor(sample(x=c("Andorra", "Belize", "Cameroon", "Denmark", "Eswatini", "Fiji", "Ghana"), replace = TRUE, size = data_nrow, prob=c(.1, .15, .2, .25, .1, .1, .1))))
ex_survey[,c(paste0("a_", 1:9), paste0("b_", 1:3), paste0("c_", 1:2), paste0("d_", 1:4), paste0("e_", 1:4), paste0("p_", 1:4), "f_uni")] <- NA
ex_survey <-
	dplyr::mutate(ex_survey,
		dplyr::across(dplyr::all_of(paste0("a_", 1:9)),
					  ~factor(sample(x=c(0, 1, NA_real_), replace = TRUE, size = data_nrow, prob=c(.45,.45,.1)),
					  					labels = c("No", "Yes"))),
		dplyr::across(dplyr::all_of(paste0("b_", 1:3)),
					  ~factor(sample(x=0:2, replace = TRUE, size = data_nrow, prob=c(.45,.45,.1)),
					  					labels = c("Not at all", "A bit", "A lot"))),
		dplyr::across(dplyr::all_of(paste0("c_", 1:2)),
					  ~round(rnorm(n = data_nrow, mean = 20, sd = 5), 1)),
		dplyr::across(dplyr::all_of(paste0("d_", 1:4)),
					  ~factor(sample(x=0:10, replace = TRUE, size = data_nrow),
					  					labels = c("0\nCannot do at all", 1:4, "5\nModerately can do", 6:9, "10\nHighly certain can do"))),
		dplyr::across(dplyr::all_of(paste0("e_", 1:4)),
					  ~factor(sample(x=0:4, replace = TRUE, size = data_nrow),
					  					labels = c("Very rarely", "Rarely", "Sometimes", "Often", "Very often"))),
		dplyr::across(dplyr::all_of(paste0("p_", 1:3)),
					  ~factor(sample(x=0:3, replace = TRUE, size = data_nrow, prob = c(.2,.2,.2,.4)),
					  					labels = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"))),
		p_4 = factor(sample(x=c(1:4, NA), replace = TRUE, size = data_nrow, prob = c(.2,.2,.2,.3, .1)),
		             labels = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")),
		f_uni=sample(x=paste0("Uni of ", LETTERS[1:5]), replace = TRUE, size = data_nrow),
		open_comments = sample(x = c(rep(NA_character_, data_nrow-10), paste0("This is an open comment with random </br> symbols #", 1:10)),
		                       replace = FALSE, size = data_nrow),
		resp_status = sample(x=1:6, replace=TRUE, size=data_nrow, prob = c(.01, .1, .15, .7, .03, .01)))

labelled::var_label(ex_survey) <- c(
	"Gender",
	"Is respondent human?",
	"In which country were you born?",
	paste0("Do you consent to the following? - Agreement #", 1:9),
	paste0("How much do you like living in - ", c("Bejing", "Brussels", "Budapest")),
	paste0("How many years of experience do you have in - ", c("Company A", "Company B")),
	paste0("Rate your degree of confidence doing the following - ", c("Driving", "Drinking", "Diving", "Dancing")),
	paste0("How often do you do the following? - ", c("Eat", "Eavesdrop", "Exercise", "Encourage someone whom you have only recently met and who struggles with simple tasks that they cannot achieve by themselves")),
	paste0("To what extent do you agree or disagree to the following policies - ", c("Red", "Green", "Yellow", "Blue"), " Party"),
	paste0("Which of the following universities would you prefer to study at?"),
	"Do you have any comments to the survey?",
	"Response status (1=Not sent out, 2=Unopened, 3=Some responses, 4=Completed, 5=Excused, 6=Not serious)")

saveRDS(object = ex_survey, file = "inst/extdata/ex_survey.RDS", compress = FALSE)
usethis::use_data(ex_survey, overwrite = TRUE)



