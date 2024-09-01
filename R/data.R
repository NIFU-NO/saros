#' ex_survey: Mockup dataset of a survey.
#'
#' A dataset containing fake respondents' answers to survey questions.
#' The first two, x_sex and x_human, are intended to be independent variables,
#' whereas the remaining are dependent. The underscore _ in variable names
#' separates item groups (prefix) from items (suffix) (i.e. a_1-a_9 => a + 1-9),
#'  whereas ' - ' separates the same for labels.
#'  The latter corresponds with the default in SurveyXact.
#'
#' @format A data frame with 100 rows and 29 variables:
#' \describe{
#'   \item{x1_sex}{Gender}
#'   \item{x2_human}{Is respondent human?}
#'   \item{x3_nationality}{Where is the respondent born?}
#'   \item{a_1}{Do you consent to the following? - Agreement #1}
#'   \item{a_2}{Do you consent to the following? - Agreement #2}
#'   \item{a_3}{Do you consent to the following? - Agreement #3}
#'   \item{a_4}{Do you consent to the following? - Agreement #4}
#'   \item{a_5}{Do you consent to the following? - Agreement #5}
#'   \item{a_6}{Do you consent to the following? - Agreement #6}
#'   \item{a_7}{Do you consent to the following? - Agreement #7}
#'   \item{a_8}{Do you consent to the following? - Agreement #8}
#'   \item{a_9}{Do you consent to the following? - Agreement #9}
#'   \item{b_1}{How much do you like living in - Beijing}
#'   \item{b_2}{How much do you like living in - Brussels}
#'   \item{b_3}{How much do you like living in - Budapest}
#'   \item{c_1}{How many years of experience do you have in - Company A}
#'   \item{c_2}{How many years of experience do you have in - Company B}
#'   \item{d_1}{Rate your degree of confidence doing the following - Driving}
#'   \item{d_2}{Rate your degree of confidence doing the following - Drinking}
#'   \item{d_3}{Rate your degree of confidence doing the following - Driving}
#'   \item{d_4}{Rate your degree of confidence doing the following - Dancing}
#'   \item{e_1}{How often do you do the following? - Eat}
#'   \item{e_2}{How often do you do the following? - Eavesdrop}
#'   \item{e_3}{How often do you do the following? - Exercise}
#'   \item{e_4}{How often do you do the following? - Encourage someone whom you have only recently met and who struggles with simple tasks that they cannot achieve by themselves}
#'   \item{p_1}{To what extent do you agree or disagree to the
#'   following policies - Red Party}
#'   \item{p_2}{To what extent do you agree or disagree to the
#'   following policies - Green Party}
#'   \item{p_3}{To what extent do you agree or disagree to the
#'   following policies - Yellow Party}
#'   \item{p_4}{To what extent do you agree or disagree to the
#'   following policies - Blue Party}
#'   \item{f_uni}{Which of the following universities would you
#'   prefer to study at?}
#'   \item{open_comments}{Do you have any comments to the survey?}
#'   \item{resp_status}{Response status}
#' }
"ex_survey"
