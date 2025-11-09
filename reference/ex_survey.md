# ex_survey: Mockup dataset of a survey.

A dataset containing fake respondents' answers to survey questions. The
first two, x_sex and x_human, are intended to be independent variables,
whereas the remaining are dependent. The underscore \_ in variable names
separates item groups (prefix) from items (suffix) (i.e. a_1-a_9 =\> a +
1-9), whereas ' - ' separates the same for labels. The latter
corresponds with the default in SurveyXact.

## Usage

``` r
ex_survey
```

## Format

A data frame with 100 rows and 29 variables:

- x1_sex:

  Gender

- x2_human:

  Is respondent human?

- x3_nationality:

  Where is the respondent born?

- a_1:

  Do you consent to the following? - Agreement \#1

- a_2:

  Do you consent to the following? - Agreement \#2

- a_3:

  Do you consent to the following? - Agreement \#3

- a_4:

  Do you consent to the following? - Agreement \#4

- a_5:

  Do you consent to the following? - Agreement \#5

- a_6:

  Do you consent to the following? - Agreement \#6

- a_7:

  Do you consent to the following? - Agreement \#7

- a_8:

  Do you consent to the following? - Agreement \#8

- a_9:

  Do you consent to the following? - Agreement \#9

- b_1:

  How much do you like living in - Beijing

- b_2:

  How much do you like living in - Brussels

- b_3:

  How much do you like living in - Budapest

- c_1:

  How many years of experience do you have in - Company A

- c_2:

  How many years of experience do you have in - Company B

- d_1:

  Rate your degree of confidence doing the following - Driving

- d_2:

  Rate your degree of confidence doing the following - Drinking

- d_3:

  Rate your degree of confidence doing the following - Driving

- d_4:

  Rate your degree of confidence doing the following - Dancing

- e_1:

  How often do you do the following? - Eat

- e_2:

  How often do you do the following? - Eavesdrop

- e_3:

  How often do you do the following? - Exercise

- e_4:

  How often do you do the following? - Encourage someone whom you have
  only recently met and who struggles with simple tasks that they cannot
  achieve by themselves

- p_1:

  To what extent do you agree or disagree to the following policies -
  Red Party

- p_2:

  To what extent do you agree or disagree to the following policies -
  Green Party

- p_3:

  To what extent do you agree or disagree to the following policies -
  Yellow Party

- p_4:

  To what extent do you agree or disagree to the following policies -
  Blue Party

- f_uni:

  Which of the following universities would you prefer to study at?

- open_comments:

  Do you have any comments to the survey?

- resp_status:

  Response status
