
<!-- README.md is generated from README.Rmd. Please edit that file -->

# saros: Semi-Automatic Reporting of Ordinary Surveys

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/NIFU-NO/saros/branch/main/graph/badge.svg)](https://app.codecov.io/gh/NIFU-NO/saros?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/saros)](https://CRAN.R-project.org/package=SAROS)
[![CRAN
release](https://www.r-pkg.org/badges/version-ago/saros)](https://CRAN.R-project.org/package=SAROS)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/saros)](https://CRAN.R-project.org/package=SAROS)
[![R-CMD-check](https://github.com/NIFU-NO/saros/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NIFU-NO/saros/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Introduction

saros (Semi-Automatic Reporting of Ordinary Surveys) is an R package
designed to handle repeating surveys within the same project that occur
annually or biannually. It aims to automate the process of summarizing
and reporting on survey data, helping researchers save time and maintain
consistency across survey iterations. Specifically, SAROS produces
highly customizable figures, text, analyses and reports for a batch of
possible dependent-independent relations of possible interest.

### Connection to the Astronomical concept of Saros

The term “saros” also refers to a cyclical phenomenon in astronomy known
as the Saros cycle, which represents a period of approximately 18 years,
11 days, and 8 hours. After one Saros, the Sun, Earth, and Moon return
to approximately the same relative positions, leading to a similar
eclipse.

- Cyclical occurrences: Both versions of SAROS refer to cyclical events
  that recur at regular intervals. In the case of the R package, it
  relates to surveys that are repeated annually or biannually, while in
  astronomy, it refers to the recurring pattern of eclipses.

- Consistency: The SAROS R package aims to maintain consistency in
  reporting and analysis across survey iterations, just as the
  astronomical Saros cycle represents a predictable pattern in the
  occurrence of solar and lunar eclipses.

- Time-saving: The SAROS R package is designed to automate and
  streamline the process of summarizing and reporting on survey data,
  saving researchers time and effort. Similarly, the Saros cycle is a
  useful tool for astronomers to predict and plan for future eclipses,
  making it easier to study these events without having to make complex
  calculations each time.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("sda030/saros")
```

## Initialize a social science project

``` r
### CURRENTLY NOT WORKING. MOVE TO ANOTHER PACKAGE?
#> saros::initialize(folder = getwd())
```

``` r
# Define temporary folder for storing the elements
library(saros)
output_index_qmd <-
  draft_report(chapter_overview = ex_survey_ch_overview, 
             data = ex_survey, 
             mesos_var = "f_uni",
             path = tempdir()
             )
#> Refining chapter_overview...                             Generating report ...Generating chapter 2 Ambivalence                                 Generating report ...Generating chapter 3 Big mysteries                                   Generating report ...Generating chapter 4 Confidence                                Generating report ...Generating flexi-app Generating flexi-app
#> Generating report ...
#> 39.8 2.1 59.17 NA NA
#> 
quarto::quarto_render(output_index_qmd, as_job = FALSE)
#> 
#> 
#> processing file: index.qmd
#> 1/129                                                                                                          
#> 2/129 [tbl-How_many_years_of_experience_do_you_have_in_uni_sigtest_Others_221]                                 
#> 3/129                                                                                                          
#> 4/129 [tbl-c_1_c_2_Others_BY_ALL_INDEP_202]                                                                    
#> 5/129                                                                                                          
#> 6/129 [fig-html_Do_you_consent_to_the_following__uni_cat_prop_plot_Others_292]                                 
#> 7/129                                                                                                          
#> 8/129 [fig-pdf_Do_you_consent_to_the_following__uni_cat_prop_plot_Others_662]                                  
#> 9/129                                                                                                          
#> 10/129 [fig-html_Do_you_consent_to_the_following__uni_cat_freq_plot_Others_693]                                 
#> 11/129                                                                                                          
#> 12/129 [fig-pdf_Do_you_consent_to_the_following__uni_cat_freq_plot_Others_940]                                  
#> 13/129                                                                                                          
#> 14/129 [tbl-Do_you_consent_to_the_following__uni_cat_table_Others_192]                                          
#> 15/129                                                                                                          
#> 16/129 [tbl-Do_you_consent_to_the_following__uni_sigtest_Others_228]                                            
#> 17/129                                                                                                          
#> 18/129 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_699]                                              
#> 19/129                                                                                                          
#> 20/129 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_856]                                               
#> 21/129                                                                                                          
#> 22/129 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_166]                                              
#> 23/129                                                                                                          
#> 24/129 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_142]                                               
#> 25/129                                                                                                          
#> 26/129 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_038]                                              
#> 27/129                                                                                                          
#> 28/129 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_478]                                               
#> 29/129                                                                                                          
#> 30/129 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_316]                                              
#> 31/129                                                                                                          
#> 32/129 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_708]                                               
#> 33/129                                                                                                          
#> 34/129 [tbl-a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_926]                                                   
#> 35/129                                                                                                          
#> 36/129 [tbl-a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_ALL_INDEP_112]                                                
#> 37/129                                                                                                          
#> 38/129 [fig-html_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_uni_cat_prop_plot_Others_206]
#> 39/129                                                                                                          
#> 40/129 [fig-pdf_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_uni_cat_prop_plot_Others_022] 
#> 41/129                                                                                                          
#> 42/129 [fig-html_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_uni_cat_freq_plot_Others_268]
#> 43/129                                                                                                          
#> 44/129 [fig-pdf_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_uni_cat_freq_plot_Others_156] 
#> 45/129                                                                                                          
#> 46/129 [tbl-To_what_extent_do_you_agree_or_disagree_to_the_following_policies_uni_cat_table_Others_573]         
#> 47/129                                                                                                          
#> 48/129 [tbl-To_what_extent_do_you_agree_or_disagree_to_the_following_policies_uni_sigtest_Others_122]           
#> 49/129                                                                                                          
#> 50/129 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_845]                                                        
#> 51/129                                                                                                          
#> 52/129 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_612]                                                         
#> 53/129                                                                                                          
#> 54/129 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_329]                                                        
#> 55/129                                                                                                          
#> 56/129 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_844]                                                         
#> 57/129                                                                                                          
#> 58/129 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_078]                                                        
#> 59/129                                                                                                          
#> 60/129 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_641]                                                         
#> 61/129                                                                                                          
#> 62/129 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_526]                                                        
#> 63/129                                                                                                          
#> 64/129 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_219]                                                         
#> 65/129                                                                                                          
#> 66/129 [tbl-p_1_p_2_p_3_p_4_Others_BY_x2_human_456]                                                             
#> 67/129                                                                                                          
#> 68/129 [tbl-p_1_p_2_p_3_p_4_Others_BY_ALL_INDEP_313]                                                            
#> 69/129                                                                                                          
#> 70/129 [fig-html_How_much_do_you_like_living_in_uni_cat_prop_plot_Others_752]                                   
#> 71/129                                                                                                          
#> 72/129 [fig-pdf_How_much_do_you_like_living_in_uni_cat_prop_plot_Others_006]                                    
#> 73/129                                                                                                          
#> 74/129 [fig-html_How_much_do_you_like_living_in_uni_cat_freq_plot_Others_598]                                   
#> 75/129                                                                                                          
#> 76/129 [fig-pdf_How_much_do_you_like_living_in_uni_cat_freq_plot_Others_340]                                    
#> 77/129                                                                                                          
#> 78/129 [tbl-How_much_do_you_like_living_in_uni_cat_table_Others_139]                                            
#> 79/129                                                                                                          
#> 80/129 [tbl-How_much_do_you_like_living_in_uni_sigtest_Others_540]                                              
#> 81/129                                                                                                          
#> 82/129 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_164]                                                            
#> 83/129                                                                                                          
#> 84/129 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_312]                                                             
#> 85/129                                                                                                          
#> 86/129 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_710]                                                            
#> 87/129                                                                                                          
#> 88/129 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_953]                                                             
#> 89/129                                                                                                          
#> 90/129 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_102]                                                            
#> 91/129                                                                                                          
#> 92/129 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_111]                                                             
#> 93/129                                                                                                          
#> 94/129 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_840]                                                            
#> 95/129                                                                                                          
#> 96/129 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_476]                                                             
#> 97/129                                                                                                          
#> 98/129 [tbl-b_1_b_2_b_3_Others_BY_x2_human_001]                                                                 
#> 99/129                                                                                                          
#> 100/129 [tbl-b_1_b_2_b_3_Others_BY_ALL_INDEP_974]                                                                
#> 101/129                                                                                                          
#> 102/129 [fig-html_Rate_your_degree_of_confidence_doing_the_following_uni_cat_prop_plot_Others_849]               
#> 103/129                                                                                                          
#> 104/129 [fig-pdf_Rate_your_degree_of_confidence_doing_the_following_uni_cat_prop_plot_Others_089]                
#> 105/129                                                                                                          
#> 106/129 [fig-html_Rate_your_degree_of_confidence_doing_the_following_uni_cat_freq_plot_Others_586]               
#> 107/129                                                                                                          
#> 108/129 [fig-pdf_Rate_your_degree_of_confidence_doing_the_following_uni_cat_freq_plot_Others_082]                
#> 109/129                                                                                                          
#> 110/129 [tbl-Rate_your_degree_of_confidence_doing_the_following_uni_cat_table_Others_652]                        
#> 111/129                                                                                                          
#> 112/129 [tbl-Rate_your_degree_of_confidence_doing_the_following_uni_sigtest_Others_951]                          
#> 113/129                                                                                                          
#> 114/129 [tbl-d_1_d_2_d_3_d_4_Others_BY_ALL_INDEP_670]                                                            
#> 115/129                                                                                                          
#> 116/129 [fig-html_How_often_do_you_do_the_following__uni_cat_prop_plot_Others_451]                               
#> 117/129                                                                                                          
#> 118/129 [fig-pdf_How_often_do_you_do_the_following__uni_cat_prop_plot_Others_431]                                
#> 119/129                                                                                                          
#> 120/129 [fig-html_How_often_do_you_do_the_following__uni_cat_freq_plot_Others_736]                               
#> 121/129                                                                                                          
#> 122/129 [fig-pdf_How_often_do_you_do_the_following__uni_cat_freq_plot_Others_263]                                
#> 123/129                                                                                                          
#> 124/129 [tbl-How_often_do_you_do_the_following__uni_cat_table_Others_582]                                        
#> 125/129                                                                                                          
#> 126/129 [tbl-How_often_do_you_do_the_following__uni_sigtest_Others_730]                                          
#> 127/129                                                                                                          
#> 128/129 [tbl-e_1_e_2_e_3_e_4_Others_BY_ALL_INDEP_139]                                                            
#> 129/129                                                                                                          
#> output file: index.knit.md
#> 
#> pandoc 
#>   to: html
#>   output-file: index.html
#>   standalone: true
#>   section-divs: true
#>   html-math-method: mathjax
#>   wrap: none
#>   default-image-extension: png
#>   
#> metadata
#>   document-css: false
#>   link-citations: true
#>   date-format: long
#>   lang: en
#>   title: Report
#>   authors:
#>     - Agatha Christie
#>     - Mark Twain
#>     - Stephen King
#>   
#> [WARNING] This document format requires a nonempty <title> element.
#>   Defaulting to 'quarto-input0c68d3c9' as the title.
#>   To specify a title, use 'title' in metadata or --metadata title="...".
#> Output created: index.html
if(interactive()) {
  browseURL(output_index_qmd)
  browseURL(fs::path(tempdir(), "index.html"))
}
```

``` r
# Define temporary folder for storing the elements
library(saros)
output_index_qmd <-
  draft_report(chapter_overview = 
                 dplyr::mutate(ex_survey_ch_overview, 
                               indep = ifelse(.data$indep == "x1_sex",
                                              "x1_sex,x2_human", 
                                              .data$indep)),
              data = ex_survey[ex_survey$f_uni %in% c("Uni of A", "Uni of B"), ], 
               title = "My first report - ",
               mesos_var = "f_uni",
               label_separator = " - ",
               name_separator = "_",
              groupby = c("chapter", ".element_name", ".variable_label_prefix"),
               index_filename = "main_index.qmd",
               data_label = "percentage_bare",
               always_show_bi_for_by = "x1_sex",
               totals = TRUE,
               hide_label_if_prop_below = .05, 
               hide_test_if_n_below = 10,
               mesos_first = TRUE,
               colour_palette = nifutheme::nifu_cols(),
               digits = 0,
               data_label_decimal_symbol = ",",
             path = tempdir())
#> Refining chapter_overview... Generating report ...Generating chapter 2
#> Ambivalence Generating report ...Generating chapter 3 Big mysteries Generating
#> report ...Generating chapter 4 Confidence
#> Warning: Fewer colours in user-provided colour palette than needed.
#> Fewer colours in user-provided colour palette than needed.
#> Fewer colours in user-provided colour palette than needed.
#> Fewer colours in user-provided colour palette than needed.
#>                                 Generating report ...Generating flexi-app Generating flexi-app
#> Generating report ...
#> 44.01 2.31 58.63 NA NA
#> 
system.time(
  for(file in output_index_qmd) {
    quarto::quarto_render(file, as_job = TRUE)
  }
  )
#> 
#> 
#> processing file: main_index.qmd
#> 1/147                                                                                                          
#> 2/147 [fig-html_uni_cat_prop_plot_Do_you_consent_to_the_following__Others_926]                                 
#> 3/147                                                                                                          
#> 4/147 [fig-pdf_uni_cat_prop_plot_Do_you_consent_to_the_following__Others_330]                                  
#> 5/147                                                                                                          
#> 6/147 [fig-html_uni_cat_freq_plot_Do_you_consent_to_the_following__Others_131]                                 
#> 7/147                                                                                                          
#> 8/147 [fig-pdf_uni_cat_freq_plot_Do_you_consent_to_the_following__Others_597]                                  
#> 9/147                                                                                                          
#> 10/147 [tbl-uni_cat_table_Do_you_consent_to_the_following__Others_614]                                          
#> 11/147                                                                                                          
#> 12/147 [tbl-uni_sigtest_How_many_years_of_experience_do_you_have_in_Others_538]                                 
#> 13/147                                                                                                          
#> 14/147 [tbl-uni_sigtest_Do_you_consent_to_the_following__Others_543]                                            
#> 15/147                                                                                                          
#> 16/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_619]                                              
#> 17/147                                                                                                          
#> 18/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_156]                                               
#> 19/147                                                                                                          
#> 20/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_505]                                            
#> 21/147                                                                                                          
#> 22/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_047]                                             
#> 23/147                                                                                                          
#> 24/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_794]                                              
#> 25/147                                                                                                          
#> 26/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_711]                                               
#> 27/147                                                                                                          
#> 28/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_823]                                            
#> 29/147                                                                                                          
#> 30/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_126]                                             
#> 31/147                                                                                                          
#> 32/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_167]                                              
#> 33/147                                                                                                          
#> 34/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_047]                                               
#> 35/147                                                                                                          
#> 36/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_611]                                            
#> 37/147                                                                                                          
#> 38/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_879]                                             
#> 39/147                                                                                                          
#> 40/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_480]                                              
#> 41/147                                                                                                          
#> 42/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_849]                                               
#> 43/147                                                                                                          
#> 44/147 [fig-html_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_382]                                            
#> 45/147                                                                                                          
#> 46/147 [fig-pdf_a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_474]                                             
#> 47/147                                                                                                          
#> 48/147 [tbl-a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x1_sex_231]                                                   
#> 49/147                                                                                                          
#> 50/147 [tbl-a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_x2_human_144]                                                 
#> 51/147                                                                                                          
#> 52/147 [tbl-c_1_c_2_Others_BY_ALL_INDEP_136]                                                                    
#> 53/147                                                                                                          
#> 54/147 [tbl-a_1_a_2_a_3_a_4_a_5_a_6_a_9_Others_BY_ALL_INDEP_652]                                                
#> 55/147                                                                                                          
#> 56/147 [fig-html_uni_cat_prop_plot_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_Others_123]
#> 57/147                                                                                                          
#> 58/147 [fig-pdf_uni_cat_prop_plot_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_Others_932] 
#> 59/147                                                                                                          
#> 60/147 [fig-html_uni_cat_prop_plot_How_much_do_you_like_living_in_Others_641]                                   
#> 61/147                                                                                                          
#> 62/147 [fig-pdf_uni_cat_prop_plot_How_much_do_you_like_living_in_Others_722]                                    
#> 63/147                                                                                                          
#> 64/147 [fig-html_uni_cat_freq_plot_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_Others_991]
#> 65/147                                                                                                          
#> 66/147 [fig-pdf_uni_cat_freq_plot_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_Others_921] 
#> 67/147                                                                                                          
#> 68/147 [fig-html_uni_cat_freq_plot_How_much_do_you_like_living_in_Others_569]                                   
#> 69/147                                                                                                          
#> 70/147 [fig-pdf_uni_cat_freq_plot_How_much_do_you_like_living_in_Others_827]                                    
#> 71/147                                                                                                          
#> 72/147 [tbl-uni_cat_table_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_Others_896]         
#> 73/147                                                                                                          
#> 74/147 [tbl-uni_cat_table_How_much_do_you_like_living_in_Others_136]                                            
#> 75/147                                                                                                          
#> 76/147 [tbl-uni_sigtest_To_what_extent_do_you_agree_or_disagree_to_the_following_policies_Others_070]           
#> 77/147                                                                                                          
#> 78/147 [tbl-uni_sigtest_How_much_do_you_like_living_in_Others_197]                                              
#> 79/147                                                                                                          
#> 80/147 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_539]                                                        
#> 81/147                                                                                                          
#> 82/147 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_860]                                                         
#> 83/147                                                                                                          
#> 84/147 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_296]                                                            
#> 85/147                                                                                                          
#> 86/147 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_258]                                                             
#> 87/147                                                                                                          
#> 88/147 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_349]                                                        
#> 89/147                                                                                                          
#> 90/147 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_842]                                                         
#> 91/147                                                                                                          
#> 92/147 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_391]                                                            
#> 93/147                                                                                                          
#> 94/147 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_180]                                                             
#> 95/147                                                                                                          
#> 96/147 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_741]                                                        
#> 97/147                                                                                                          
#> 98/147 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_131]                                                         
#> 99/147                                                                                                          
#> 100/147 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_374]                                                            
#> 101/147                                                                                                          
#> 102/147 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_994]                                                             
#> 103/147                                                                                                          
#> 104/147 [fig-html_p_1_p_2_p_3_p_4_Others_BY_x2_human_410]                                                        
#> 105/147                                                                                                          
#> 106/147 [fig-pdf_p_1_p_2_p_3_p_4_Others_BY_x2_human_919]                                                         
#> 107/147                                                                                                          
#> 108/147 [fig-html_b_1_b_2_b_3_Others_BY_x2_human_923]                                                            
#> 109/147                                                                                                          
#> 110/147 [fig-pdf_b_1_b_2_b_3_Others_BY_x2_human_824]                                                             
#> 111/147                                                                                                          
#> 112/147 [tbl-p_1_p_2_p_3_p_4_Others_BY_x2_human_364]                                                             
#> 113/147                                                                                                          
#> 114/147 [tbl-b_1_b_2_b_3_Others_BY_x2_human_468]                                                                 
#> 115/147                                                                                                          
#> 116/147 [tbl-p_1_p_2_p_3_p_4_Others_BY_ALL_INDEP_482]                                                            
#> 117/147                                                                                                          
#> 118/147 [tbl-b_1_b_2_b_3_Others_BY_ALL_INDEP_931]                                                                
#> 119/147                                                                                                          
#> 120/147 [fig-html_uni_cat_prop_plot_Rate_your_degree_of_confidence_doing_the_following_Others_335]               
#> 121/147                                                                                                          
#> 122/147 [fig-pdf_uni_cat_prop_plot_Rate_your_degree_of_confidence_doing_the_following_Others_519]                
#> 123/147                                                                                                          
#> 124/147 [fig-html_uni_cat_prop_plot_How_often_do_you_do_the_following__Others_407]                               
#> 125/147                                                                                                          
#> 126/147 [fig-pdf_uni_cat_prop_plot_How_often_do_you_do_the_following__Others_625]                                
#> 127/147                                                                                                          
#> 128/147 [fig-html_uni_cat_freq_plot_Rate_your_degree_of_confidence_doing_the_following_Others_648]               
#> 129/147                                                                                                          
#> 130/147 [fig-pdf_uni_cat_freq_plot_Rate_your_degree_of_confidence_doing_the_following_Others_837]                
#> 131/147                                                                                                          
#> 132/147 [fig-html_uni_cat_freq_plot_How_often_do_you_do_the_following__Others_393]                               
#> 133/147                                                                                                          
#> 134/147 [fig-pdf_uni_cat_freq_plot_How_often_do_you_do_the_following__Others_595]                                
#> 135/147                                                                                                          
#> 136/147 [tbl-uni_cat_table_Rate_your_degree_of_confidence_doing_the_following_Others_224]                        
#> 137/147                                                                                                          
#> 138/147 [tbl-uni_cat_table_How_often_do_you_do_the_following__Others_654]                                        
#> 139/147                                                                                                          
#> 140/147 [tbl-uni_sigtest_Rate_your_degree_of_confidence_doing_the_following_Others_023]                          
#> 141/147                                                                                                          
#> 142/147 [tbl-uni_sigtest_How_often_do_you_do_the_following__Others_724]                                          
#> 143/147                                                                                                          
#> 144/147 [tbl-d_1_d_2_d_3_d_4_Others_BY_ALL_INDEP_490]                                                            
#> 145/147                                                                                                          
#> 146/147 [tbl-e_1_e_2_e_3_e_4_Others_BY_ALL_INDEP_688]                                                            
#> 147/147                                                                                                          
#> output file: main_index.knit.md
#> 
#> pandoc 
#>   to: html
#>   output-file: main_index.html
#>   standalone: true
#>   section-divs: true
#>   html-math-method: mathjax
#>   wrap: none
#>   default-image-extension: png
#>   
#> metadata
#>   document-css: false
#>   link-citations: true
#>   date-format: long
#>   lang: en
#>   title: 'My first report - '
#>   authors:
#>     - Agatha Christie
#>     - Mark Twain
#>     - Stephen King
#>   
#> [WARNING] This document format requires a nonempty <title> element.
#>   Defaulting to 'quarto-input8024d7e4' as the title.
#>   To specify a title, use 'title' in metadata or --metadata title="...".
#> Output created: main_index.html
#>    user  system elapsed 
#>    0.00    0.02  519.32
if(interactive()) {
  browseURL(fs::path(tempdir(), "Uni of C", "Uni of C_main_index.html"))
}
```

## Documentation

Please see the [documentation](https://NIFU-NO.github.io/saros/).

## Licence: saros is not free to use.

In contrast to many R packages, this package is NOT free to use for any
Norwegian institution or person unless explicit written permission has
been given by the author, or NIFU. For anyone else in the world, it is
free to use for commercial and non-commercial purposes. However,
reproduction is not allowed.

## Code of Conduct

Please note that the saros project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
