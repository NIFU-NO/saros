insert_qmd_tablet_tailored_order <- function(element_name = NULL,
                                             qmd_snippet = "",
                                             qmd_snippet_tailored = NULL,
                                             tailored_var = NULL,
                                             tailored_group = NULL,
                                             panel_tabset_tailored = FALSE,
                                             tailored_first = TRUE,
                                             translations = .saros.env$defaults$translations) {

  qmd_out <- ""
  # if(!rlang::is_string(qmd_snippet) || stringi::stri_isempty(qmd_snippet)) return("")

  if(rlang::is_string(tailored_var) &&
     rlang::is_string(qmd_snippet_tailored) &&
     !stringi::stri_isempty(qmd_snippet_tailored)) {
    stringr::str_c(qmd_out,
                   if(panel_tabset_tailored) stringr::str_c("::: {.panel-tabset}",
                   "\n\n##### ",
                   if(tailored_first) tailored_group else translations$tailored_label_all_others,
                   "\n\n"),
                   if(tailored_first) qmd_snippet_tailored else qmd_snippet,
                   if(panel_tabset_tailored) stringr::str_c("\n\n##### ",
                   if(tailored_first) translations$tailored_label_all_others else tailored_group,
                   "\n\n"),
                   if(tailored_first) qmd_snippet else qmd_snippet_tailored,
                   if(panel_tabset_tailored) "\n:::\n\n")
  } else stringr::str_c(qmd_out, "\n\n", qmd_snippet)

}
