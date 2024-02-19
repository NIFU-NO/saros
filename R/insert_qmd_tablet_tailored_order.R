insert_qmd_tablet_mesos_order <-
  function(element_name = NULL,
           qmd_snippet = "",
           qmd_snippet_mesos = NULL,
           mesos_report = FALSE,
           mesos_var = NULL,
           mesos_group = NULL,
           panel_tabset_mesos = FALSE,
           mesos_first = TRUE,
           translations = eval(formals(draft_report)$translations)) {

  qmd_out <- ""
  # if(!is_string(qmd_snippet) || stringi::stri_isempty(qmd_snippet)) return("")

  qmd_snippet <- stringi::stri_remove_empty_na(qmd_snippet)
  qmd_snippet_mesos <- stringi::stri_remove_empty_na(qmd_snippet_mesos)

  if(isTRUE(mesos_report) &&
     is_string(mesos_var) &&
     is_string(qmd_snippet_mesos) &&
     !stringi::stri_isempty(qmd_snippet_mesos)) {
    stringi::stri_c(ignore_null=TRUE, qmd_out,
                   if(panel_tabset_mesos) stringi::stri_c(ignore_null=TRUE, "::: {.panel-tabset}",
                   "\n\n##### ",
                   if(mesos_first) mesos_group else translations$mesos_label_all_others,
                   "\n\n"),
                   if(mesos_first) qmd_snippet_mesos else qmd_snippet,
                   if(panel_tabset_mesos) stringi::stri_c(ignore_null=TRUE, "\n\n##### ",
                   if(mesos_first) translations$mesos_label_all_others else mesos_group,
                   "\n\n"),
                   if(mesos_first) qmd_snippet else qmd_snippet_mesos,
                   if(panel_tabset_mesos) "\n:::\n\n")
  } else stringi::stri_c(ignore_null=TRUE, qmd_out, "\n\n", qmd_snippet)

}
