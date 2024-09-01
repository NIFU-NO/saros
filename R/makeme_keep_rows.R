makeme_keep_rows <- function(data, crwd, mesos_var=NULL, mesos_group=NULL) {

  if(crwd == "all") return(TRUE)
  if(crwd == "target" &&
     rlang::is_string(mesos_var) &&
     rlang::is_string(mesos_group)) return(as.character(data[[mesos_var]]) == mesos_group)
  if(crwd == "others" &&
     rlang::is_string(mesos_var) &&
     rlang::is_string(mesos_group)) return(as.character(data[[mesos_var]]) != mesos_group)
  cli::cli_abort("Invalid argument combination {.arg {c('crwd', 'mesos_var', 'mesos_group')}}: {.val {c(crwd, mesos_var, mesos_group)}}.")
}
