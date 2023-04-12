
#' Mostly an internal function to check whether options provided are valid
#'
#' Not much else to say.
#'
#' @param df Typically a design frame or a var_frame.
#' @param df_var Character string for variable name.
#' @param global_default If missing, what to set as default value.
#' @param options A type (character()) or a character vector with valid options.
#'
#' @importFrom rlang inform abort
#' @return Data frame, optionally with the missing variable added, set with the
#'   global_default if it is among the options.
#'
check_options <- function(df, df_var, global_default, options) {
  if(is.null(df[[df_var]])) {
    df[[df_var]] <- global_default
    rlang::inform(paste0('Variable `', df_var, '` does not exist in design frame. Adding and using global default: `', global_default, "`"))
  } else {
    if(!is.numeric(options) && !is.numeric(df[[df_var]])) {
      invalid <- unique(df[[df_var]][!df[[df_var]] %in% options])
      if(length(invalid) > 0L) {
        rlang::abort(c("Incorrect options:",
                       x=paste0('Variable `', df_var, '` in design frame contains invalid options'),
                       rlang::quo_text(invalid),
                       i='Please use one of:',
                       rlang::quo_text(options)))
      }
    } else if(class(options) != class(df[[df_var]])) {
      rlang::abort(c(x=paste0('Variable `', df_var, '` in design frame is of class ', class(df[[df_var]])),
                     i=paste0('Options expect ', class(options))))
    } else if(any(is.na(df[[df_var]]))) {
      df[is.na(df[[df_var]]), df_var] <- global_default
      rlang::inform(paste0('Variable `', df_var, '` in design frame contains NA. Using global default: `', global_default, "`"))
    }
  }
  df
}
