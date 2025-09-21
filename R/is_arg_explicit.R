is_arg_explicit <- function(arg_name, call) {
  # Get the actual arguments passed to the function call
  actual_args <- as.list(call)

  # Check if the argument was explicitly passed
  if (arg_name %in% names(actual_args)) {
    TRUE
  } else {
    FALSE
  }
}

# Helper function to get argument value considering priorities
get_argument_value <- function(arg_name, call, defaults_env, formals_list) {
  if (is_arg_explicit(arg_name, call)) {
    return(call[[arg_name]])
  } else if (!is.null(defaults_env[[arg_name]])) {
    return(defaults_env[[arg_name]])
  } else {
    return(formals_list[[arg_name]])
  }
}
