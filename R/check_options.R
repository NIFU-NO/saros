check_options <- function(
  call,
  defaults_env = global_settings_get(),
  default_values = formals(makeme),
  ignore_args = .saros.env$ignore_args
) {
  # Get all parent environments up to global
  parent_envs <- rev(sys.frames())

  # Create sentinel value for "not found"
  not_found <- structure(list(), class = "not_found")

  call <- call[!names(call) %in% ignore_args]

  # Try to evaluate in each parent environment until success
  call <- lapply(call, function(arg) {
    if (is.language(arg)) {
      # Try each environment in the call stack
      for (env in parent_envs) {
        result <- tryCatch(
          {
            eval(arg, envir = env)
          },
          error = function(e) not_found
        )

        if (!inherits(result, "not_found")) {
          return(result)
        }
      }
      # If we get here, evaluation failed in all environments
      return(arg)
    }
    return(arg)
  })

  # Process default values
  default_values <- default_values[!names(default_values) %in% ignore_args]
  default_values <- lapply(default_values, eval)

  # Loop over each argument in the function
  arg_names <- names(default_values)
  final_args <- list()

  for (arg_name in arg_names) {
    # Try to evaluate any unevaluated arguments again in the parent environment
    if (is_arg_explicit(arg_name, call) && is.language(call[[arg_name]])) {
      # Try each environment again for any remaining unevaluated args
      for (env in parent_envs) {
        result <- tryCatch(
          {
            eval(call[[arg_name]], envir = env)
          },
          error = function(e) not_found
        )

        if (!inherits(result, "not_found")) {
          call[[arg_name]] <- result
          break
        }
      }
    }

    final_args[[arg_name]] <-
      get_argument_value(
        arg_name = arg_name,
        call = call,
        defaults_env = defaults_env,
        formals_list = default_values
      )
  }
  final_args
}
