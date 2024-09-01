
check_options <- function(call,
                          defaults_env = global_settings_get(),
                          default_values = formals(makeme),
                          ignore_args = .saros.env$ignore_args) {

  call <- call[!names(call) %in% ignore_args]
  call <- lapply(call, eval, envir = rlang::caller_env(n = 1))
  default_values <- default_values[!names(default_values) %in% ignore_args]
  default_values <- lapply(default_values, eval)

  # Loop over each argument in the function
  arg_names <- names(default_values)
  final_args <- list()

  for (arg_name in arg_names) {
    final_args[[arg_name]] <- get_argument_value(arg_name = arg_name,
                                                 call = call,
                                                 defaults_env = defaults_env,
                                                 formals_list = default_values)
  }
  final_args
}
