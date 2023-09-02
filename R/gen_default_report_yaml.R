# gen_default_report_yaml <-
#   function(yaml_path, data_colnames) {
#     yaml_out <-
#       tryCatch(expr = yaml::yaml.load_file(yaml_path),
#                warning = function(e) {
#                  cli::cli_progress_message(msg = "Creating YAML-template file...")
#                  cli::cli_inform("{.arg {yaml_path}} did not exist, created it with defaults.")
#
#                  yaml_defaults <-
#                    yamlthis::yaml_empty() %>%
#                    yamlthis::yaml_params( # Can this list be !!!spliced? Or simply params = getOption("saros")
#                      label_separator = .saros.env$defaults$label_separator,
#                      name_separator = .saros.env$defaults$name_separator,
#                      index_yaml_file = .saros.env$defaults$index_yaml_file,
#                      chapter_yaml_file = .saros.env$defaults$chapter_yaml_file,
#                      index_filename = .saros.env$defaults$index_filename,
#                      element_names = .saros.env$defaults$element_names,
#                      translations = .saros.env$defaults$translations
#                    ) %>%
#                    yaml::as.yaml()
#
#                  fs::dir_create(file.path_dir(yaml_path), recurse = TRUE)
#                  cat(yaml_defaults, file = yaml_path)
#                  yaml::read_yaml(file = as.character(yaml_path))
#                })
#
#     yaml_out$params
#
#
#   }
