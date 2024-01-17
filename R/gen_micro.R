gen_micro <- function(data, cols, path,
                      serialized_format,
                      tabular_format) {
  data <- data[, cols, drop = FALSE]
  dir.create(path, "Micro")
  tabular_write(object = data,
                       path = file.path(path, "Micro", paste0("micro.", serialized_format)),
                       format = tabular_format)
  serialize_write(data, path = file.path(path, "Micro", paste0("micro.", serialized_format)),
                  format = serialized_format)


  # Codebook


  # qmd-file
}
