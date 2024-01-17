gen_micro <- function(data, cols, path, serialized_format) {
  data <- data[, cols, drop = FALSE]
  dir.create(path, "Micro")
  utils::write.csv2(data, file=file.path(path, "Micro", "micro.csv"))
  writexl::write_xlsx(data, path=file.path(path, "Micro", "micro.xlsx"))
  serialize_write(data, path = file.path(path, "Micro", paste0("micro", serialized_format)),
                  format = serialized_format)


  # Codebook


  # qmd-file
}
