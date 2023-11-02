gen_micro <- function(data, cols, path) {
  data <- data[, cols, drop = FALSE]
  dir.create(path, "Micro")
  utils::write.csv2(data, file=file.path(path, "Micro", "micro.csv"))
  writexl::write_xlsx(data, path=file.path(path, "Micro", "micro.xlsx"))
  qs::qsave(data, file = file.path(path, "Micro", "micro.Rds"))


  # Codebook


  # qmd-file
}
