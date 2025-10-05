#' Read tabular data from various formats
#'
#' A wrapper function to read data from different file formats
#'
#' @param path Character string specifying the file path
#' @param format Character string specifying the format: "delim", "xlsx", "csv", "csv2", "tsv", "sav", "dta"
#' @param ... Additional arguments passed to the underlying read functions
#'
#' @return A data frame containing the loaded data
#' @export
#'
tabular_read <- function(path, format, ...) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }

  switch(
    format,
    delim = utils::read.table(
      file = path,
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE,
      ...
    ),
    xlsx = readxl::read_excel(path = path, ...),
    csv = readr::read_csv(file = path, ...),
    csv2 = readr::read_csv2(file = path, ...),
    tsv = readr::read_tsv(file = path, ...),
    sav = haven::read_sav(file = path, ...),
    dta = haven::read_dta(file = path, ...),
    stop(
      "Unsupported format: ",
      format,
      ". Supported formats are: delim, xlsx, csv, csv2, tsv, sav, dta"
    )
  )
}
