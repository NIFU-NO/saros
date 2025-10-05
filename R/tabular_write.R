#' Write tabular data to various formats
#'
#' A wrapper function to write data frames to different file formats
#'
#' @param object A data frame to write
#' @param path Character string specifying the output file path
#' @param format Character string specifying the format: "delim", "xlsx", "csv", "csv2", "tsv", "sav", "dta"
#'
#' @return Invisibly returns TRUE on success, used for side effects
#' @export
#'
#' @examples
#' data <- data.frame(x = 1:3, y = letters[1:3])
#'
#' # Write as CSV
#' tabular_write(data, tempfile(fileext = ".csv"), format = "csv")
#'
#' # Write as Excel
#' tabular_write(data, tempfile(fileext = ".xlsx"), format = "xlsx")
#'
#' # Write as SPSS
#' tabular_write(data, tempfile(fileext = ".sav"), format = "sav")
tabular_write <- function(object, path, format) {
  switch(
    format,
    delim = utils::write.table(
      x = object,
      file = path,
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE
    ),
    xlsx = writexl::write_xlsx(x = object, path = path),
    csv = readr::write_csv(x = object, file = path),
    csv2 = readr::write_csv2(x = object, file = path),
    tsv = readr::write_tsv(x = object, file = path),
    sav = haven::write_sav(data = object, path = path),
    dta = haven::write_dta(data = object, path = path)
  )
}
