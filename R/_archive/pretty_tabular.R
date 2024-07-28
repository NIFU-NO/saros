
tabular_write <- function(object, path, format) {
  switch(format,
         delim = utils::write.table(x = object, file = path, sep = "\t", row.names = FALSE, col.names = TRUE),
         xlsx = writexl::write_xlsx(x = object, path = path),
         csv = readr::write_csv(x = object, file = path),
         csv2 = readr::write_csv2(x = object, file = path),
         tsv = readr::write_tsv(x = object, file = path),
         sav = haven::write_sav(data = object, path = path),
         dta = haven::write_dta(data = object, path = path)
  )
}
