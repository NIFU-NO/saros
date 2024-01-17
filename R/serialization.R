serialize_read <- function(path, format) {
  switch(format,
         rds = readRDS(file = path),
         qs = qs::qread(file = path, strict = TRUE)
  )
}

serialize_read_syntax <- function(format) {
  switch(format,
         rds = "readRDS",
         qs = "qs::qread")
}

serialize_write <- function(object, path, format) {
  switch(format,
         rds = saveRDS(object = object, file = path),
         qs = qs::qsave(x = object, file = path)
  )
}
