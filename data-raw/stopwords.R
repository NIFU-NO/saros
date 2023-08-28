## code to prepare `stopwords` dataset goes here

stopwords <-
  jsonlite::read_json(path =
								 	system.file("extdata", "stopwords-iso.json", package = "saros", mustWork = TRUE),
								 simplifyVector = TRUE)
usethis::use_data(stopwords, overwrite = TRUE, internal = FALSE)
