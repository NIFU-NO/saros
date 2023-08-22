# tmp <-
#   list.files(path = here::here("data-raw"),
#              all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = TRUE) |>
#   stringi::stri_subset(pattern = "\\.R$|[^\\.]") |>
#   stringi::stri_subset(pattern =
#                         paste(sep="|",
#                               "/\\.git",
#                               "/\\.Rproj\\.user",
#                               "/renv",
#                               "_NIFU_",
#                               "site_libs",
#                               "2022[HV]",
#                               "archive",
#                               "_css",
#                               "_files/libs",
#                               "6 Logo and graphical materials/images/",
#                               "\\.quarto",
#                               "\\.docx$",
#                               "\\.js$",
#                               "\\.css$",
#                               "\\.pdf$",
#                               "\\.html$",
#                               "\\.ps1$",
#                               "\\.png$",
#                               "\\.pptx$"
#                         ), negate = TRUE)
#
# zip::zip(zipfile = here::here("inst", "template", "saros_files.zip"),
#          files = tmp,
#          recurse = TRUE, root = "data-raw",
#          compression_level = 9, include_directories = TRUE)
