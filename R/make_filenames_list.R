make_filenames_list <- function(element_folderpath_relative,
                                element_folderpath_absolute,
                                filename_prefix,
                                serialized_format) {
  filepath <- list(rel = list(), abs = list())
  efr <- element_folderpath_relative
  efa <- element_folderpath_absolute
  filepath$rel[[serialized_format]] <- file.path(efr, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".", serialized_format))
  filepath$rel$png <- file.path(efr, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".png"))
  filepath$rel$xlsx <- file.path(efr, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))
  filepath$rel$txt <- file.path(efr, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".txt"))
  filepath$rel$docx <- file.path(efr, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".docx"))
  filepath$abs[[serialized_format]] <- file.path(efa, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".", serialized_format))
  filepath$abs$png <- file.path(efa, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".png"))
  filepath$abs$xlsx <- file.path(efa, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".xlsx"))
  filepath$abs$txt <- file.path(efa, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".txt"))
  filepath$abs$docx <- file.path(efa, stringi::stri_c(ignore_null=TRUE, filename_prefix, ".docx"))
  filepath
}
