attach_chapter_dataset2 <- function(chapter_overview_chapter,
                                   data,
                                   path,
                                   chapter_foldername_clean,
                                   mesos_var,
                                   auxiliary_variables) {

  data_chapter <- data[, names(data) %in% unique(c(chapter_overview_chapter$.variable_name_dep,
                                                   chapter_overview_chapter$.variable_name_indep,
                                                   mesos_var,
                                                   auxiliary_variables)), drop = FALSE]

  filename_chapter_dataset <-
    stringi::stri_c("data_", chapter_foldername_clean, ".qs", ignore_null = TRUE)
  filepath_chapter_dataset_absolute <- file.path(path, chapter_foldername_clean, filename_chapter_dataset)
  filepath_chapter_dataset_relative <- file.path(chapter_foldername_clean, filename_chapter_dataset)

  qs::qsave(data_chapter, file = filepath_chapter_dataset_absolute)

  r_chunk_header <- stringi::stri_c("```{r}\n",
                                    "#| label: 'Import data for ",
                                    chapter_foldername_clean,
                                    "'",
                                    sep="", ignore_null = TRUE)
  import_code <- stringi::stri_c("data_",
                                 chapter_foldername_clean,
                                 " <- qs::qread('", filepath_chapter_dataset_relative, "')",
                                 sep="", ignore_null = TRUE)
  stringi::stri_c(r_chunk_header, import_code, "```", sep="\n")
}
