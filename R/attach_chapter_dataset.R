attach_chapter_dataset <- function(chapter_overview_chapter,
                                   data,
                                   path,
                                   chapter_foldername_clean,
                                   mesos_var,
                                   auxiliary_variables) {

  chapter_columns <- dplyr::ungroup(chapter_overview_chapter)
  chapter_columns <- dplyr::distinct(chapter_columns,
                                     .data$.variable_name,
                                     .data$indep_cols_df,
                                     .keep_all = FALSE)
  indeps <- tidyr::unchop(chapter_columns, cols = "indep_cols_df")

  if(nrow(indeps)>0) {
    indeps <- unique(indeps[["indep_cols_df"]][[".variable_name"]])
  }
  data_chapter <- data[, names(data) %in% c(chapter_columns$.variable_name,
                                            indeps,
                                            mesos_var,
                                            auxiliary_variables), drop = FALSE]


  filename_chapter_dataset <-
    stringi::stri_c("data_", chapter_foldername_clean, ".RDs", ignore_null = TRUE)
  filepath_chapter_dataset_absolute <- file.path(path, chapter_foldername_clean, filename_chapter_dataset)
  filepath_chapter_dataset_relative <- file.path(chapter_foldername_clean, filename_chapter_dataset)

  qs::qsave(data_chapter, file = filepath_chapter_dataset_absolute)

  r_chunk_header <- stringi::stri_c("```{r}\n",
                                    "#| label: 'Import data for ",
                                    chapter_foldername_clean,
                                    "'",
                                    sep="", ignore_null = TRUE)
  import_code <- stringi::stri_c("`data_",
                                 chapter_foldername_clean,
                                 "` <- qs::qread('", filepath_chapter_dataset_relative, "')",
                                 sep="", ignore_null = TRUE)
  stringi::stri_c(r_chunk_header, import_code, "```", sep="\n")
}
