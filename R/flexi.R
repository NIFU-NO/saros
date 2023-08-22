embed_flexi <- function(data,
                        chapter_overview,
                        ...) {

  dots <- update_dots(dots = rlang::list2(...),
                      caller_function = "flexi")

  # must handle dep and indep by chapters (as well as all of them if chapter == "---")
  chapter_overview <- chapter_overview[!chapter_overview$.variable_type %in% c("chr", "int", "dbl"), ]
  chapter_overview <- dplyr::ungroup(chapter_overview)
  chapter_overview <- dplyr::distinct(chapter_overview,
                                      .data$chapter,
                                      .data$.variable_name,
                                      .data$indep_cols_df,
                                      .keep_all = FALSE)
  chapter_overview$chapter <- as.character(chapter_overview$chapter)
  indeps <- tidyr::unchop(chapter_overview, cols = "indep_cols_df")
  indeps <- unique(indeps$indep_cols_df$.variable_name)
  data <- data[, names(data) %in% c(chapter_overview$.variable_name, indeps, dots$mesos_var)]

  fs::dir_create(fs::path(dots$path, "_flexi"))
  filepath_rds_data <- fs::path(dots$path, "_flexi", "flexi_data.RDs")
  save(list=c("data", "dots", "chapter_overview"), file = filepath_rds_data)

  template <- system.file("template", "telescope.qmd", package = "saros")
  readLines(con = file(template))


}
