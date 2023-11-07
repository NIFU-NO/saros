embed_flexi <- function(data,
                        chapter_overview,
                        ...) {

  dots <- update_dots(dots = rlang::list2(...),
                      caller_function = "flexi")

  # must handle dep and indep by chapters (as well as all of them if chapter == "---")
  chapter_overview <- chapter_overview[!chapter_overview$.variable_type_dep %in% c("chr", "int", "dbl"), ]
  chapter_overview <- dplyr::ungroup(chapter_overview)
  chapter_overview <- dplyr::distinct(chapter_overview,
                                      .data$chapter,
                                      .data$.variable_name_dep,
                                      .data$.variable_name_indep,
                                      .keep_all = FALSE)
  chapter_overview$chapter <- as.character(chapter_overview$chapter)
  data <- data[, names(data) %in% unique(c(chapter_overview$.variable_name_dep,
                                           chapter_overview$.variable_name_indep,
                                           dots$auxiliary,
                                           dots$mesos_var))]

  dir.create(file.path(dots$path, "_flexi"), recursive = TRUE, showWarnings = FALSE)
  filepath_rds_data <- file.path(dots$path, "_flexi", "flexi_data.RDs")
  save(list=c("data", "dots", "chapter_overview"), file = filepath_rds_data)

  template <- system.file("template", "telescope.qmd", package = "saros")
  readLines(con = file(template))


}
