if (!exists(".saros.env")) .saros.env <- NULL

.onLoad <- function(libname, pkgname) {
  # Initialize the .saros.env environment if not already set
  if (!exists(".saros.env")) .saros.env <<- new.env(parent = emptyenv())

  .saros.env$summary_data_sort1 <<-
    c(".top", ".upper", ".mid_upper", ".lower", ".mid_lower", ".bottom")
  .saros.env$summary_data_sort2 <<-
    c( # Can this be constructed on the fly from class or crosstable_empty_output()? Or opposite?
      ".variable_name",
      ".variable_position",
      ".variable_label", ".variable_label_prefix",
      ".category",
      ".count", ".count_se",
      ".count_per_dep", ".count_per_dep_se",
      ".count_per_indep_group", ".count_per_indep_group_se",
      ".proportion", ".proportion_se",
      ".mean", ".mean_se",
      ".data_label",
      ".comb_categories",
      ".sum_value",
      ".id",
      ".tooltip",
      ".onclick"
    )
  .saros.env$data_label_opts <<-
    c(
      "proportion", "percentage", "percentage_bare",
      "count", "mean", "median"
    )

  .saros.env$ignore_args <<- c(
    "data",
    "dep",
    "indep",
    "chapter_overview",
    "chapter_structure",
    "call",
    "n_y",
    "n_cats_y",
    "ggobj",
    "..."
  )


  fn_opt_list <- list(
    makeme = makeme,
    make_link = make_link,
    n_rng = n_rng,
    n_range = n_range,
    fig_height_h_barchart = fig_height_h_barchart,
    girafe = girafe
  )

  for (fn_name in names(fn_opt_list)) {
    .saros.env[[paste0(fn_name, "_defaults")]] <<-
      lapply(
        formals(fn_opt_list[[fn_name]])[!names(formals(fn_opt_list[[fn_name]])) %in% .saros.env$ignore_args],
        eval
      )
  }
  .saros.env$makeme_defaults$type <<- .saros.env$makeme_defaults$type[1]
  .saros.env$makeme_defaults$showNA <<- .saros.env$makeme_defaults$showNA[1]
  .saros.env$makeme_defaults$data_label <<- .saros.env$makeme_defaults$data_label[1]
  .saros.env$fig_height_h_barchart_defaults$legend_location <<- .saros.env$fig_height_h_barchart_defaults$legend_location[1]

  # Initialize global options with the factory defaults if not already set
  .saros_options <- getOption("saros", list())
  for (fn in names(fn_opt_list)) {
    if (is.null(.saros_options[[paste0(fn, "_defaults")]])) {
      .saros_options[[paste0(fn, "_defaults")]] <- .saros.env[[paste0(fn, "_defaults")]]
      options(saros = .saros_options)
    }
  }
}
