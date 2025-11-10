if (!exists(".saros.env")) {
  .saros.env <- NULL
}

.onLoad <- function(libname, pkgname) {
  # Initialize the .saros.env environment if not already set
  if (!exists(".saros.env")) {
    .saros.env <<- new.env(parent = emptyenv())
  }

  .saros.env$summary_data_sort1 <<-
    c(".top", ".upper", ".mid_upper", ".lower", ".mid_lower", ".bottom")
  .saros.env$summary_data_sort2 <<-
    c(
      # Can this be constructed on the fly from class or crosstable_empty_output()? Or opposite?
      ".variable_name",
      ".variable_position",
      ".variable_label",
      ".variable_label_prefix",
      ".category",
      ".count",
      ".count_se",
      ".count_per_dep",
      ".count_per_dep_se",
      ".count_per_indep_group",
      ".count_per_indep_group_se",
      ".proportion",
      ".proportion_se",
      ".mean",
      ".mean_se",
      ".median",
      ".data_label",
      ".comb_categories",
      ".sum_value",
      ".id",
      ".tooltip",
      ".onclick",
      ".percentage",
      # Order columns from centralized sorting system
      ".dep_order",
      ".indep_order",
      ".category_order"
    )
  # Whitelists for direct column-based sorting (B1 strategy)
  .saros.env$allowed_dep_sort_columns <<-
    c(
      ".count",
      ".proportion",
      ".mean",
      ".median",
      ".sum_value"
    )
  .saros.env$allowed_indep_sort_columns <<-
    c(
      ".count",
      ".count_total_indep",
      ".mean",
      ".median",
      ".sum_value"
    )
  .saros.env$data_label_opts <<-
    c(
      "proportion",
      "percentage",
      "percentage_bare",
      "count",
      "mean",
      "median"
    )

  # Makeme output types that skip factor level processing
  .saros.env$types_skip_factor_processing <<-
    c(
      "sigtest_table_html",
      "int_table_html",
      "int_plot_html",
      "chr_table_html"
    )

  # Makeme output types that skip multiple indep validation
  .saros.env$types_skip_multiple_indep_validation <<-
    c(
      "sigtest_table_html",
      "chr_table_html"
    )

  # Common column names used throughout the package
  .saros.env$col_names <<-
    list(
      variable_name = ".variable_name",
      variable_label = ".variable_label",
      variable_label_prefix = ".variable_label_prefix",
      variable_label_suffix = ".variable_label_suffix",
      category = ".category",
      count = ".count",
      proportion = ".proportion",
      percentage = ".percentage"
    )

  .saros.env$ignore_args <<- c(
    "data",
    "plots",
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
    girafe = girafe,
    txt_from_cat_mesos_plots = txt_from_cat_mesos_plots
  )

  for (fn_name in names(fn_opt_list)) {
    .saros.env[[paste0(fn_name, "_defaults")]] <<-
      lapply(
        formals(fn_opt_list[[fn_name]])[
          !names(formals(fn_opt_list[[fn_name]])) %in% .saros.env$ignore_args
        ],
        eval
      )
  }
  .saros.env$makeme_defaults$type <<- .saros.env$makeme_defaults$type[1]
  .saros.env$makeme_defaults$showNA <<- .saros.env$makeme_defaults$showNA[1]
  .saros.env$makeme_defaults$data_label <<- .saros.env$makeme_defaults$data_label[
    1
  ]
  .saros.env$fig_height_h_barchart_defaults$legend_location <<- .saros.env$fig_height_h_barchart_defaults$legend_location[
    1
  ]

  # Initialize global options with the factory defaults if not already set
  .saros_options <- getOption("saros", list())
  for (fn in names(fn_opt_list)) {
    if (is.null(.saros_options[[paste0(fn, "_defaults")]])) {
      .saros_options[[paste0(fn, "_defaults")]] <- .saros.env[[paste0(
        fn,
        "_defaults"
      )]]
      options(saros = .saros_options)
    }
  }
}

.onUnload <- function(libpath) {
  # Clean up global options
  options(saros = NULL)
  invisible()
}
