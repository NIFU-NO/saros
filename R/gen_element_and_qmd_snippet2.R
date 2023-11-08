#' Mass Create Elements of A Certain Type
#'
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#'
#' @param chapter_overview_section *Overview of chapter section*
#'
#'   `obj:<data.frame>|obj:<tbl_df>` // Required
#'
#'   Data frame (or tibble, possibly grouped). Must contain column 'dep'
#'   with similar items. See `draft_report()`.
#'
#' @param element_folderpath_absolute,element_folderpath_relative *Absolute and relative folder paths*
#'
#'   `scalar<character>` // Required
#'
#'   Both the absolute and relative folderpaths are required, as strings.
#'
#' @param grouping_structure *Vector of groups*
#'
#'  `vector<character>` // *default:* `NULL` (`Optional`)
#'
#'  Internal usage.
#'
#' @return Named list of elements, where each element can UNFINISHED.
#' @importFrom rlang !!!
#' @keywords internal
#'
gen_element_and_qmd_snippet2 <-
  function(chapter_overview_section,
           data,
           mesos_group = NULL,
           chapter_folderpath_absolute,
           chapter_foldername,
           element_folderpath_absolute,
           element_folderpath_relative,
           grouping_structure = NULL,
           ...,
           call = rlang::caller_env()) {

    if(all(is.na(chapter_overview_section$.variable_name_dep))) return("")
    if(any(is.na(chapter_overview_section$.variable_name_dep))) {
      cli::cli_abort("chapter_overview_section cannot contain .variable_name_dep NA and non-NA.")
    }


    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)

    stopifnot(inherits(data, "data.frame") || inherits(data, "survey"))
    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)



    if(dplyr::n_distinct(chapter_overview_section$.variable_type_dep, na.rm = FALSE) > 1) {
      cli::cli_warn("chapter_overview_section has dep variables {chapter_overview_section$.variable_name_dep} containing multiple variable types: {chapter_overview_section$.variable_type_dep}.")
      return("")
    }
    if(dplyr::n_distinct(chapter_overview_section$.variable_label_prefix_dep, na.rm = FALSE) != 1) {
      cli::cli_warn("chapter_overview_section has dep variables {chapter_overview_section$.variable_name_dep} containing multiple variable labels: {chapter_overview_section$.variable_label_prefix_dep}.")
      return("")
    }
    # if(dplyr::n_distinct(chapter_overview_section$.variable_type_indep, na.rm = FALSE) != 1) {
    #   cli::cli_warn("chapter_overview_section has indep variables {chapter_overview_section$.variable_name_indep} containing multiple variable types: {chapter_overview_section$.variable_type_indep}.")
    #   return("")
    # }
    # if(dplyr::n_distinct(chapter_overview_section$.variable_label_prefix_indep, na.rm = FALSE) != 1) {
    #   cli::cli_warn("chapter_overview_section has indep variables {chapter_overview_section$.variable_name_indep} containing multiple variable labels: {chapter_overview_section$.variable_label_prefix_dep}.")
    #   return("")
    # }
    if(!all(chapter_overview_section$.variable_name_dep %in% c(colnames(data)))) {
      cli::cli_warn("chapter_overview_section contains dep variables not in data: {unique(chapter_overview_section$.variable_name_dep)[!unique(chapter_overview_section$.variable_name_dep) %in% c(colnames(data), NA)]}.")
      return("")
    }
    if(!all(chapter_overview_section$.variable_name_indep %in% c(colnames(data), NA))) {
      cli::cli_warn("chapter_overview_section contains indep variables not in data: {unique(chapter_overview_section$.variable_name_indep)[!unique(chapter_overview_section$.variable_name_indep) %in% c(colnames(data), NA)]}.")
      return("")
    }
    if(dplyr::n_distinct(chapter_overview_section$.element_name, na.rm = TRUE) != 1) {
      cli::cli_abort("chapter_overview_section must contain only one kind of element_type. Problem with {unique(chapter_overview_section$chapter)}.")
      return("")
    }


    element_folderpath_absolute <- file.path(chapter_folderpath_absolute, unique(chapter_overview_section$.element_name))
    element_folderpath_relative <- file.path(chapter_foldername, unique(chapter_overview_section$.element_name))
    dir.create(element_folderpath_absolute, recursive = TRUE, showWarnings = FALSE)

    grouping_structure <- dplyr::group_vars(chapter_overview_section)
    grouping_structure <- grouping_structure[!grouping_structure %in% "chapter"]

    section_key <- chapter_overview_section
    section_key <- dplyr::ungroup(section_key)
    section_key <- dplyr::distinct(section_key, dplyr::pick(tidyselect::all_of(grouping_structure)))
    section_key <- dplyr::group_by(section_key, dplyr::pick(tidyselect::all_of(grouping_structure)))


    if(nrow(section_key)>1) cli::cli_warn("Something weird going on in grouping.")

    # Creating a universal object name


    obj_name <- create_obj_name(section_key = section_key,
                                max_width = dots$max_width_obj,
                                indep_vars = chapter_overview_section$.variable_name_indep,
                                mesos_group = mesos_group,
                                indep_sep_string = "_BY_",
                                mesos_sep_string = "_FOR_")
    ## Only for filenames

    filename_prefix <-
      make_filename_prefix(
        grouping_structure = grouping_structure,
        chapter_overview_section = chapter_overview_section,
        max_width_obj = dots$max_width_obj,
        mesos_group = mesos_group)

    obj_name_indep <-
      if(dplyr::n_distinct(chapter_overview_section$.variable_name_indep, na.rm = TRUE) == 1) {
        stringi::stri_c("_BY_",
                        unique(chapter_overview_section$.variable_name_indep)[!is.na(unique(chapter_overview_section$.variable_name_indep))])
      }
    if(length(obj_name_indep)>0 && all(is.na(obj_name_indep))) obj_name_indep <- NULL

    filename_prefix <- stringi::stri_c(filename_prefix,
                                       obj_name_indep,
                                       ignore_null=TRUE)


    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)


    y_col_names <- unique(chapter_overview_section$.variable_name_dep)
    x_col_names <- unique(chapter_overview_section$.variable_name_indep)
    x_col_names <- x_col_names[!is.na(x_col_names)]
    if(length(x_col_names)==0) x_col_names <- NULL



    variable_prefix <-
      if(any(names(section_key) == ".variable_name_prefix_dep") &&
         dplyr::n_distinct(section_key$.variable_name_prefix_dep)==1) unique(section_key$.variable_name_prefix_dep)

    common_data_type <-
      get_common_data_type(data,
                           col_pos = unique(chapter_overview_section$.variable_name_dep))


    colour_palette <- NULL
    if(any(unique(chapter_overview_section$.variable_type_dep) %in% c("fct", "ord")) &&
       any(unique(chapter_overview_section$.variable_type_dep) %in% c("int", "dbl", "chr"))) browser()

    if(all(unique(chapter_overview_section$.variable_type_dep) %in% c("fct", "ord"))) {

      common_levels <-
        get_common_levels(data,
                          col_pos = unique(chapter_overview_section$.variable_name_dep))

      colour_palette <-
        get_colour_set(
          x = common_levels,
          common_data_type = common_data_type,
          colour_palette_nominal = dots$colour_palette_nominal,
          colour_palette_ordinal = dots$colour_palette_ordinal,
          colour_na = dots$colour_na,
          colour_2nd_binary_cat = dots$colour_2nd_binary_cat,
          categories_treated_as_na = dots$categories_treated_as_na[dots$categories_treated_as_na %in% common_levels])

    }


    plot_height <- estimate_plot_height(y_col_pos = unique(chapter_overview_section$.variable_name_dep),
                                        x_cols = unique(chapter_overview_section$.variable_name_indep),
                                        element_name = unique(chapter_overview_section$.element_name),
                                        vertical = dots$vertical,
                                        label_separator = dots$label_separator,
                                        x_axis_label_width = dots$x_axis_label_width,
                                        data = data,
                                        showNA = dots$showNA,
                                        plot_height_multiplier = dots$plot_height_multiplier,
                                        plot_height_fixed_constant = dots$plot_height_fixed_constant,
                                        plot_height_max = dots$plot_height_max,
                                        plot_height_min = dots$plot_height_min,
                                        vertical_height = dots$vertical_height)


    out<-
    rlang::exec(prepare_chunk,
                element_name = unique(chapter_overview_section$.element_name),
                chapter_overview_section = chapter_overview_section,
                data = data,
                mesos_group = mesos_group,
                filepaths = filepaths,
                obj_name = obj_name,
                variable_prefix = variable_prefix,
                plot_height = plot_height,
                colour_palette = colour_palette,
                !!!dots)
      out
  }
