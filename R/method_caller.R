# are all elements of list x identical to each other?
compare_many <- function(x) {
  all(unlist(lapply(as.list(x[-1]),
                     FUN = function(.x) identical(.x, x[[1]])))) ||
  nrow(x[[1]])==1
}


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
#' @param element_name *Element name*
#'
#'   `scalar<character>` // *default:* `"uni_cat_prop_plot"`
#'
#'   One of the element_names, see `draft_report()`.
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
gen_element_and_qmd_snippet3 <-
  function(element_name = "uni_cat_prop_plot",
           chapter_overview_section,
           data,
           mesos_group = NULL,
           chapter_folderpath_absolute,
           chapter_foldername,
           element_folderpath_absolute,
           element_folderpath_relative,
           grouping_structure = NULL,
           ...,
           call = rlang::caller_env()) {


    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)

    stopifnot(inherits(data, "data.frame") || inherits(data, "survey"))

    element_folderpath_absolute <- file.path(chapter_folderpath_absolute, element_name)
    element_folderpath_relative <- file.path(chapter_foldername, element_name)
    dir.create(element_folderpath_absolute, recursive = TRUE, showWarnings = FALSE)

    if(dplyr::n_distinct(chapter_overview_section$.variable_type) != 1 || # Later add check that all items contain the same indep_cols_df
       dplyr::n_distinct(chapter_overview_section$.variable_label_prefix) != 1) return("")


    grouping_structure <- dplyr::group_vars(chapter_overview_section)
    grouping_structure <- grouping_structure[!grouping_structure %in% "chapter"]

    section_key <- chapter_overview_section
    section_key <- dplyr::ungroup(section_key)
    section_key <- dplyr::distinct(section_key, dplyr::pick(tidyselect::all_of(grouping_structure)))
    section_key <- dplyr::group_by(section_key, dplyr::pick(tidyselect::all_of(grouping_structure)))


    if(nrow(section_key)>1) cli::cli_warn("Something weird going on in grouping.")

    obj_name <- stringi::stri_c(list_valid_obj_name(section_key,
                                                    max_width = dots$max_width_obj),
                               if(rlang::is_string(mesos_group)) "_", mesos_group,
                               ignore_null=TRUE)


    ## Only for filenames

    filename_prefix <- make_filename_prefix(
      grouping_structure = grouping_structure,
      chapter_overview_section = chapter_overview_section,
      max_width_obj = dots$max_width_obj,
      mesos_group = mesos_group)

    y_col_names <- unique(chapter_overview_section$.variable_name)
    y_col_pos <- match(y_col_names, colnames(data))



    variable_prefix <-
      if(any(names(section_key) == ".variable_name_prefix") &&
         dplyr::n_distinct(section_key$.variable_name_prefix)==1) unique(section_key$.variable_name_prefix)

    rlang::exec(prepare_chunk,
                element_name = element_name,
                chapter_overview_section = chapter_overview_section,
                data = data,
                y_col_pos = y_col_pos,
                y_col_names = y_col_names,
                mesos_group = mesos_group,
                filepaths = filepaths,
                obj_name = obj_name,
                variable_prefix = variable_prefix,
                element_folderpath_relative,
                element_folderpath_absolute,
                filename_prefix,
                !!!dots)
  }

