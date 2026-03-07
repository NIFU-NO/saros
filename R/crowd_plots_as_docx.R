#' Write Plots to Word Document (DOCX)
#'
#' @description
#' High-level function to write a list of mschart plots directly to a Word
#' document file, with optional section headings and chart labels. Simplifies
#' the workflow for generating DOCX reports from Quarto/RMarkdown by handling
#' all officer boilerplate internally.
#'
#' @param plot_list Either:
#'   - A named list of mschart objects (from `makeme(..., type = "cat_plot_docx", docx_return_object = TRUE)`)
#'   - A `saros_officer_plots` object (from [crowd_plots_as_officer()])
#' @param path Character. File path where the DOCX should be saved (e.g., `"output.docx"`).
#' @param docx_template Optional template passed to saros' internal `use_docx()` helper.
#'   Can be a file path string, `NULL` for default template, or FALSE to skip template.
#' @param heading_style Character. officer paragraph style for section headings (plot names).
#'   Default is `"heading 2"`.
#' @param prefix_style Character. officer paragraph style for prefix text (main question labels).
#'   Default is `"Normal"`. Only used when `add_dep_label_prefix = TRUE`.
#' @param add_dep_label_prefix Logical. If `TRUE` (default), adds the main question text
#'   (from [get_dep_label_prefix()]) as a paragraph before each chart.
#' @param chart_width Numeric or `NULL`. Width in inches for charts. If `NULL` (default),
#'   uses saros' internal `get_docx_dims()` helper based on template page layout.
#' @param chart_height Numeric or `NULL`. Height in inches for charts. If `NULL` (default),
#'   uses saros' internal `get_docx_dims()` helper based on template page layout.
#' @param extract_metadata Logical. If `TRUE` (default), automatically extracts metadata
#'   from plots using [crowd_plots_as_officer()] when `plot_list` is a plain list.
#'   Ignored if `plot_list` is already a `saros_officer_plots` object.
#'
#' @return Invisible file path to the created DOCX file.
#'
#' @details
#' This function provides a simple, single-call interface for writing Word documents
#' from saros plots, ideal for Quarto workflows where you want:
#' - **One QMD file → One DOCX file** (no chapter merging)
#' - **Minimal code in .qmd chunks** (just `makeme()` + `crowd_plots_as_docx()`)
#' - **Automatic layout handling** (page dimensions, chart sizing, heading styles)
#'
#' **Typical Workflow:**
#' 1. Generate mschart objects with `makeme(..., type = "cat_plot_docx", docx_return_object = TRUE, crowd = ...)`
#' 2. Call `crowd_plots_as_docx(plots, path = "report.docx")` to write the file
#'
#' **Empty Plot Lists:**
#' If `plot_list` contains no valid mschart objects, the function:
#' - Issues a warning via `cli::cli_warn()`
#' - Creates an empty DOCX file at the specified path
#' - Returns the path invisibly
#'
#' @seealso
#' - [crowd_plots_as_officer()] for obtaining a structured plot object
#' - [crowd_plots_as_tabset()] for the Quarto/HTML equivalent
#' - [makeme()] for creating plots with crowd parameter
#' - [officer::read_docx()] for manual DOCX assembly
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate mschart objects for Word
#' plots <- makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3,
#'   crowd = c("target", "others"),
#'   mesos_var = "f_uni",
#'   mesos_group = "Uni of A",
#'   type = "cat_plot_docx",
#'   docx_return_object = TRUE
#' )
#'
#' # Write directly to a Word file
#' crowd_plots_as_docx(plots, path = "survey_results.docx")
#'
#' # With custom template and styling
#' crowd_plots_as_docx(
#'   plots,
#'   path = "styled_report.docx",
#'   docx_template = "my_template.docx",
#'   heading_style = "Heading 1",
#'   prefix_style = "Body Text",
#'   chart_width = 6,
#'   chart_height = 4
#' )
#'
#' # Without prefix labels
#' crowd_plots_as_docx(
#'   plots,
#'   path = "minimal_report.docx",
#'   add_dep_label_prefix = FALSE
#' )
#' }
crowd_plots_as_docx <- function(
  plot_list,
  path,
  docx_template = NULL,
  heading_style = "heading 2",
  prefix_style = "Normal",
  add_dep_label_prefix = TRUE,
  chart_width = NULL,
  chart_height = NULL,
  extract_metadata = TRUE
) {
  # Validate path
  if (!rlang::is_string(path)) {
    cli::cli_abort(
      "{.arg path} must be a single character string, not {.obj_type_friendly {path}}."
    )
  }

  # Validate styles
  if (!rlang::is_string(heading_style)) {
    cli::cli_abort(
      "{.arg heading_style} must be a single character string, not {.obj_type_friendly {heading_style}}."
    )
  }

  if (!rlang::is_string(prefix_style)) {
    cli::cli_abort(
      "{.arg prefix_style} must be a single character string, not {.obj_type_friendly {prefix_style}}."
    )
  }

  # Validate boolean flags
  if (!rlang::is_bool(add_dep_label_prefix)) {
    cli::cli_abort(
      "{.arg add_dep_label_prefix} must be TRUE or FALSE, not {.obj_type_friendly {add_dep_label_prefix}}."
    )
  }

  if (!rlang::is_bool(extract_metadata)) {
    cli::cli_abort(
      "{.arg extract_metadata} must be TRUE or FALSE, not {.obj_type_friendly {extract_metadata}}."
    )
  }

  # Validate chart dimensions (NULL or numeric)
  if (
    !is.null(chart_width) &&
      (!is.numeric(chart_width) || length(chart_width) != 1)
  ) {
    cli::cli_abort(
      "{.arg chart_width} must be NULL or a single numeric value, not {.obj_type_friendly {chart_width}}."
    )
  }

  if (
    !is.null(chart_height) &&
      (!is.numeric(chart_height) || length(chart_height) != 1)
  ) {
    cli::cli_abort(
      "{.arg chart_height} must be NULL or a single numeric value, not {.obj_type_friendly {chart_height}}."
    )
  }

  # Convert plot_list to saros_officer_plots if needed
  if (!inherits(plot_list, "saros_officer_plots")) {
    plot_list <- crowd_plots_as_officer(
      plot_list,
      extract_metadata = extract_metadata
    )
  }

  # Handle empty plot list
  if (plot_list$n_plots == 0) {
    cli::cli_warn(
      "No valid plots to insert. Creating empty DOCX file at {.file {path}}."
    )
    doc <- use_docx(docx_template = docx_template)
    print(doc, target = path)
    return(invisible(path))
  }

  # Initialize document
  doc <- use_docx(docx_template = docx_template)

  # Get chart dimensions if not specified
  dims <- get_docx_dims(doc)
  if (is.null(chart_width)) {
    chart_width <- dims["w"]
  }
  if (is.null(chart_height)) {
    chart_height <- dims["h"]
  }

  # Assemble document
  for (plot_name in names(plot_list$plots)) {
    # Add heading
    doc <- officer::body_add_par(doc, value = plot_name, style = heading_style)

    # Add prefix text if requested and metadata exists
    if (add_dep_label_prefix && !is.null(plot_list$metadata)) {
      prefix_text <- plot_list$metadata[[plot_name]]$dep_label_prefix
      if (!is.null(prefix_text) && nzchar(prefix_text)) {
        doc <- officer::body_add_par(
          doc,
          value = prefix_text,
          style = prefix_style
        )
      }
    }

    # Add chart
    doc <- mschart::body_add_chart(
      doc,
      chart = plot_list$plots[[plot_name]],
      width = chart_width,
      height = chart_height
    )
  }

  # Write file
  print(doc, target = path)

  invisible(path)
}
