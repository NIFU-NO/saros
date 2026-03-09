#' Universal Output Function for Crowd Plots and Tables
#'
#' @description
#' Automatically detects the appropriate output method based on the rendering
#' context and input type. Simplifies workflows by providing a single function
#' that works for both Quarto/knitr rendering (HTML tabsets/tables) and officer-based
#' DOCX generation.
#'
#' @param plot_list Either:
#'   - A named list of ggplot2 objects (for HTML plots)
#'   - A named list of mschart objects (for DOCX plots)
#'   - A `saros_officer_plots` object (from [crowd_plots_as_officer()])
#'   - A data.frame or list of data.frames (for tables)
#' @param path Character. File path for DOCX output (e.g., `"output.docx"`).
#'   Only used when not in a knitr/Quarto rendering context.
#'   Default: `"crowd_output.docx"`.
#' @param force_format Character. Force a specific output format:
#'   - `"auto"` (default): Auto-detect based on context
#'   - `"html"`: Force HTML tabset output via [crowd_plots_as_tabset()]
#'   - `"docx"`: Force DOCX file output via [crowd_plots_as_docx()]
#' @param ... Additional arguments passed to [crowd_plots_as_tabset()] or
#'   [crowd_plots_as_docx()] depending on the detected/forced format.
#'
#' @return
#' - In knitr/Quarto context: Invisibly returns `NULL` (side effect: prints tabset markdown)
#' - Outside knitr context: Invisibly returns the DOCX file path
#'
#' @details
#' **Context Detection:**
#' The function uses `getOption("knitr.in.progress")` to detect if code is
#' running within a knitr/Quarto rendering context:
#' - **In knitr/Quarto** → Generates HTML tabset via [crowd_plots_as_tabset()]
#' - **Outside knitr** → Writes DOCX file via [crowd_plots_as_docx()]
#'
#' This allows the same code to work in multiple contexts:
#' - Quarto → HTML rendering
#' - Quarto → DOCX rendering (still uses HTML output in document)
#' - R script → Officer-based DOCX generation
#'
#' **Input Type Detection:**
#' The function automatically detects and handles different input types:
#' - ggplot2 objects → Uses [crowd_plots_as_tabset()] or [crowd_plots_as_docx()]
#' - mschart objects → Uses [crowd_plots_as_docx()]
#' - `saros_officer_plots` → Uses [crowd_plots_as_docx()]
#' - data.frame/tables → Uses [crowd_tables_as_tabset()] or writes to DOCX
#'
#' **Typical Workflow:**
#' ```r
#' # In a Quarto document - works for both HTML and DOCX output formats
#' plots <- makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3,
#'   crowd = c("target", "others"),
#'   mesos_var = "f_uni",
#'   mesos_group = "Uni of A",
#'   type = if (is_rendering()) "cat_plot_html" else "cat_plot_docx"
#' )
#' crowd_output(plots)  # Auto-detects context and renders appropriately
#' ```
#'
#' @seealso
#' - [crowd_plots_as_tabset()] for HTML tabset generation
#' - [crowd_plots_as_docx()] for DOCX file creation
#' - [is_rendering()] for context detection helper
#' - [makeme()] for creating plots with crowd parameter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: In a Quarto document (auto-detects HTML context)
#' plots <- makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3,
#'   crowd = c("target", "others"),
#'   mesos_var = "f_uni",
#'   mesos_group = "Uni of A",
#'   type = if (is_rendering()) "cat_plot_html" else "cat_plot_docx"
#' )
#' crowd_output(plots)
#'
#' # Example 2: In an R script (auto-detects non-knitr context, writes DOCX)
#' plots <- makeme(
#'   data = ex_survey,
#'   dep = b_1:b_3,
#'   crowd = c("target", "others"),
#'   mesos_var = "f_uni",
#'   mesos_group = "Uni of A",
#'   type = "cat_plot_docx"
#' )
#' crowd_output(plots, path = "my_report.docx")
#'
#' # Example 3: Force specific format
#' crowd_output(plots, force_format = "docx", path = "forced_output.docx")
#' }
crowd_output <- function(
  plot_list,
  path = "crowd_output.docx",
  force_format = c("auto", "html", "docx"),
  ...
) {
  force_format <- rlang::arg_match(force_format)

  # Determine output format
  if (force_format == "auto") {
    # Auto-detect: Are we in a knitr/Quarto rendering context?
    use_html <- is_rendering()
  } else {
    use_html <- force_format == "html"
  }

  # Detect input type
  is_table <- is.data.frame(plot_list) ||
              (is.list(plot_list) && length(plot_list) > 0 && is.data.frame(plot_list[[1]]))

  # Route to appropriate output function
  if (use_html) {
    # HTML output for knitr/Quarto rendering
    if (is_table) {
      crowd_tables_as_tabset(plot_list, ...)
    } else {
      crowd_plots_as_tabset(plot_list, ...)
    }
  } else {
    # DOCX file for officer-based generation
    if (is_table) {
      # For tables, write directly to DOCX using officer
      # Ensure plot_list is a list of data.frames
      table_list <- if (is.data.frame(plot_list)) list(table = plot_list) else plot_list

      # Create DOCX file
      docx_file <- use_docx(docx_template = list(...)$docx_template)

      # Add each table
      for (i in seq_along(table_list)) {
        tbl <- table_list[[i]]
        # Add heading if named
        if (!is.null(names(table_list)[i]) && nzchar(names(table_list)[i])) {
          docx_file <- officer::body_add_par(docx_file, names(table_list)[i],
                                             style = "heading 2")
        }
        docx_file <- officer::body_add_table(docx_file, tbl)
      }

      # Save and return path
      print(docx_file, target = path)
      invisible(path)
    } else {
      crowd_plots_as_docx(plot_list, path = path, ...)
    }
  }
}


#' Detect if Running in knitr/Quarto Rendering Context
#'
#' @description
#' Helper function to detect if code is running within a knitr/Quarto rendering
#' context. Useful for creating unified code that works in both interactive R
#' sessions and when rendering documents.
#'
#' @return Logical. `TRUE` if rendering a document, `FALSE` otherwise.
#'
#' @details
#' This function checks `getOption("knitr.in.progress")` which is set by knitr
#' during document rendering. This works for:
#' - Quarto documents (all output formats: HTML, DOCX, PDF, etc.)
#' - R Markdown documents
#' - Any knitr-based rendering systems
#'
#' Returns `FALSE` when running in:
#' - Interactive R sessions (RStudio console, R terminal)
#' - R scripts executed outside knitr
#' - Shiny applications (unless explicitly using knitr)
#'
#' @seealso [crowd_output()] for automatic context-aware output generation
#'
#' @export
#'
#' @examples
#' # Check if rendering a document
#' if (is_rendering()) {
#'   message("Rendering a document with knitr/Quarto")
#' } else {
#'   message("Running in regular R session")
#' }
#'
#' # Use for conditional logic
#' plot_type <- if (is_rendering()) "cat_plot_html" else "cat_plot_docx"
is_rendering <- function() {
  isTRUE(getOption("knitr.in.progress"))
}
