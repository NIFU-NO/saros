#' @export
make_content.chr_table_docx <-
  function(...) {
    dots <- rlang::list2(...)

    # Call HTML version to get data.frame
    df <- make_content.chr_table_html(...)

    # Early exit if empty
    if (nrow(df) == 0) {
      if (isTRUE(dots$docx_return_object)) {
        return(data.frame())
      }
      docx_file <- use_docx(docx_template = dots$docx_template)
      if (!is.null(dots$path)) {
        print(docx_file, target = dots$path)
      } else {
        docx_file
      }
      return(docx_file)
    }

    # Return data.frame if requested
    if (isTRUE(dots$docx_return_object)) {
      return(df)
    }

    # Embed in rdocx using officer::body_add_table()
    docx_file <- use_docx(docx_template = dots$docx_template)
    docx_file <- officer::body_add_table(docx_file, df)

    if (!is.null(dots$path)) print(docx_file, target = dots$path) else docx_file
  }
