#' @export
make_content.chr_table_html <-
  function(...) {
    dots <- rlang::list2(...)

    # Setup data and early exit if empty
    setup_result <- setup_table_data(dots)
    if (setup_result$should_return) {
      return(data.frame())
    }

    out <- setup_result$data

    # Convert category to character and filter
    out$.category <- as.character(out$.category)
    out <- dplyr::filter(
      out,
      !is.na(.data[[".category"]]),
      .data[[".category"]] != ""
    )

    # Select columns excluding summary sort columns
    out <- out[,
      c(
        colnames(out)[!colnames(out) %in% .saros.env$summary_data_sort2],
        ".category"
      ),
      drop = FALSE
    ]

    # Rename category column
    names(out)[names(out) == ".category"] <- " "

    out
  }
