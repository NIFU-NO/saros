#' @export
make_content.chr_table_html <-
  function(...) {
    dots <- rlang::list2(...)
    out <- dots$data_summary

    out$.category <- as.character(out$.category)
    out <- dplyr::filter(out, !is.na(.data[[".category"]]), .data[[".category"]] != "")
    out <- out[, c(colnames(out)[!colnames(out) %in% .saros.env$summary_data_sort2], ".category"), drop = FALSE]
    names(out)[names(out) == ".category"] <- " "

    out
  }
