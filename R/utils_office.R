
use_docx <- function(docx_template = NULL) {
  if(!is.null(docx_template)) {
    if(is.character(docx_template) &&
       length(docx_template) == 1L &&
       file.exists(docx_template)) {
      officer::read_docx(path = docx_template)
    } else docx_template
  } else officer::read_docx()
}


get_docx_dims <- function(docx_file) {
  docx_dims <- officer::docx_dim(docx_file)
  docx_dims <- c(w =
                   docx_dims$page[["width"]] -
                   docx_dims$margins[["left"]] -
                   docx_dims$margins[["right"]],
                 h =
                   docx_dims$page[["height"]] -
                   docx_dims$margins[["top"]] -
                   docx_dims$margins[["bottom"]])
}


get_block_caption <-
  function(data,
           cols_pos,
           docx_file,
           label_separator = NULL,
           caption_style = NULL,
           caption_autonum = NULL) {

    if(!is.null(label_separator)) {


      main_question <-
        get_raw_labels(data = data, cols_pos = cols_pos) %>%
        get_main_question2(label_separator = label_separator)

      caption_style <-
        if(!is.null(caption_style)) caption_style else "Normal"

      block_caption <-
        officer::block_caption(
          label = main_question,
          style = caption_style,
          autonum = caption_autonum)

      officer::body_add_caption(x = docx_file, value = block_caption)
    }
  }


get_docx_height <-
  function(height_fixed,
           height_per_col,
           minimum_height,
           n_col) {
  min(c(height_fixed + height_per_col*n_col,
        minimum_height))

}
