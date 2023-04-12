#' Prints rdocx and fixes issues with mschart
#' 
#' mschart package has a small bug which this function rectifies.
#'
#' @param x rdocx object
#' @param target target file path
#'
#' @return target file path
### #' @export
#' @import officer
#' @importFrom utils unzip zip 
#' @importFrom readr read_file write_file
#' @importFrom purrr map_chr
#' @examples
print2.rdocx <- function(x, target=file.path(getwd(), "tmp.docx")) {
	# Unzip everything in temp, recode chart files to UTF-8 and return
	current_wd <- getwd()
	dir.create(tmp_zip_dir <- tempfile())
	tmp_zip <- tempfile(fileext = ".docx")
	print(x, target = tmp_zip)
	
	utils::unzip(zipfile = tmp_zip, exdir = tmp_zip_dir) %>%
		grep(pattern = "charts\\/.*\\.xml$", x = ., value = TRUE) %>%
		purrr::map_chr(., function(xml_file) {
			xml_content <-
				readr::read_file(xml_file) %>%
				iconv(x = ., from = "latin1", to = "UTF-8") %>%
				gsub('<c:title xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">\r\n      <c:tx>\r\n        <c:rich>\r\n          <a:bodyPr/>.*<c:autoTitleDeleted val=\"0\"/>',
					 replacement = '<c:autoTitleDeleted val="1"/>', x = .)
			readr::write_file(x = xml_content, file = xml_file, append = FALSE)
		})
	
	#### Replace with officer::pack_folder(folder=tmp_zip_dir, target=file.path(path, paste0(file_prefix, ".docx")))
	setwd(tmp_zip_dir)
	list.files(path = tmp_zip_dir, all.files = TRUE, recursive = TRUE, include.dirs = FALSE) %>%
		utils::zip(files=., zipfile = tmp_zip)
	setwd(current_wd)
	file.copy(from = tmp_zip, to = target, overwrite = TRUE, copy.date = TRUE)
	target
}