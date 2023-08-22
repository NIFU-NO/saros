
#' Easily upload folder to ftp-server, with safe storage of credentials
#'
#' @param folder_path String. Local path to upload.
#' @param ftp_path String. Remote path.
#' @param ftp_server String. Server, without "ftp://". Can be stored in .renviron for safe storage. Use SAROS_FTP_USERNAME
#' @param username String. Can be stored in .renviron for safe storage. Use SAROS_FTP_USERNAME
#' @param password String. Can be stored in .renviron for safe storage. Use SAROS_FTP_PASSWORD
#'
#' @return results from RCurl
#' @export
#'
upload_to_ftp <- function(folder_path,
                          ftp_path,
                          ftp_server,
                          username,
                          password) {
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  # Retrieve username and password from environment variables
  if(missing(ftp_server)) ftp_server <- Sys.getenv("SAROS_FTP_SERVER")
  if(missing(username)) username <- Sys.getenv("SAROS_FTP_USERNAME")
  if(missing(password)) password <- Sys.getenv("SAROS_FTP_PASSWORD")

  print(c(ftp_server = ftp_server,
             username = username))
  # Create a curl handle
  curl_handle <- RCurl::getCurlHandle(ftp.use.epsv = FALSE,
                                      ftp.create.missing.dirs = TRUE,
                                      userpwd = paste0(username, ":", password),
                                      ftp.ssl = TRUE,
                                      ssl.verifypeer = FALSE,
                                      .encoding = "UTF-8",
                                      timeout = 90)

  # Get a list of all files in the folder
  files <- fs::dir_ls(path = folder_path, recurse = TRUE, all = TRUE, type = "file")

  curl_multi_handles <- lapply(length(files), function(x) curl_handle)
  curl_handles <- RCurl::getCurlMultiHandle(.handles = curl_multi_handles)

  # Upload each file in a single session using the same curl handle
  results <- lapply(X = files, FUN = function(file) {
    # Extract the relative file path
    relative_path <- gsub(paste0("^", folder_path, "/?"), "", file)
    # Construct the remote path
    remote_path <- paste0("ftp://", ftp_server, .Platform$file.sep, ftp_path, .Platform$file.sep, relative_path)

    # Diagnostic print statements
    cat("Uploading file:", file, "\n")
    cat("To remote path:", remote_path, "\n")

    # Upload the file
    result <- tryCatch(
      expr = {
        RCurl::curlMultiPerform(curl = curl_handles)
        # RCurl::curlPerform(upload = file, url = remote_path, curl = curl_handle)
        cat("Upload successful.\n")
        },
      error = function(e) {
        cat("Error during upload:", e$message, "\n")
      }
    )

    # Return the result of the file transfer
    return(result)
  })

  # Return the results of all file transfers
  return(results)
}
