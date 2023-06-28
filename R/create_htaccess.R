#' Create Bulk of .htaccess files in local folders, having valid AuthUserFile when uploaded to ftp-server at remote_basepath
#'
#' @param remote_basepath String
#' @param local_basepath String
#' @param local_subfolders Character vector
#' @param local_main_htpasswd_path string. Path to main htpasswd file containing all usernames and passwords
#' @param universal_usernames Character vector. Usernames in local_main_htpasswd_path which always have access to folder
#'
#' @return paste0(local_folders, ".htaccess")
#' @export
#' @examples
#' \dontrun{
#' create_htaccess(remote_basepath =
#'                "/home/stagiede/public_html/stephan/SSN/rapporter/2023V/org",
#'                 local_basepath =
#'      "O - Outputs to be published/1 Main site - testing/rapporter/2023V/org",
#'                 local_main_htpasswd_path =
#'  "C:/Users/py128/NIFU/Metode - Sensitivt - Sensitivt/.main_htpasswd_public",
#' universal_usernames = c("admin", "nifu"))
#' }
create_htaccess <-
  function(remote_basepath = "/home/",
           local_basepath = "rapporter/2023V/org",
           local_subfolders = basename(fs::dir_ls(local_basepath,
                                                  all = FALSE,
                                                  recurse = FALSE,
                                                  type = "directory")),
           local_main_htpasswd_path = ".main_htpasswd_public",
           universal_usernames = c("admin", "nifu")) {

    if(!rlang::is_string(remote_basepath)) cli::cli_abort("{.arg remote_basepath} must be a string.")
    if(!rlang::is_string(local_basepath)) cli::cli_abort("{.arg local_basepath} must be a string.")
    if(!rlang::is_string(local_main_htpasswd_path)) cli::cli_abort("{.arg local_main_htpasswd_path} must be a string.")
    if(!rlang::is_character(local_subfolders) ||
       !all(fs::dir_exists(path = paste0(local_basepath, .Platform$file.sep, local_subfolders)))) {
      cli::cli_abort("{.arg local_subfolders} must be a character vector of existing folder names.")
    }

    remote_basepath <- fs::path_tidy(remote_basepath)
    local_basepath <- fs::path_tidy(local_basepath)

    purrr::walk(.x = local_subfolders, .f = ~{
      ### .htaccess
      con <- file(fs::path(local_basepath, .x, ".htaccess"), "w")
      content <- paste0('AuthName "NIFUs mesos-rapporter: ', .x, '"
AuthUserFile ', fs::path(remote_basepath, .x, '.htpasswd'), '
AuthType Basic
Require valid-user
AddHandler server-parsed .html')
      writeLines(text = content, con = con)
      close(con)


### .htpasswd

      content <- read.delim(file = local_main_htpasswd_path, header = FALSE,
                            sep = ":", quote = "", col.names = c("username", "hash"),
                            colClasses = "character", strip.white = FALSE, blank.lines.skip = TRUE,
                            encoding = "UTF-8", fileEncoding = "UTF-8")
      content <- content[content$username %in% c(.x, universal_usernames),]
      # con <- file(, "w")
      write.table(x = content, file = fs::path(local_basepath, .x, ".htpasswd"),
                  quote = FALSE, sep = ":", col.names = FALSE, row.names = FALSE,
                  fileEncoding = "UTF-8")
      # close(con)
    })
    paste0(local_basepath, .Platform$file.sep, local_subfolders, "/.htaccess")
  }
