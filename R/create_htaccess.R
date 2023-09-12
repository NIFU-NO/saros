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
           local_basepath = "reports/2023V/org",
           local_subfolders = basename(fs::dir_ls(local_basepath,
                                                  all = FALSE,
                                                  recurse = FALSE,
                                                  type = "directory")),
           local_main_htpasswd_path = file.path(Sys.getenv("USERPROFILE"), "NIFU",
                                                "Metode - Sensitivt - Sensitivt",
                                                ".main_htpasswd_public"),
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

    lapply(X = local_subfolders, function(.x) {
      ### .htaccess
      con <- file(file.path(local_basepath, .x, ".htaccess"), "w")
      content <- paste0('AuthName "NIFUs mesos-rapporter: ', .x, '"
AuthUserFile ', file.path(remote_basepath, .x, '.htpasswd'), '
AuthType Basic
Require valid-user
AddHandler server-parsed .html')
      writeLines(text = content, con = con)
      close(con)


### .htpasswd

      content <- utils::read.delim(file = local_main_htpasswd_path, header = FALSE,
                            sep = ":", quote = "", col.names = c("username", "hash"),
                            colClasses = "character", strip.white = FALSE, blank.lines.skip = TRUE,
                            encoding = "UTF-8", fileEncoding = "UTF-8")
      content <- content[content$username %in% c(.x, universal_usernames),]
      # con <- file(, "w")
      utils::write.table(x = content, file = file.path(local_basepath, .x, ".htpasswd"),
                  quote = FALSE, sep = ":", col.names = FALSE, row.names = FALSE,
                  fileEncoding = "UTF-8")
      # close(con)
    })
    stringi::stri_c(local_basepath, .Platform$file.sep, local_subfolders, "/.htaccess",
                    ignore_null = TRUE)
  }


add_entry_local_main_htpasswd <-
  function(username = stringi::stri_c(sample(letters, size=5, replace=TRUE), collapse=""),
           password = stringi::stri_c(sample(letters, size=5, replace=TRUE), collapse=""),
           local_main_htpasswd_public_path =
             file.path(Sys.getenv("USERPROFILE"), "NIFU",
                       "Metode - Sensitivt - Sensitivt",
                       ".main_htpasswd_public"),
           local_main_htpasswd_private_path =
             file.path(Sys.getenv("USERPROFILE"), "NIFU",
                       "Metode - Sensitivt - Sensitivt",
                       ".main_htpasswd_private"),
           gensalt_log_rounds = 12) {

    if(!file.exists(local_main_htpasswd_private_path)) {
      cli::cli_abort(c("Cannot find {.arg local_main_htpasswd_private_path}.",
                       "An empty file must be created manually first, due to security precautions.",
                       "Check that the file has been made available to you"))
    }
    if(!file.exists(local_main_htpasswd_public_path)) {
      cli::cli_abort(c("Cannot find {.arg local_main_htpasswd_public_path}.",
                       "An empty file must be created manually first, due to security precautions.",
                       "Check that the file has been made available to you"))
    }
    if(length(username) != length(password)) cli::cli_abort("{.arg username} length does not match {.arg password} length.")

    handle_credentials <- function(username = username,
                                   password = password,
                                   local_main_htpasswd_path) {

      credentials_added <- stringi::stri_c(username, ":", password, collapse="\n")
      cat(credentials_added, file = local_main_htpasswd_path, append = TRUE)
    }

      handle_credentials(local_main_htpasswd_path = local_main_htpasswd_private_path)
      password <- unlist(lapply(password, function(x) bcrypt::hashpw(password = x,
                                 salt = bcrypt::gensalt(log_rounds = gensalt_log_rounds))))
      handle_credentials(local_main_htpasswd_path = local_main_htpasswd_public_path)
    cli::cli_inform("Successfully registered password credentials.")
  }


#' Create a _headers file for Netlify publication
#'
#' @param site_path String, path to where to locate the _headers file
#' @param mesos_paths Character vector of relative paths within site_path to the locked directories
#' @param mesos_usernames,mesos_passwords Character vector of respective usernames and passwords for the mesos_paths
#' @param global_username,global_password String, username and password that always gives access to a mesos_paths path. Defaults to "admin"
#'
#' @return String, the path to the _headers file
#' @export
#'
#' @examples create__headers_file(site_path=tempdir())
create__headers_file <- function(site_path="_site",
                                 mesos_paths=paste0("rapporter/Barnehageleder/2022H/mesos/",
                                                    c("BI", "DMMH", "NHH_og_AFF", "OsloMet", "UiA", "UiT", "USN")),
                                 mesos_usernames=c("BI", "DMMH", "NHH_og_AFF", "OsloMet", "UiA", "UiT", "USN"),
                                 mesos_passwords=c("BI", "DMMH", "NHH_og_AFF", "OsloMet", "UiA", "UiT", "USN"),
                                 global_username="admin",
                                 global_password="arturead") {

  if(length(mesos_paths) != length(mesos_usernames) ||
     length(mesos_usernames) != length(mesos_passwords)) {
    cli::cli_abort("Lengths of {.arg mesos_paths}, {.arg mesos_usernames}, and {.arg mesos_passwords} do not match.")
  }
  global_credentials <- stringi::stri_c(
    global_username, ":", global_password,
    ignore_null = TRUE, collapse = " ")
  out <- stringi::stri_c(mesos_paths, "/*\n  Basic-Auth: ",
                         mesos_usernames, ":", mesos_passwords, " ",
                         global_credentials, "\n",
                         ignore_null = TRUE, collapse = "")

  out_file <- file.path(site_path, "_headers")
  cat(out, file=out_file, sep = "\n")

  out_file


}
