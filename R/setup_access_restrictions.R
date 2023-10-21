read_main_password_file <- function(x=".main_htpasswd_private",
                                    usernames = "admin",
                                    log_rounds = 12,
                                    append_users = FALSE,
                                    password_input = c("prompt", "8", "10", "12", "16")) {
  password_input <- rlang::arg_match(password_input, multiple = FALSE)
  # Read in x, split it into usernames and plaintext passwords, encrypt the passwords, return a table
  if(!file.exists(x)) {
    cli::cli_abort(c(x="Cannot find {.file x}.",
                     i="Check that the file has been made available to you"))
  }
  master_table <- utils::read.table(file = x, header = FALSE, col.names = c("username", "password"), sep=":", tryLogical = FALSE)
  lapply(usernames, FUN = function(user) {
    passwd <- master_table[master_table$username == user, "password"]
    if(!rlang::is_string(passwd)) {
      if(isFALSE(append_users)) cli::cli_abort("Unable to find password for username {user}.")
      if(password_input == "prompt") {
        plaintext_password <- rstudioapi::askForPassword(prompt = stringi::stri_c("Enter password for new user: ", user))
      } else {
        plaintext_password <- sample(x = c(letters, LETTERS, 0:9),
                                     size = as.integer(password_input),
                                     replace = TRUE)
      }
      append_main_password_file(x = x,
                                usernames = user,
                                plaintext_passwords = plaintext_password)
    }
    passwd <- bcrypt::hashpw(password = passwd, salt = bcrypt::gensalt(log_rounds = log_rounds))
    rlang::set_names(x = passwd, nm = user)
  })
}


append_main_password_file <- function(x=".main_htpasswd_private",
                                      usernames="admin",
                                      plaintext_passwords="admin") {
  if(!file.exists(x)) {
    cli::cli_abort(c("Cannot find {.file x}.",
                     "An empty file must be created manually first, due to security precautions.",
                     "Check that the file has been made available to you."))
  }
  if(length(usernames) != length(plaintext_passwords)) {
    cli::cli_abort("Lengths of {.arg usernames} and {.arg plaintext_passwords} do not match.")
  }
  master_table <- utils::read.table(file = x, header = TRUE, col.names = c("username", "password"), sep=":", tryLogical = FALSE)
  new_table <- data.frame(username = usernames, password = plaintext_passwords)
  duplicates <- new_table$username[new_table$username %in% master_table$username]
  if(length(duplicates)>0) cli::cli_warn("usernames {usernames} already exist in x, ignoring these. Delete these manually in {.file x} if you want to change passwords.")
  new_table <- new_table[!new_table$username %in% master_table$username, ]
  out <- rbind(master_table, new_table)
  utils::write.table(x = out, file = x)
  cli::cli_progress_done("Successfully registered password credentials!")
  x
}

write_htpasswd_file <- function(x, file) {
  utils::write.table(x = x, file = file,
                     quote = FALSE, sep = ":", col.names = FALSE, row.names = FALSE,
                     fileEncoding = "UTF-8")

}

obtain_usernames_from_foldernames <- function(x) {
  if(!rlang::is_string(x) || !file.exists(x)) {
    cli::cli_abort("{.arg x} does not exist: {.file x}")
  }
  list.dirs(path = x, full.names = FALSE, recursive = FALSE)
}


validate_access_folder_paths <- function(remote_basepath,
                                         local_basepath = "_site",
                                         rel_path_base_to_parent_of_user_restricted_folder =
                                           file.path( "Reports",
                                                      "2023",
                                                      "Mesos")) {
  rel_path_base_to_parent_of_user_restricted_folder <- file.path(local_basepath, rel_path_base_to_parent_of_user_restricted_folder)
  for(path in c(remote_basepath, local_basepath, rel_path_base_to_parent_of_user_restricted_folder)) {
    if(!rlang::is_string(path)) {
      cli::cli_abort("{.arg {path}} must be a string.")
    }
  }
}


create_htaccess <-
  function(remote_basepath = "/home/",
           local_basepath = "_site",
           rel_path_base_to_parent_of_user_restricted_folder = file.path("Reports", "2022", "Mesos"),
           local_main_password_path = ".main_htpasswd_public",
           universal_usernames = c("admin"),
           log_rounds = 12,
           append_users = TRUE,
           password_input = "prompt") {

    paths <- validate_access_folder_paths(remote_basepath = remote_basepath,
                                   local_basepath = local_basepath,
                                   rel_path_base_to_parent_of_user_restricted_folder = rel_path_base_to_parent_of_user_restricted_folder)

    local_subfolders <-
      obtain_usernames_from_foldernames(x = file.path(local_basepath,
                                                    rel_path_base_to_parent_of_user_restricted_folder))

    lapply(X = local_subfolders, function(.x) {
      ### .htaccess
      con <- file(file.path(local_basepath, .x, ".htaccess"), "w")
      content <- paste0('AuthName "Saros-report access: ', .x, '"
AuthUserFile ', file.path(remote_basepath,
                          file.path(rel_path_base_to_parent_of_user_restricted_folder),
                          .x, '.htpasswd'), '
AuthType Basic
Require valid-user
AddHandler server-parsed .html')
      writeLines(text = content, con = con)
      close(con)


### .htpasswd

      credentials <- read_main_password_file(x = local_main_password_path,
                                         usernames = unique(c(.x, universal_usernames)),
                                         log_rounds = log_rounds,
                                         append_users = append_users,
                                         password_input = password_input)
      write_htpasswd_file(x= credentials, file = file.path(local_basepath, .x, ".htpasswd"))

    })
    file.path(local_basepath, local_subfolders, ".htaccess")
  }

create__headers_file <- function(remote_basepath = "/home/", # Not used in this function, included for consistency
                                 local_basepath = "_site",
                                 rel_path_base_to_parent_of_user_restricted_folder = file.path("Reports", "2022", "Mesos"),
                                 local_main_password_path = ".main_htpasswd_public",
                                 universal_usernames = c("admin"),
                                 log_rounds = 12,
                                 append_users = TRUE,
                                 password_input = "prompt") {

  abs_path_parent <- file.path(local_basepath,
                               rel_path_base_to_parent_of_user_restricted_folder)
  local_subfolders <-
    obtain_usernames_from_foldernames(x = abs_path_parent)


  out <-
    lapply(local_subfolders, FUN = function(.x) {


      credentials <- read_main_password_file(x = local_main_password_path,
                                             usernames = unique(c(.x, universal_usernames)),
                                             log_rounds = log_rounds,
                                             append_users = append_users,
                                             password_input = password_input)

      path <- file.path(rel_path_base_to_parent_of_user_restricted_folder, .x)
      credentials_flattened <- stringi::stri_c(.x, ":", credentials[[.x]], collapse = " ", ignore_null = TRUE)
      stringi::stri_c(path, "/*\n  Basic-Auth: ", credentials_flattened)
    })

  out_file <- stringi::stri_c(local_basepath, .Platform$file.sep, "_headers") #No file.path because of _headers lacking extension
  cat(out, file = out_file, sep = "\n")

  out_file


}




#' Setup files needed for basic password-based access restriction for website
#'
#' Create a _headers file for 'Netlify' publishing or a set of .htaccess and .htpasswd files (FTP)
#'  placed in the specific subfolders.
#'
#' @param remote_basepath String. Folder where site will be located if using FTP-server. Needed for .htaccess-files.
#' @param local_basepath String. Local folder for website, typically "_site".
#' @param rel_path_base_to_parent_of_user_restricted_folder String, relative path from basepath to the folder where the restricted folders are located. (E.g. the "mesos"-folder)
#' @param local_main_password_path String. Path to main file containing all usernames and passwords formatted with a colon between username and password.
#' @param universal_usernames Character vector. Usernames in local_main_htpasswd_path which always have access to folder
#' @param log_rounds Integer, number of rounds in the bcrypt algorithm. The higher the more time consuming and harder to brute-force.
#' @param append_users Boolean, if TRUE (default) will create new users and add them to local_main_password_path. See also password_input.
#' @param password_input String, either "prompt" which asks the user for input. Alternatively, a number stored as string for a generated random password of said length: "8", "10", "12", "16"
#' @param type Character vector. "netlify" will create _headers file used for Netlify. "apache" will create .htaccess and .htpasswd files used for general FTP-servers.
#'
#' @return String, the path to the newly created _headers-file or .htaccess files.
#' @export
setup_access_restrictions <- function(remote_basepath = "/home/",
                                      local_basepath = "_site",
                                      rel_path_base_to_parent_of_user_restricted_folder = file.path("Reports", "2022", "Mesos"),
                                      local_main_password_path = ".main_htpasswd_public",
                                      universal_usernames = c("admin"),
                                      log_rounds = 12,
                                      append_users = TRUE,
                                      password_input = "prompt",
                                      type = c("netlify", "apache")) {

  if(any("netlify" == type)) {
    create__headers_file(remote_basepath = remote_basepath, # Not used in this function, included for consistency
                         local_basepath = local_basepath,
                         rel_path_base_to_parent_of_user_restricted_folder = rel_path_base_to_parent_of_user_restricted_folder,
                         local_main_password_path = local_main_password_path,
                         universal_usernames = universal_usernames,
                         log_rounds = log_rounds,
                         append_users = append_users,
                         password_input = password_input)
  }
  if(any("apache" == type)) {
    create__headers_file(remote_basepath = remote_basepath, # Not used in this function, included for consistency
                         local_basepath = local_basepath,
                         rel_path_base_to_parent_of_user_restricted_folder = rel_path_base_to_parent_of_user_restricted_folder,
                         local_main_password_path = local_main_password_path,
                         universal_usernames = universal_usernames,
                         log_rounds = log_rounds,
                         append_users = append_users,
                         password_input = password_input)
  }

}
