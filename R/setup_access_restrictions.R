
read_main_password_file <- function(file) {
  utils::read.table(file = file, header = TRUE,
                    sep=":")
}


append_main_password_file <- function(x=".main_htpasswd_private",
                                      usernames="admin",
                                      plaintext_passwords="admin") {
  if(!file.exists(x)) {
    cli::cli_abort(c("Cannot find {.file {x}}.",
                     "An empty file must be created manually first, due to security precautions.",
                     "Check that the file has been made available to you."))
  }
  if(length(usernames) != length(plaintext_passwords)) {
    cli::cli_abort("Lengths of {.arg usernames} and {.arg plaintext_passwords} do not match.")
  }
  master_table <- read_main_password_file(file = x)
  new_table <- data.frame(username = usernames, password = plaintext_passwords)
  duplicates <- new_table$username[new_table$username %in% master_table$username]
  if(length(duplicates)>0) cli::cli_warn(c(i="usernames {usernames} already exist in {.file {x}}, ignoring these.",
                                           i="Delete these lines manually in the file if you want to change passwords."))
  new_table <- new_table[!new_table$username %in% master_table$username, ]
  out <- rbind(master_table, new_table)
  write_htpasswd_file(x = out, file = x, header=TRUE)
  cli::cli_progress_done("Successfully registered password credentials!")
  x
}


refer_main_password_file <- function(x=".main_htpasswd_private",
                                    usernames = "admin",
                                    ...,
                                    log_rounds = 12,
                                    append_users = FALSE,
                                    password_input = c("prompt", "8", "10", "12", "16")) {

  password_input <- rlang::arg_match(password_input, multiple = FALSE)
  # Read in x, split it into usernames and plaintext passwords, encrypt the passwords, return a table
  if(!file.exists(x)) {
    cli::cli_abort(c(x="Cannot find {.file {x}}.",
                     i="Check that the file has been made available to you"))
  }
  master_table <- read_main_password_file(file = x)
  out <-
  lapply(usernames, FUN = function(user) {
    passwd <- master_table[master_table$username == user, "password"]
    if(length(passwd)>1) cli::cli_abort("Multiple entries found for username {user}.")
    if(!rlang::is_string(passwd) || nchar(passwd)==0) {
      if(isFALSE(append_users)) cli::cli_abort("Unable to find password for username {user}.")
      if(password_input == "prompt") {

        passwd <- rstudioapi::askForPassword(prompt = stringi::stri_c("Enter password for new user: ", user))
        if(is.null(passwd)) return(NULL)

      } else {
        if(length(password_input)>1 ||
           is.na(as.integer(password_input)) ||
           as.integer(password_input)<1) {

          cli::cli_abort("Password input must be a positive integer stored as string.")

        }
        passwd <- sample(x = c(letters, LETTERS, 0:9),
                         size = as.integer(password_input),
                         replace = TRUE)
        passwd <- stringi::stri_c(passwd, collapse="", ignore_null = TRUE)
      }

      append_main_password_file(x = x,
                                usernames = user,
                                plaintext_passwords = passwd)
    }
    passwd <- bcrypt::hashpw(password = passwd, salt = bcrypt::gensalt(log_rounds = log_rounds))
    rlang::set_names(x = passwd, nm = user)
  })
  unlist(out)
}



write_htpasswd_file <- function(x, file, header=FALSE) {
  utils::write.table(x = x, file = file,
                     quote = FALSE, sep = ":",
                     col.names = if(rlang::is_true(header)) c("username", "password") else FALSE,
                     row.names = FALSE,
                     fileEncoding = "UTF-8")

}

obtain_mesos_folders_from_parent_folder <- function(x) {
  if(!rlang::is_string(x) || !file.exists(x)) {
    cli::cli_abort("{.arg x} does not exist: {.file {x}}")
  }
  list.dirs(path = x, full.names = FALSE, recursive = FALSE)
}


validate_access_folder_paths <- function(remote_basepath,
                                         local_basepath = "_site",
                                         rel_path_base_to_parent_of_user_restricted_folder =
                                           file.path( "Reports",
                                                      "2023",
                                                      "Mesos"),
                                         warn = FALSE) {
  rel_path_base_to_parent_of_user_restricted_folder <- file.path(local_basepath, rel_path_base_to_parent_of_user_restricted_folder)
  warnabort_fn <- if(isTRUE(warn)) cli::cli_warn else cli::cli_abort
  for(path in c(remote_basepath, local_basepath, rel_path_base_to_parent_of_user_restricted_folder)) {
    if(!rlang::is_string(path)) {
      warnabort_fn("{.arg {path}} must be a string.")
    }
  }
}


create_htaccess <-
  function(remote_basepath = "/home/",
           local_basepath = "_site",
           rel_path_base_to_parent_of_user_restricted_folder = file.path("Reports", "2022", "Mesos"),
           local_main_password_path = ".main_htpasswd_public",
           username_folder_matching_df = NULL,
           universal_usernames = c("admin"),
           log_rounds = 12,
           append_users = TRUE,
           password_input = "prompt") {

    abs_path_parents <-
      file.path(local_basepath,
              rel_path_base_to_parent_of_user_restricted_folder)

    local_subfolders_sets <- lapply(abs_path_parents, obtain_mesos_folders_from_parent_folder)
    names(local_subfolders_sets) <- rel_path_base_to_parent_of_user_restricted_folder

    lapply(seq_along(local_subfolders_sets), function(i) {

      lapply(X = seq_along(local_subfolders_sets[[i]]), function(j) {
        ### .htaccess
        folder_name <- local_subfolders_sets[[i]][j]
        user_names <-
          if(is.data.frame(username_folder_matching_df)) {
            username_folder_matching_df$username[username_folder_matching_df$folder == folder_name]
          } else {
            folder_name
          }

        content <- paste0('AuthName "Saros-report access: ', folder_name,
                          '"
AuthUserFile ', file.path(remote_basepath,
                          names(local_subfolders_sets)[i],
                          folder_name,
                          '.htpasswd'), '
AuthType Basic
Require valid-user
AddHandler server-parsed .html')
        outpath <- file.path(local_basepath,
                             names(local_subfolders_sets)[i],
                             folder_name,
                             ".htaccess")

        writeLines(text = content, con = outpath)

        ### .htpasswd

        credentials <- refer_main_password_file(x = local_main_password_path,
                                               usernames = unique(c(user_names, universal_usernames)),
                                               log_rounds = log_rounds,
                                               append_users = append_users,
                                               password_input = password_input)
        credentials <- data.frame(username=names(credentials),
                                  password=unname(credentials),
                                  row.names = NULL, stringsAsFactors = FALSE)

        write_htpasswd_file(x= credentials,
                            file = file.path(local_basepath,
                                             names(local_subfolders_sets)[i],
                                             folder_name,
                                             ".htpasswd"),
                            header=FALSE)
      })
    })
    invisible()

  }

create__headers_file <- function(remote_basepath = "/home/", # Not used in this function, included for consistency
                                 local_basepath = "_site",
                                 rel_path_base_to_parent_of_user_restricted_folder = file.path("Reports", "2022", "Mesos"),
                                 local_main_password_path = ".main_htpasswd_public",
                                 username_folder_matching_df = NULL,
                                 universal_usernames = c("admin"),
                                 log_rounds = 12,
                                 append_users = TRUE,
                                 password_input = "prompt") {

  abs_path_parents <-
    file.path(local_basepath,
              rel_path_base_to_parent_of_user_restricted_folder)


  local_subfolders_sets <- lapply(abs_path_parents, obtain_mesos_folders_from_parent_folder)
  names(local_subfolders_sets) <- rel_path_base_to_parent_of_user_restricted_folder


  out <-
    lapply(seq_along(local_subfolders_sets), function(i) {

    lapply(seq_along(local_subfolders_sets[[i]]), FUN = function(j) {

      folder_name <- local_subfolders_sets[[i]][j]
      user_names <- if(is.data.frame(username_folder_matching_df)) {
        username_folder_matching_df$username[username_folder_matching_df$folder == folder_name]
      } else {
        folder_name
      }

      credentials <- refer_main_password_file(x = local_main_password_path,
                                             usernames = unique(c(user_names, universal_usernames)),
                                             log_rounds = log_rounds,
                                             append_users = append_users,
                                             password_input = password_input)

      path <- file.path(names(local_subfolders_sets)[i], folder_name)
      credentials_flattened <- stringi::stri_c(user_names,
                                               ":", unname(credentials[user_names]),
                                               collapse = " ", ignore_null = TRUE)
      stringi::stri_c(path, "/*\n  Basic-Auth: ", credentials_flattened)
    })
    })

  out_file <- stringi::stri_c(local_basepath, .Platform$file.sep, "_headers") #No file.path because of _headers lacking extension
  out <- unlist(unlist(out))
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
#' @param warn Flag. Whether to provide warning or error if paths do not exist.
#' @param local_main_password_path String. Path to main file containing all usernames and passwords formatted with a colon between username and password.
#' @param username_folder_matching_df Data frame. If NULL (default), will use folder names as usernames. Otherwise, a data frame with two columns: "folder" and "username" where "folder" is the name of the folder and "username" is the username for that folder.
#' @param universal_usernames Character vector. Usernames in local_main_htpasswd_path which always have access to folder
#' @param log_rounds Integer, number of rounds in the bcrypt algorithm. The higher the more time consuming and harder to brute-force.
#' @param append_users Boolean, if TRUE (default) will create new users and add them to local_main_password_path. See also password_input.
#' @param password_input String, either "prompt" which asks the user for input. Alternatively, a number stored as string for a generated random password of said length: "8", "10", "12", "16"
#' @param type Character vector. "netlify" will create _headers file used for Netlify. "apache" will create .htaccess and .htpasswd files used for general FTP-servers.
#'
#' @return String, the path to the newly created _headers-file or .htaccess files.
#' @export
setup_access_restrictions <- function(remote_basepath = "/home/",
                                      local_basepath,
                                      rel_path_base_to_parent_of_user_restricted_folder = file.path("Reports", "2022", "Mesos"),
                                      warn = TRUE,
                                      local_main_password_path = ".main_htpasswd_public",
                                      username_folder_matching_df = NULL,
                                      universal_usernames = c("admin"),
                                      log_rounds = 12,
                                      append_users = TRUE,
                                      password_input = "prompt",
                                      type = c("netlify", "apache")) {

  remote_basepath <- stringi::stri_replace_last_regex(remote_basepath, pattern = "/", "")

  for(rel_path_base_to_parent_of_user_restricted_folder_string in rel_path_base_to_parent_of_user_restricted_folder) {
    validate_access_folder_paths(remote_basepath = remote_basepath,
                                 local_basepath = local_basepath,
                                 rel_path_base_to_parent_of_user_restricted_folder = rel_path_base_to_parent_of_user_restricted_folder)
  }

  if(!rlang::is_null(username_folder_matching_df) &&
     (!inherits(username_folder_matching_df, "data.frame") ||
     !all(c("folder", "username") %in% colnames(username_folder_matching_df)))) {
    cli::cli_abort("{.arg username_folder_matching_df} must be a data.frame with columns 'folder' and 'username', not {.obj_type_friendly {username_folder_matching_df}}.")
    ## Assume df has multiple usernames per folder, and multiple folders per username
  }


  if(any("netlify" == type)) {
    create__headers_file(remote_basepath = remote_basepath, # Not used in this function, included for consistency
                         local_basepath = local_basepath,
                         rel_path_base_to_parent_of_user_restricted_folder = rel_path_base_to_parent_of_user_restricted_folder,
                         local_main_password_path = local_main_password_path,
                         username_folder_matching_df = username_folder_matching_df,
                         universal_usernames = universal_usernames,
                         log_rounds = log_rounds,
                         append_users = append_users,
                         password_input = password_input)
  }
  if(any("apache" == type)) {
    create_htaccess(remote_basepath = remote_basepath, # Not used in this function, included for consistency
                    local_basepath = local_basepath,
                    rel_path_base_to_parent_of_user_restricted_folder = rel_path_base_to_parent_of_user_restricted_folder,
                    local_main_password_path = local_main_password_path,
                    username_folder_matching_df = username_folder_matching_df,
                    universal_usernames = universal_usernames,
                    log_rounds = log_rounds,
                    append_users = append_users,
                    password_input = password_input)
  }

}
