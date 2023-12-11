
# Function to recursively copy folders
recursive_copy <- function(from, to) {
  # Create the directory structure in the destination
  dir.create(to, recursive = TRUE, showWarnings = FALSE)

  # List all files in the source directory
  files <- list.files(from, full.names = TRUE)

  for (file in files) {
    # If it's a directory, recursively copy its contents
    if (file.info(file)$isdir) {
      recursive_copy(file, file.path(to, basename(file)))
    } else {
      # If it's a file, copy it to the destination
      file.copy(file, to)
    }
  }
}

copy_content <- function(from_files, from_folders, to_folder) {
  # Create the destination directory if it doesn't exist
  if (!dir.exists(to_folder)) {
    dir.create(to_folder, recursive = TRUE, showWarnings = TRUE)
  } else {
    # If the directory exists, clear its contents
    file.remove(list.files(to_folder, full.names = TRUE))
  }


  # Copy each file to the destination folder
  for (file in from_files) {
    if (file.exists(file)) {
      file.copy(file, to_folder)
    }
  }

  # Copy each folder to the destination folder
  for (folder in from_folders) {
    if (dir.exists(folder)) {
      recursive_copy(folder, file.path(to_folder, basename(folder)))
    }
  }
}


# Function to create directories if they don't exist
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

#' Simple wrapper for quarto::quarto_render() that temporarily sets LC_ALL="C" and takes the processing time
#'
#' @param site_path String, path to render.
#' @param ... Dynamic dots forwarded to quarto::quarto_render
#'
#' @return Nothing
#' @export
quarto_render_saros <- function(site_path,
                                ...) {

  dots <- rlang::list2(...)
  # Render the site using Quarto
  old_locale <- Sys.getlocale("LC_ALL")
  old_wd <- getwd()
  # on.exit(setwd(site_path), add = TRUE)
  on.exit(Sys.setenv(LC_ALL = old_locale), add = TRUE)
  Sys.setenv(LC_ALL = "C")

  # Execute the rendering
  system.time(
    quarto::quarto_render(
      input = site_path,
      # as_job = FALSE,
      output_format = c("all"),
      !!!dots
    )
  )

  utils::browseURL(site_path)

  invisible()
}



#' Convenience function to prepare, copy and render website
#'
#' Rendering a website on a Sharepoint/OneDrive location can be a pain
#' due to long filepaths above 260 characters, which causes read errors. This
#' function simplifies things by copying a site from local_basepath to a site_basepath,
#' sets up basic access authentication files (either Netlify's _headers file or
#' regular Apache .htaccess/.htpasswd files), as well as optionally copying
#' in an existing Netlify _publish.yaml-file.
#'
#' @param remote_basepath String. FTP servers often have a subdomain path location different from "/home/". Adjust this here.
#' @param local_basepath String. Path to where your QMD-files are located (the site to be rendered).
#' @param site String. Path to where to copy folders and files to setup what Quarto needs to build a project.
#' @param rel_path_base_to_parent_of_user_restricted_folder String. Path going from basepath to the folder containing folders to password-protect.
#' @param from_folders,from_files Character vector of folders and files to copy into the site path.
#' @param overwrite Flag. Defaults to FALSE to ensure you know what you are doing. If TRUE, will delete all files and folders in site!
#' @param prompt Flag. Whether to ask the user if they are certain. Defaults to TRUE.
#'
#' @return local_basepath
#' @export
#'
prepare_safe_render <- function(remote_basepath = "/home/",
                                 from_folders = NULL,
                                 from_files = NULL,
                                 local_basepath = getwd(),
                                 site = tempdir(),
                                 rel_path_base_to_parent_of_user_restricted_folder = "Reports",

                                 overwrite = FALSE,
                                 prompt = TRUE) {

  # dots <- rlang::list2(...)

  # Initial checks of folders
  if(!dir.exists(local_basepath) || length(dir(local_basepath))==0) {
    cli::cli_abort("Nothing to render: Reports directory is missing/empty: {.path {local_basepath}}.")
  }

  if(rlang::is_character(rel_path_base_to_parent_of_user_restricted_folder)) {
    local_mesos_paths <- file.path(local_basepath, rel_path_base_to_parent_of_user_restricted_folder)
      if(any(!dir.exists(local_mesos_paths)) || any(lengths(dir(local_mesos_paths))==0)) {
        local_mesos_paths <- local_mesos_paths[!dir.exists(local_mesos_paths) | lengths(dir(local_mesos_paths))==0]
        cli::cli_abort("Nothing to render: Reports directory is missing/empty: {.path {local_mesos_paths}}.")
      }
  }
  ensure_dir_exists(site)

  # Emptying folders, or keeping them if freeze/cache, then copying files. EXPERIMENTAL
  if(prompt) {
    answer <- utils::menu(graphics = TRUE,
                          title = "Sure you want to overwrite the local site folder {.path {site}}?",
                          choices = c("Y", "N")) # Default to "No"
    if (toupper(answer) == "N") return()
  }

  if(isTRUE(overwrite)) {
    unlink(site, recursive = TRUE)
  }
  copy_content(from_folders = from_folders,
               from_files = from_files,
               to_folder = site)

}



#' Create Data Frame Containing Email Drafts with User Credentials
#'
#' @inheritParams prepare_safe_render
#' @param email_data_frame Data.frame/tibble with (at least) emails and usernames
#' @param email_col String, name of email column
#' @param username_col String, name of username column in email_data_frame
#' @param local_main_password_path Path to a local .htpasswd file containing username:password header and : as separator.
#' @param ignore_missing_emails Flag, defaults to FALSE. Whether usernames existing in password file but not email file will result in warnings.
#' @param email_body,email_subject String, subject line and email body respectively. Supports glue syntax referring to columns found in the email data frame or password file.
#' @param ... Dynamic dots forwarded to quarto::quarto_render
#' @return Data.frame
#' @export
create_email_credentials <- function(local_basepath = getwd(),
                              rel_path_base_to_parent_of_user_restricted_folder,
                              email_data_frame,
                              email_col= "email",
                              username_col = "username",
                              local_main_password_path = ".htpasswd_private",
                              ignore_missing_emails = FALSE,
                              email_body = "Your login credentials for www.example.net are \nUsername: {username},\nPassword: {password}",
                              email_subject = "User credentials for website.",
                              ...) {

  mesos_paths <- file.path(local_basepath, rel_path_base_to_parent_of_user_restricted_folder)
  usernames <- basename(list.dirs(mesos_paths, full.names = FALSE, recursive = FALSE))

  # MUST READ IN THE PLAINTEXT PASSWORDS WITH THIS FUNCTION
  credentials <- read_main_password_file(file=local_main_password_path)

  colnames(credentials) <- c(username_col, "password")
  emails <- vctrs::vec_slice(email_data_frame,
                             !is.na(email_data_frame[[username_col]]) &
                               !is.na(email_data_frame[[email_col]]) &
                               email_data_frame[[username_col]] %in% unique(as.character(usernames)))

  in_email_not_in_cred <- setdiff(emails[[username_col]], credentials[[username_col]])
  if(length(in_email_not_in_cred)>0) {
    cli::cli_warn("Usernames in emails data set not found in password file ({.path {local_main_password_path}}): {in_email_not_in_cred}")
  }
  # in_cred_not_in_email <- setdiff(credentials[[username_col]], emails[[username_col]])
  # if(length(in_cred_not_in_email)>0 && rlang::is_false(ignore_missing_emails)) {
  #   cli::cli_warn("Usernames in password file ({.path {local_main_password_path}}) not found in emails data set: {in_cred_not_in_email}")
  # }

  out <- dplyr::inner_join(emails, credentials, by=username_col, relationship = "many-to-one")

  body <- as.character(glue::glue_data(.x = out, email_body))
  if(length(body)==1) body <- rep(body, length=nrow(out))
  subject <- as.character(glue::glue_data(.x = out, email_subject))
  if(length(subject)==1) subject <- rep(subject, length=nrow(out))

  data.frame(body = body,
             to = out[[email_col]],
             subject = subject)

}
