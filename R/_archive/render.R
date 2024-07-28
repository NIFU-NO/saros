
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


#' Create Data Frame Containing Email Drafts with User Credentials
#'
#' @param email_data_frame Data.frame/tibble with (at least) emails and usernames
#' @param email_col String, name of email column
#' @param username_col String, name of username column in email_data_frame
#' @param local_main_password_path Path to a local .htpasswd file containing username:password header and : as separator.
#' @param ignore_missing_emails Flag, defaults to FALSE. Whether usernames existing in password file but not email file will result in warnings.
#' @param email_body,email_subject String, subject line and email body respectively. Supports glue syntax referring to columns found in the email data frame or password file.
#' @param ... Dynamic dots forwarded to quarto::quarto_render
#' @return Data.frame
#' @export
create_email_credentials <-
  function(email_data_frame,
           email_col= "email",
           username_col = "username",
           local_main_password_path = ".htpasswd_private",
           ignore_missing_emails = FALSE,
           email_body = "Login credentials are \nUsername: {username},\nPassword: {password}",
           email_subject = "User credentials for website example.net.",
           ...) {

  # MUST READ IN THE PLAINTEXT PASSWORDS WITH THIS FUNCTION
  credentials <- read_main_password_file(file=local_main_password_path)

  colnames(credentials) <- c(username_col, "password")
  emails <- vctrs::vec_slice(email_data_frame,
                             !is.na(email_data_frame[[username_col]]) &
                               !is.na(email_data_frame[[email_col]]))

  in_email_not_in_cred <- setdiff(emails[[username_col]], credentials[[username_col]])
  if(length(in_email_not_in_cred)>0) {
    cli::cli_warn("Usernames in emails data set not found in password file ({.path {local_main_password_path}}): {in_email_not_in_cred}")
  }
  # in_cred_not_in_email <- setdiff(credentials[[username_col]], emails[[username_col]])
  # if(length(in_cred_not_in_email)>0 && isFALSE(ignore_missing_emails)) {
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
