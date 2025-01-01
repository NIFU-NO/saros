# #' Aggregate _metadata.yml
# #'
# #' For use within a qmd-file to obtain list parameters from _metadata.yml-files in the subfolders.
# #' Parameters are inherited and updated from parent to child folders.
# #'
# #' @export
# #'
# aggregate_metadata_yml <- function() {
#     # Get the current input directory
#     current_dir <- knitr::current_input(dir = TRUE) |> fs::path_dir()

#     # Initialize an empty list to store metadata
#     aggregated_metadata <- list()

#     # Traverse up the directory hierarchy until no _metadata.yml is found
#     dirs <- list()
#     tmp <- current_dir
#     while (TRUE) {
#         metadata_file <- fs::path(tmp, "_metadata.yml")

#         if (fs::file_exists(metadata_file)) {
#             dirs <- c(tmp, dirs)
#         } else {
#             break
#         }

#         parent_folder <- fs::path(tmp, "..") |> fs::path_norm()

#         # Stop if the parent folder is the same as the current folder (root reached)
#         if (tmp == parent_folder) {
#             break
#         }

#         tmp <- parent_folder
#     }

#     # Traverse from the upper-most directory downward
#     for (dir in dirs) {
#         metadata_file <- fs::path(dir, "_metadata.yml")

#         if (fs::file_exists(metadata_file)) {
#             # Read the metadata and merge it into the aggregated metadata
#             metadata_content <- yaml::read_yaml(metadata_file)
#             aggregated_metadata <- utils::modifyList(aggregated_metadata, metadata_content)
#         }
#     }

#     return(aggregated_metadata)
# }

# aggregate_metadata_yaml <- aggregate_metadata_yml
