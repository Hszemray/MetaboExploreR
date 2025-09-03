#' msConvertR
#'
#' #' Import specific functions from packages
#' @name import_external_functions
#' @importFrom stringr str_remove str_extract
NULL

#' This function is used to convert vendor files to `.mzml` files
#' using the `msconvert` tool from ProteoWizard.
#' @param input_directory The directory where the vendor files are located.
#' @param output_directory The directory where the converted `.mzml` files will be saved.
#' @return None. The function performs the conversion and restructures directories.
#' @export
#' @examples
#' \dontrun{
#' # example code
#'  msConvertR(input_directory = "path/to/input_directory", output_directory = "path/to/output_directory")
#'  }

msConvertR <- function (input_directory, output_directory) {
  # Validate input_directory
  validate_input_directory(input_directory)

  # Validate wiff files
  file_paths <- validate_file_types(input_directory)

  # Check if wiff files are found
  if (length(file_paths) == 0) {
    stop(
      "No files supported found in the specified project directory for processing. Please check the directory and try again."
    )
  }

  #Vendor file extentions
  vendor_extension_patterns <- "\\.(d|baf|fid|yep|tsf|tdf|mbi|wiff|wiff2|qgd|qgb|qgm|lcd|lcdproj|raw|uep|sdf|dat|wcf|wproj|wdata)$"

  # Set plateIDs
  plateIDs <- str_remove(str_extract(file_paths, "[^/]+$"),
                         vendor_extension_patterns)

  #Check install and run status of docker
  check_docker()

  # Process vendor files
  tryCatch({
    msConvertR_mzml_conversion(input_directory,
                               output_directory,
                               plateIDs,
                               vendor_extension_patterns)

  }, error = function(e) {
    message("Error processing vendor files: ", e$message)
  })

  # Notify user about converted files
  message(
    "\nConverted mzML files are located in ",
    file.path(output_directory, "plate_id", "data", "mzml")
  )

  # Directory structure messages
  if (input_directory == output_directory) {
    message(
      "\nNote: Input and output directories are the same.\n",
      "Vendor files have been relocated to ",
      file.path(output_directory, "plateID", "data", "raw_data")
    )
  } else {
    message(
      "\nNote: Input and output directories are different.\n",
      "The project structure has been created in ",
      output_directory,
      ".\n",
      "Vendor files are located in ",
      file.path(input_directory, "plateID", "data", "raw_data")
    )
  }

  message("Thank you for using msConvertR")

}
