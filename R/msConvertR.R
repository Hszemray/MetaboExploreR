#' msConvertR
#'
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

msConvertR <- function (input_directory,  output_directory){

  # Validate input_directory
  validate_input_directory(input_directory)

  # Validate msconvert is installed in C:/Program Files/
  validate_proteowizard_skyline()

  # Validate wiff files
  file_paths <- validate_file_types(input_directory)

  # Check if wiff files are found
  if (length(file_paths) == 0) {
    stop("No files supported found in the specified project directory for processing. Please check the directory and try again.")
  }

  # Set plateIDs
  plateIDs <- str_remove(str_extract(file_paths, "[^/]+$"), "\\.(wiff|raw|d|RAW|yep|td2)$")

  #Set failed/successful plates
  failed_plates <- c()
  successful_plates <- c()

  # Process each plate
  for (plateID in plateIDs) {
    tryCatch({
      file_path <- file_paths[grepl(plateID, file_paths)]
      msConvertR_mzml_conversion(input_directory, output_directory, file_path, plateID)
      successful_plates <- c(successful_plates, plateID)
    }, error = function(e) {
      message(paste("Error processing plate ",plateID,": ",e$message))
      log_error(paste("Error processing plate ",plateID,": ",e$message))
      failed_plates <<- c(failed_plates, plateID)
    })
  }

  # Display results
  message("Processing complete! \n")
  if (length(successful_plates) > 0){
    message("Successful plates:\n", paste(successful_plates, collapse = "\n"))
  }
  if (length(failed_plates) > 0){
    message("Failed plates:\n", paste(failed_plates, collapse = "\n "))
  }

  # Check if all plates failed
  if (length(successful_plates) == 0) {
    stop("All plates failed. Halting script.")
  }

  message("\n Converted mzml files are located in ",paste(output_directory))
}



