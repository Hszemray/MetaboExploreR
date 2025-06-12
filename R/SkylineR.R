#'SkylineR
#'
#' This function processes `.wiff` files in a specified project directory and converts them to `.mzml` files, followed by various processing steps.
#'
#' @param project_directory User's path to project directory containing a folder named 'wiff' with selected .wiff and .wiff.scan files.
#' @param mrm_template_list User's path to desired MRM templates, must be in specified format. Can contain more than one template for multi-method projects.
#' @return Curated project directory containing .wiff and .mzml files, and Skyline exports.
#' @export
#' @examples
#' SkylineR(project_directory = "USER/PATH/TO/PROJECT/DIRECTORY", mrm_template_list = list("USER/PATH/TO/user_mrm_template1.csv", "USER/PATH/TO/user_mrm_template2.csv"))
SkylineR <- function(project_directory, mrm_template_list = list()) {
  # Validate project_directory
  validate_project_directory(project_directory)

  # Validate mrm_template_list
  validate_mrm_template_list(mrm_template_list)

  # Set wiff file paths from project file wiff
  wiff_file_paths <- list.files(path = paste0(project_directory, "/wiff"), pattern = ".wiff$", full.names = TRUE)

  # Check if wiff files are found
  if (length(wiff_file_paths) == 0) {
    stop("No .wiff files found in the specified project directory.")
  }

  # Set plateIDs
  plateIDs <- str_remove(str_extract(wiff_file_paths, "[^/]+$"), "\\.wiff$")

  # Process each plate
  for (plateID in plateIDs) {
    tryCatch({
      master_list <- setup_project(project_directory, plateID)
      master_list <- mzml_conversion(plateID, master_list)
      master_list <- import_mzml(plateID, master_list)
      master_list <- peak_picking(plateID, master_list)
    }, error = function(e) {
      message(paste("Error processing plate", plateID, ":", e$message))
      log_error(paste("Error processing plate", plateID, ":", e$message))
    })
  }

  # Final cleanup and archiving
  archive_raw_files(project_directory)
  message("Chromatograms and reports are now available per plate in your specified directory.\nPlease Run qcCheckR to calculate concentrations and QC data")
}



