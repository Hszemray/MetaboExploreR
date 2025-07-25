
#mzML File Conversion Functions ----
#' Validate input Directory
#'
#' This function checks if the `input_directory` parameter is a single string and if the specified directory exists.
#'
#' @param input_directory A character string representing the path to the project directory.
#' @return TRUE if the validation is successful, otherwise an error is thrown.
#' @examples
#' \dontrun{
#' validate_project_directory("input_directory")
#' }
validate_input_directory <- function(input_directory) {
  # Check if input_directory is a single string
  if (!is.character(input_directory) || length(input_directory) != 1) {
    stop("input_directory must be a single string.")
  }

  # Check if the specified directory exists
  if (!dir.exists(input_directory)) {
    stop("The specified project directory does not exist.")
  }

  # Return TRUE if validation is successful
  message(paste("Accessing project directory ", input_directory))
}

###Primary Function----
#' msConvertR_mzml_conversion
#'
#' This function converts raw vendor files to mzML format using ProteoWizard's msconvert tool, restructures directories, and updates the script log.
#'
#' @param input_directory Directory path for vendor files.
#' @param output_directory Directory path for the mzml output.
#' @param file_path Path to the vendor file to be converted.
#' @param plateID Plate ID for the current plate.
#' @return Converted mzml files.
#' @examples
#' \dontrun{
#' msConvertR_mzml_conversion(input_directory, output_directory, file_path, plateID)
#' }
msConvertR_mzml_conversion <- function(input_directory, output_directory, file_path, plateID) {
  msConvertR_set_working_directory(input_directory)
  msConvertR_setup_project_directories(output_directory, plateID)
  command <- msConvertR_construct_command_for_terminal(file_path, output_directory)
  msConvertR_execute_command(command)
  msConvertR_restructure_directory(output_directory, plateID)
}

###Sub Functions----

#' msConvertR_setup_project_directories
#'
#' This function sets up project directories for each plate ID.
#'
#' @param output_directory Output directory for mzml files.
#' @param plateID Plate ID for the current plate.
#' @return None. The function sets up directories.
#' @examples
#' \dontrun{
#' msConvertR_setup_project_directories(output_directory, plateID)
#' }
msConvertR_setup_project_directories <- function(output_directory, plateID) {
    dir.create(paste0(output_directory, "/", plateID), showWarnings = FALSE)
    dir.create(paste0(output_directory, "/", plateID, "/data"), showWarnings = FALSE)
    dir.create(paste0(output_directory, "/", plateID, "/data/mzml"), showWarnings = FALSE)
    dir.create(paste0(output_directory, "/", plateID, "/data/rda"), showWarnings = FALSE)
    dir.create(paste0(output_directory, "/", plateID, "/data/skyline"), showWarnings = FALSE)
    dir.create(paste0(output_directory, "/", plateID, "/data/raw_data"), showWarnings = FALSE)
    dir.create(paste0(output_directory, "/", plateID, "/data/batch_correction"), showWarnings = FALSE)
    dir.create(paste0(output_directory, "/", plateID, "/html_report"), showWarnings = FALSE)
  }


#' msConvertR_set_working_directory
#'
#' This function sets the working directory to the project directory.
#'
#' @param output_directory Directory path for the project folder.
#' @return None. The function sets the working directory.
#' @examples
#' \dontrun{
#' msConvertR_set_working_directory("path/to/output_directory")
#' }
msConvertR_set_working_directory <- function(output_directory) {
  setwd(output_directory)
}

#' msConvertR_construct_command_for_terminal
#'
#' This function constructs the command for terminal to convert files to mzML format.
#'
#' @param file_path path to vendor file to be converted.
#' @param output_directory path to output directory.
#' @return The constructed command string.
#' @examples
#' \dontrun{
#' command <- msConvertR_construct_command_for_terminal(file_path, "path/to/output_directory")
#' }
msConvertR_construct_command_for_terminal <- function(file_path, output_directory) {
  # Normalise file paths
  command_path <- gsub("/", "\\\\", file_path)

  # Detect ProteoWizard version folder
  base_path <- "C:\\Program Files\\ProteoWizard\\"
  folders <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  version_folder <- folders[grepl("^ProteoWizard", folders)]

  # Use the first match or sort to get the latest version
  selected_version <- sort(version_folder, decreasing = TRUE)[1]
  msconvert_path <- file.path(base_path, selected_version, "msconvert.exe")

  # Construct the base command
  base_command <- sprintf('"%s" --zlib --filter "titleMaker <RunId>.<ScanNumber>.<ScanNumber>.<ChargeState> File:\\"<SourcePath>\\", NativeID:\\"<Id>\\""', msconvert_path)

  # Prepare output directory
  output_dir <- gsub("/", "\\\\", output_directory) %>% paste0(.,"\\msConvert_mzml_output")

  # Quote file paths
  quoted_file_paths <- shQuote(command_path)

  # Construct full command
  full_command <- paste(
    base_command,
    paste(quoted_file_paths, collapse = " "),
    "--outdir", shQuote(output_dir)
  )

  return(full_command)
}

#' msConvertR_execute_command
#'
#' This function executes the command to convert files to mzML format.
#'
#' @param command The command string to execute.
#' @return None. The function executes the command.
#' @examples
#' \dontrun{
#' msConvertR_execute_command(command)
#' }
msConvertR_execute_command <- function(command) {
  system(command)
}

#' msConvertR_restructure_directory
#'
#' This function restructures the directory by moving raw_data and mzML files to correct locations.
#'
#' @param output_directory Output directory where the mzML files will be stored.
#' @param plateID Plate ID for the current plate.
#' @return The updated master list object with mzML file paths.
#' @examples
#' \dontrun{
#' master_list <- msConvertR_restructure_directory(output_directory, "plateID")
#' }
msConvertR_restructure_directory <- function(output_directory, plateID) {
  # Define key paths
  project_dir <- output_directory
  raw_data_dir <- file.path(output_directory, "raw_data")
  mzml_output_dir <- file.path(output_directory, "msConvert_mzml_output")
  plate_data_dir <- file.path(output_directory, plateID, "data")
  raw_data_dest <- file.path(plate_data_dir, "raw_data")
  mzml_dest <- file.path(plate_data_dir, "mzml")

  # Create destination directories if they don't exist
  dir.create(raw_data_dest, recursive = TRUE, showWarnings = FALSE)
  dir.create(mzml_dest, recursive = TRUE, showWarnings = FALSE)

  # Find raw data files matching plateID
  raw_files <- list.files(path = raw_data_dir,
                          pattern = "\\.(wiff.scan|wiff|d|raw)$",
                          full.names = TRUE)
  matched_raw_files <- str_subset(raw_files, plateID)

  # Copy individual raw files (non-directories)
  file_info <- file.info(matched_raw_files)
  raw_file_paths <- matched_raw_files[!file_info$isdir]
  file.copy(from = raw_file_paths, to = raw_data_dest, recursive = FALSE)

  # Copy entire .d directories
  d_dirs <- matched_raw_files[file_info$isdir]
  for (dir in d_dirs) {
    dest_dir <- file.path(raw_data_dest)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(from = dir, to = dest_dir, recursive = TRUE)
  }

  # Filter and copy mzML files
  mzml_files <- list.files(path = mzml_output_dir,
                           pattern = "\\.mzML$",
                           full.names = TRUE)
  mzml_files <- mzml_files[!grepl("COND|Blank|ISTDs", mzml_files)]
  matched_mzml <- str_subset(mzml_files, plateID)
  file.copy(from = matched_mzml, to = mzml_dest, recursive = FALSE)

}

