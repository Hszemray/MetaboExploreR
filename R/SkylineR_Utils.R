#' Utils_Global.R

#' Import specific functions from packages
#' @name import_external_functions
#' @importFrom utils installed.packages sessionInfo browseURL capture.output install.packages data str read.csv
#' @importFrom readr read_tsv write_csv read_tsv write_tsv
#' @importFrom dplyr slice arrange_at mutate_all mutate bind_rows bind_cols filter select rename arrange contains intersect pull left_join right_join any_of mutate_at all_of across distinct rowwise c_across ungroup
#' @importFrom purrr map
#' @importFrom plotly ggplotly layout
#' @importFrom ggplot2 ggplot aes geom_vline geom_hline geom_point theme_bw scale_shape_manual scale_color_manual scale_size_manual guides guide_legend facet_wrap scale_fill_manual ylab geom_text
#' @importFrom tibble tibble add_column as_tibble is_tibble column_to_rownames rownames_to_column
#' @importFrom tidyr replace_na pivot_wider
#' @importFrom stringr str_remove str_extract str_sub str_subset str_detect
#' @importFrom mzR openMSfile chromatogramHeader chromatograms
#' @importFrom janitor clean_names
#' @importFrom magrittr %>%
#' @importFrom stats median setNames na.omit
#' @importFrom data.table :=
#' @importFrom tidyselect where
#' @importFrom stats sd
NULL

#SkylineR internal functions----
#.----

#Move Folder Functions----

###Primary Function----
#' move_folder
#' This function moves a folder from the source directory to the destination directory, waits until files are not in use, and then deletes the source directory.
#' @param source_dir Directory path for the folder to copy.
#' @param dest_dir Directory path for the folder to be copied to.
#' @param max_wait Maximum wait time (in seconds) for the system prior to deleting the moved folder.
#' @param max_retries Maximum number of attempts to try move/delete prior to error. Default 30
#' @return None. The function performs the move operation and prints a message upon successful completion.
#' @examples
#' \dontrun{
#' move_folder(source_dir = "path/to/source", dest_dir = "path/to/destination", max_wait = 60, max_retries = 30)
#' }
move_folder <- function(source_dir, dest_dir, max_wait = 60, max_retries = 30) {
  validate_directories(source_dir, dest_dir)
  files_to_copy <- copy_files(source_dir, dest_dir)
  wait_until_files_free(files_to_copy, max_wait, max_retries)
  delete_source_directory(source_dir)
  return(TRUE)
}

###Sub Functions----

#' validate_directories
#' This function validates the existence of the source directory and creates the destination directory if it does not exist.
#' @param source_dir Directory path for the folder to copy.
#' @param dest_dir Directory path for the folder to be copied to.
#' @return None. The function performs directory validation.
#' @examples
#' \dontrun{
#' validate_directories(source_dir = "path/to/source", dest_dir = "path/to/destination")
#' }
validate_directories <- function(source_dir, dest_dir) {
  if (!dir.exists(source_dir)) {
    stop(paste("Source directory does not exist:", source_dir))
  }
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
}

#' copy_files
#' This function copies files from the source directory to the destination directory.
#' @param source_dir Directory path for the folder to copy.
#' @param dest_dir Directory path for the folder to be copied to.
#' @return A vector of file paths that were copied.
#' @examples
#' \dontrun{
#' copy_files(source_dir = "path/to/source", dest_dir = "path/to/destination")
#' }
copy_files <- function(source_dir, dest_dir) {
  files_to_copy <- list.files(source_dir, full.names = TRUE)
  success <- file.copy(files_to_copy, dest_dir, recursive = TRUE)
  if (!all(success)) {
    stop("Some files failed to copy.")
  }
  return(files_to_copy)
}

#' wait_until_files_free
#' This function waits until files are not in use by attempting to rename them.
#' @param files_to_copy A vector of file paths to check.
#' @param max_wait Maximum wait time (in seconds) for the system prior to deleting the moved folder. Default is 60
#' @param max_retries Maximum number of attempts to try move/delete prior to error. Default is 30.
#' @return None. The function waits until files are free.
#' @examples
#' \dontrun{
#' wait_until_files_free(files_to_copy, max_wait = 60, max_retries = 30)
#' }
wait_until_files_free <- function(files_to_copy, max_wait = 60, max_retries = 30) {
  for (test_file in files_to_copy) {
    retries <- 0
    start_time <- Sys.time()
    while (TRUE) {
      if (!file.exists(test_file)) break

      test_rename <- try(file.rename(test_file, paste0(test_file, ".tmp")), silent = TRUE)
      if (!inherits(test_rename, "try-error") && test_rename) {
        file.rename(paste0(test_file, ".tmp"), test_file)
        break
      }

      if (as.numeric(Sys.time() - start_time, units = "secs") > max_wait || retries >= max_retries) {
        stop(paste("File still in use after waiting:", test_file))
      }

      Sys.sleep(2)
      retries <- retries + 1
    }
  }
}

#' delete_source_directory
#' This function deletes the source directory after files have been moved.
#' @param source_dir Directory path for the folder to delete.
#' @return None. The function deletes the source directory.
#' @examples
#' \dontrun{
#' delete_source_directory(source_dir = "path/to/source")
#' }
delete_source_directory <- function(source_dir) {
  unlink(source_dir, recursive = TRUE, force = TRUE)
  if (dir.exists(source_dir)) {
    stop(paste("Failed to delete directory:", source_dir))
  } else {
    message(paste("Successfully moved and deleted:", source_dir))
  }
}

#.----
#Setup Project Functions----

###Primary Function----
#' skyline_setup_project
#'
#' This function sets up the project by initialising the master list, setting up project directories, and updating the script log.
#'
#' @param project_directory Directory path for the project folder containing the wiff folder and wiff files.
#' @param plateID Plate ID for the current plate.
#' @param mrm_template_list List of MRM guides.
#' @param QC_sample_label Key for filtering QC samples from sample list.
#' @return The updated `master_list` object with the project setup details.
#' @examples
#' \dontrun{
#' skyline_setup_project("path/to/project_directory", "plateID", mrm_template_list, "QC_sample_label")
#' }
skyline_setup_project <- function(project_directory, plateID, mrm_template_list, QC_sample_label) {
  master_list <- initialise_master_list()
  master_list <- store_environment_details(master_list)
  master_list <- set_project_details(master_list, project_directory, plateID, QC_sample_label)
  master_list <- read_mrm_guides(master_list, mrm_template_list)
  setup_project_directories(master_list)
  master_list <- update_script_log(master_list, "project_setup", "start_time", "ms_convert")
  return(master_list)
}

###Sub Functions----
#' initialise_master_list
#'
#' This function initialises the master list with default values.
#'
#' @return A list representing the initialised master list.
#' @examples
#' \dontrun{
#' master_list <- initialise_master_list()
#' }
initialise_master_list <- function() {
  master_list <- list()
  master_list$environment <- list()
  master_list$environment$user_functions <- list()
  master_list$templates <- list()
  master_list$templates$mrm_guides <- list()
  master_list$project_details <- list()
  master_list$data <- list()
  master_list$summary_tables <- list()
  master_list$process_lists <- list()
  return(master_list)
}

#' store_environment_details
#'
#' This function stores environment details in the master list.
#'
#' @param master_list The master list object.
#' @return The updated master list object with environment details.
#' @examples
#' \dontrun{
#' master_list <- store_environment_details(master_list)
#' }
store_environment_details <- function(master_list) {
  master_list$environment$r_version <- sessionInfo()$R.version$version.string
  master_list$environment$base_packages <- sessionInfo()$basePkgs
  master_list$environment$user_packages <- paste0(names(sessionInfo()$otherPkgs), ": ", paste0(installed.packages()[names(sessionInfo()$otherPkgs), "Version"]))
  return(master_list)
}

#' set_project_details
#'
#' This function sets project details in the master list.
#'
#' @param master_list The master list object.
#' @param project_directory Directory path for the project folder.
#' @param plateID Plate ID for the current plate.
#' @param QC_sample_label Key for filtering QC samples from sample list.
#' @return The updated master list object with project details.
#' @examples
#' \dontrun{
#' master_list <- set_project_details(master_list, "path/to/project_directory", "plateID", "QC_sample_label")
#' }
set_project_details <- function(master_list, project_directory, plateID, QC_sample_label) {
  master_list$project_details$project_dir <- project_directory
  master_list$project_details$lipidExploreR_version <- "Automated"
  master_list$project_details$user_name <- "Australian National Phenome Centre"
  master_list$project_details$project_name <- str_extract(master_list$project_details$project_dir, "[^/]*$")
  master_list$project_details$all_file_paths <- list.files(path = paste0(master_list$project_details$project_dir, "/raw_data"),
                                                       pattern = paste0(plateID, "\\.(wiff|d|raw)$"),
                                                       all.files = FALSE, full.names = TRUE, recursive = FALSE,
                                                       ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
  master_list$project_details$plateID <- plateID
  master_list$project_details$qc_type <- QC_sample_label
  master_list$project_details$script_log$timestamps$start_time <- Sys.time()
  return(master_list)
}

#' read_mrm_guides
#'
#' This function reads MRM guides from user-supplied paths in the mrm_template_list.
#'
#' @param master_list The master list object.
#' @param mrm_template_list List of MRM guide file paths.
#' @return The updated master list object with MRM guides.
#' @examples
#' \dontrun{
#' master_list <- read_mrm_guides(master_list, mrm_template_list)
#' }
read_mrm_guides <- function(master_list, mrm_template_list) {
  names(mrm_template_list) <- sapply(mrm_template_list, function(path) {
    basename(path)
  })
  for (version in names(mrm_template_list)) {
    master_list$templates$mrm_guides[[version]]$mrm_guide <- read_tsv(mrm_template_list[[version]])
  }
  return(master_list)
}

#' setup_project_directories
#'
#' This function sets up project directories for each plate ID.
#'
#' @param master_list The master list object.
#' @return None. The function sets up directories.
#' @examples
#' \dontrun{
#' setup_project_directories(master_list)
#' }
setup_project_directories <- function(master_list) {
  for (plate_ID in master_list$project_details$plateID) {
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID), showWarnings = FALSE)
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID, "/data"), showWarnings = FALSE)
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID, "/data/mzml"), showWarnings = FALSE)
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID, "/data/rda"), showWarnings = FALSE)
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID, "/data/skyline"), showWarnings = FALSE)
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID, "/data/raw_data"), showWarnings = FALSE)
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID, "/data/batch_correction"), showWarnings = FALSE)
    dir.create(paste0(master_list$project_details$project_dir, "/", plate_ID, "/html_report"), showWarnings = FALSE)
  }
}

#.----
#mzML File Conversion Functions ----

###Primary Function----
#' mzml_conversion
#'
#' This function converts raw files to mzML format using ProteoWizard's msconvert tool, restructures directories, and updates the script log.
#'
#' @param plateID Plate ID for the current plate.
#' @param master_list Master list generated internally.
#' @return The updated `master_list` object with the mzML conversion details.
#' @examples
#' \dontrun{
#' mzml_conversion("plateID", master_list)
#' }
mzml_conversion <- function(plateID, master_list) {
  validate_master_list_project_directory(master_list)
  set_working_directory(master_list$project_details$project_dir)
  command <- construct_command_for_terminal(master_list$project_details$all_file_paths, master_list$project_details$project_dir)
  execute_command(command)
  master_list <- restructure_directory(master_list, plateID)
  master_list <- update_script_log(master_list, "ms_convert", "project_setup", "mzR_mzml_import")
  return(master_list)
}

###Sub Functions----
#' set_working_directory
#'
#' This function sets the working directory to the project directory.
#'
#' @param project_directory Directory path for the project folder.
#' @return None. The function sets the working directory.
#' @examples
#' \dontrun{
#' set_working_directory("path/to/project_directory")
#' }
set_working_directory <- function(project_directory) {
  setwd(project_directory)
}

#' construct_command_for_terminal
#'
#' This function constructs the command for terminal to convert files to mzML format.
#'
#' @param all_file_paths Vector of file paths.
#' @param project_directory Directory path for the project folder.
#' @return The constructed command string.
#' @examples
#' \dontrun{
#' command <- construct_command_for_terminal(all_file_paths, "path/to/project_directory")
#' }
construct_command_for_terminal <- function(all_file_paths, project_directory) {
  # Normalize file paths
  file_paths <- gsub("/", "\\\\", all_file_paths)

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
  output_dir <- gsub("/", "\\\\", project_directory) %>% paste0(.,"\\msConvert_mzml_output")

  # Quote file paths
  quoted_file_paths <- sapply(file_paths, shQuote)

  # Construct full command
  full_command <- paste(
    base_command,
    paste(quoted_file_paths, collapse = " "),
    "--outdir", shQuote(output_dir)
  )

  return(full_command)
}

#' execute_command
#'
#' This function executes the command to convert files to mzML format.
#'
#' @param command The command string to execute.
#' @return None. The function executes the command.
#' @examples
#' \dontrun{
#' execute_command(command)
#' }
execute_command <- function(command) {
  system(command)
}

#' restructure_directory
#'
#' This function restructures the directory by moving raw_data and mzML files to correct locations.
#'
#' @param master_list The master list object.
#' @param plateID Plate ID for the current plate.
#' @return The updated master list object with mzML file paths.
#' @examples
#' \dontrun{
#' master_list <- restructure_directory(master_list, "plateID")
#' }
restructure_directory <- function(master_list, plateID) {
  # Define key paths
  project_dir <- master_list$project_details$project_dir
  raw_data_dir <- file.path(project_dir, "raw_data")
  mzml_output_dir <- file.path(project_dir, "msConvert_mzml_output")
  plate_data_dir <- file.path(project_dir, plateID, "data")
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

  # Update master list
  master_list$project_details$mzml_file_paths <- mzml_files

  return(master_list)
}


#.----
#mzR Mrm FindR Functions-----

###Primary Function----
#' mzR_mrm_findR
#'
#' This function processes mzML files to find peak apex and boundaries using QC mzR data, updates the mrm guide, and provides peak boundary information.
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param FUNC_OPTION_qc_type QC type used in the experiment passed internally.
#' @return A list containing updated mrm guide and peak boundary information.
#' @examples
#' \dontrun{
#' mzR_mrm_findR(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
#' }
mzR_mrm_findR <- function(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type) {
  validate_mzR_parameters(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
  mzML_filelist <- get_mzML_filelist(FUNC_mzR)
  mzML_filelist_qc <- filter_mzML_filelist_qc(mzML_filelist, FUNC_OPTION_qc_type)
  FUNC_tibble <- process_files(FUNC_mzR, FUNC_mrm_guide, mzML_filelist_qc)
  FUNC_output <- create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
  return(FUNC_output)
}

###Sub Functions----

#' validate_mzR_parameters
#'
#' This function validates parameters mzR_mrmfindR. If parameters fail validation script stops
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param FUNC_OPTION_qc_type QC type used in the experiment (LTR, PQC, none) parsed internally.
#' @return NULL
#' @examples
#' \dontrun{
#' validate_mzR_parameters(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
#' }
validate_mzR_parameters <- function(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type) {
  if (!is.list(FUNC_mzR) || !is.data.frame(FUNC_mrm_guide) || !is.character(FUNC_OPTION_qc_type)) {
    stop("Invalid input parameters.")
  }
}

#' get_mzML_filelist
#'
#' This function generates the mzML_filelist from FUNC_mzR
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @return A list containing mzML file information from plate.
#' @examples
#' \dontrun{
#' get_mzML_filelist(FUNC_mzR)
#' }
get_mzML_filelist <- function(FUNC_mzR) {
  mzML_filelist <- NULL
  for(idx_plate in names(FUNC_mzR)){
    mzML_filelist <- c(mzML_filelist, names(FUNC_mzR[[idx_plate]]))
  }
  return(mzML_filelist)
}

#' filter_mzML_filelist_qc
#'
#' This function filters the mzML_filelist for user selected quality control samples
#'
#' @param mzML_filelist A list containing mzML file information from plate.
#' @param FUNC_OPTION_qc_type QC type used in the experiment parsed internally.
#' @return A list containing quality control sample data.
#' @examples
#' \dontrun{
#' filter_mzML_filelist_qc(mzML_filelist,FUNC_OPTION_qc_type)
#' }
filter_mzML_filelist_qc <- function(mzML_filelist, FUNC_OPTION_qc_type) {
  mzML_filelist_qc <- mzML_filelist[grep(FUNC_OPTION_qc_type, mzML_filelist)]
  return(mzML_filelist_qc)
}

#' process_files
#'
#' This function process each mzml to produce a tibble of mrm data for all samples on plate.
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param mzML_filelist_qc A list containing mzML file information from plate for only quality control samples.
#' @return A tibble containing mrm data for all samples on the plate.
#' @examples
#' \dontrun{
#' process_files(FUNC_mzR, FUNC_mrm_guide, mzML_filelist_qc)
#' }
process_files <- function(FUNC_mzR, FUNC_mrm_guide, mzML_filelist_qc) {
  FUNC_tibble <- list()
  for(idx_mzML in mzML_filelist_qc){
    FUNC_tibble[[idx_mzML]] <- tibble()
    for(idx_plate in names(FUNC_mzR)){
      if(length(grep(idx_mzML, names(FUNC_mzR[[idx_plate]]))) == 1){
        FUNC_tibble[[idx_mzML]] <- process_mrm_transitions(FUNC_mzR, FUNC_mrm_guide, idx_plate, idx_mzML)
      }
    }
  }
  FUNC_tibble <- bind_rows(FUNC_tibble) %>%
    filter(lipid_class != "no match") %>%
    filter(lipid_class != "multiple match")
  return(FUNC_tibble)
}


#' process_mrm_transitions
#'
#' This function processes mrm_transition for each sample on a plate.
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @return A tibble of mrm data for the specific idx_plate/idx_mzML.
#' @examples
#' \dontrun{
#' process_mrm_transitions(FUNC_mzR, FUNC_mrm_guide, idx_plate, idx_mzML)
#' }
process_mrm_transitions <- function(FUNC_mzR, FUNC_mrm_guide, idx_plate, idx_mzML) {
  FUNC_tibble <- tibble()
  for(idx_mrm in 3:nrow(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header)){
    if(nrow(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]) > 0){
      precursor_mz <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$precursorIsolationWindowTargetMZ[idx_mrm]
      product_mz <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$productIsolationWindowTargetMZ[idx_mrm]
      baseline_value <- calculate_baseline(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
      peak_apex_idx <- find_peak_apex_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
      peak_start_idx <- find_peak_start_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value)
      peak_end_idx <- find_peak_end_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value)
      mzml_rt_apex <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime[peak_apex_idx] %>% round(2)
      mzml_rt_start <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime[peak_start_idx] %>% round(2)
      mzml_rt_end <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime[peak_end_idx] %>% round(2)
      lipid_info <- find_lipid_info(FUNC_mrm_guide, precursor_mz, product_mz, mzml_rt_apex, FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
      FUNC_tibble <- FUNC_tibble %>% bind_rows(bind_cols("mzml" = idx_mzML,
                                                         "lipid_class" = lipid_info$class,
                                                         "lipid" = lipid_info$name,
                                                         "precursor_mz" = precursor_mz,
                                                         "product_mz" = product_mz,
                                                         "peak_apex" = mzml_rt_apex,
                                                         "peak_start" = mzml_rt_start,
                                                         "peak_end" = mzml_rt_end))
    }
  }
  return(FUNC_tibble)
}

#' calculate_baseline
#'
#' This function calculates baseline for a single transition.
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @return A numeric value for baseline.
#' @examples
#' \dontrun{
#' calculate_baseline(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
#' }
calculate_baseline <- function(FUNC_mzR, idx_plate, idx_mzML, idx_mrm) {
  baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2] %>% median()
  if(baseline_value < 1){
    baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2] %>% mean()
  }
  return(baseline_value)
}

#' find_peak_apex_idx
#'
#' This function finds peak apex for a single transition.
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @return A numeric value for peak apex index.
#' @examples
#' \dontrun{
#' find_peak_apex_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
#' }
find_peak_apex_idx <- function(FUNC_mzR, idx_plate, idx_mzML, idx_mrm) {
  peak_apex_idx <- which.max(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])
  if(peak_apex_idx < 5 || peak_apex_idx > (length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])-5)){
    peak_apex_idx <- which.max((FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])[5:(length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])-5)]) + 4
  }
  return(peak_apex_idx)
}

#' find_peak_start_idx
#'
#' This function finds peak start for a single transition.
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @param peak_apex_idx Index of peak apex passed from process_files function.
#' @param baseline_value Numeric value indicating transitions baseline. Passed from process_files function.
#' @return A numeric value for peak start index.
#' @examples
#' \dontrun{
#' find_peak_start_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value)
#' }
find_peak_start_idx <- function(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value) {
  baseline_idx <- which((FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]) < baseline_value)
  peak_start_idx <- baseline_idx[which(baseline_idx < peak_apex_idx)][(length(which(baseline_idx < peak_apex_idx)))-3]
  if(length(peak_start_idx) == 0 || peak_start_idx < 1){
    peak_start_idx <- 1
  }
  return(peak_start_idx)
}

#' find_peak_end_idx
#'
#' This function finds peak end for a single transition.
#'
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @param peak_apex_idx Index of peak apex passed from process_files function.
#' @param baseline_value Numeric value indicating transitions baseline. Passed from process_files function.
#' @return A numeric value for peak end index.
#' @examples
#' \dontrun{
#' find_peak_end_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value)
#' }
find_peak_end_idx <- function(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value) {
  baseline_idx <- which((FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]) < baseline_value)
  peak_end_idx <- baseline_idx[which(baseline_idx > peak_apex_idx)][3]
  if(length(peak_end_idx) == 0 || peak_end_idx > length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]) || is.na(peak_end_idx)){
    peak_end_idx <- length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])
  }
  return(peak_end_idx)
}


#' find_lipid_info
#'
#' This function finds and matches lipids to mrm data.
#'
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param precursor_mz A numeric value for precursor mass to charge ratio.
#' @param product_mz A numeric value for product mass to charge ratio.
#' @param mzml_rt_apex A numeric value for retention time apex.
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @return A list contain lipid class and lipid species information.
#' @examples
#' \dontrun{
#' find_lipid_info(FUNC_mrm_guide, precursor_mz, product_mz, mzml_rt_apex, FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
#' }
find_lipid_info <- function(FUNC_mrm_guide, precursor_mz, product_mz, mzml_rt_apex, FUNC_mzR, idx_plate, idx_mzML, idx_mrm) {
  lipid_idx <- which(FUNC_mrm_guide$precursor_mz == precursor_mz & FUNC_mrm_guide$product_mz == product_mz)
  if(length(lipid_idx) != 1){
    lipid_idx <- which(FUNC_mrm_guide$precursor_mz > (precursor_mz - 0.25) &
                         FUNC_mrm_guide$precursor_mz < (precursor_mz + 0.25) &
                         FUNC_mrm_guide$product_mz > (product_mz - 0.25) &
                         FUNC_mrm_guide$product_mz < (product_mz + 0.25) &
                         FUNC_mrm_guide$explicit_retention_time > (min(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime)-0.1) &
                         FUNC_mrm_guide$explicit_retention_time < (max(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime)+0.1))
  }
  if(length(lipid_idx) > 1){
    lipid_class <- "multiple match"
    lipid <- "multiple match"
  } else if(length(lipid_idx) == 0){
    lipid_class <- "no match"
    lipid <- "no match"
  } else {
    lipid_class <- FUNC_mrm_guide$molecule_list_name[lipid_idx]
    lipid <- FUNC_mrm_guide$precursor_name[lipid_idx]
  }
  return(list(class = lipid_class, name = lipid))
}


#' create_output
#'
#' This function updated mrm_guide and peak_boundary_update for later use in Skyline.
#'
#' @param FUNC_tibble A tibble of mrm details per sample per lipid species.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param mzML_filelist A list containing mzML file information from plate.
#' @return A list containing two tibbles for mrm_guide_updated and peak_boundary_update.
#' @examples
#' \dontrun{
#' create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
#' }
create_output <- function(FUNC_tibble, FUNC_mrm_guide, mzML_filelist) {
  FUNC_output <- list()
  FUNC_output$mrm_guide_updated <- tibble()
  FUNC_output$peak_boundary_update <- tibble()
  for(idx_lipid in unique(FUNC_tibble$lipid)){
    FUNC_output$mrm_guide_updated <- bind_rows(FUNC_output$mrm_guide_updated,
                                               bind_cols("Molecule List Name" = (FUNC_tibble %>% filter(lipid == idx_lipid))[["lipid_class"]] %>% unique(),
                                                         "Precursor Name" = idx_lipid,
                                                         "Precursor Mz" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["precursor_mz"]],
                                                         "Precursor Charge" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["precursor_charge"]],
                                                         "Product Mz" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["product_mz"]],
                                                         "Product Charge" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["product_charge"]],
                                                         "Explicit Retention Time" = (FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_apex"]] %>% median(),
                                                         "Explicit Retention Time Window" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["explicit_retention_time_window"]],
                                                         "Note" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["note"]]))

    FUNC_output$peak_boundary_update <- bind_rows(FUNC_output$peak_boundary_update,
                                                  bind_cols("FileName" = mzML_filelist,
                                                            "FullPeptideName" = rep(idx_lipid, length(mzML_filelist)),
                                                            "MinStartTime" = rep(((FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_start"]] %>% summary())[["1st Qu."]], length(mzML_filelist)),
                                                            "MaxEndTime" = rep(((FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_end"]] %>% summary())[["3rd Qu."]], length(mzML_filelist))))
  }
  FUNC_output$mrm_guide_updated <- FUNC_output$mrm_guide_updated %>% dplyr::arrange(`Precursor Name`)
  FUNC_output$peak_boundary_update <- FUNC_output$peak_boundary_update %>% arrange(`FullPeptideName`)
  return(FUNC_output)
}


#.----
#Import mzML Files Functions ----

###Primary Function----
#' import_mzml
#'
#' This function imports mzML files for each plate using the mzR package, extracts relevant information, and updates the script log.
#'
#' @param plateID Plate ID for the current plate.
#' @param master_list Master list generated internally.
#' @return The updated `master_list` object with the mzML import details.
#' @examples
#' \dontrun{
#' import_mzml("plateID", master_list)
#' }
import_mzml <- function(plateID, master_list) {
  validate_master_list_project_directory(master_list)
  mzml_filelist <- initialise_mzml_filelist(master_list)
  master_list <- process_plates(master_list, mzml_filelist)
  master_list <- update_script_log(master_list, "mzR_mzml_import", "ms_convert", "mzml_file_processing")
  return(master_list)
}

###Sub Functions----

#' initialise_mzml_filelist
#'
#' This function initializes a list of mzML files for each plate in the master list, excluding files with specific patterns.
#'
#' @param master_list A list containing project details, including plate IDs and project directory.
#' @return A list of mzML files for each plate, excluding files with "COND", "Blank", or "ISTDs" in their names.
#' @examples
#' \dontrun{
#' initialise_mzml_filelist(master_list)
#' }
initialise_mzml_filelist <- function(master_list) {
  mzml_filelist <- list()
  for (idx_plate in master_list$project_details$plateID) {
    mzml_filelist[[idx_plate]] <- list.files(file.path(master_list$project_details$project_dir, idx_plate, "data/mzml"), pattern = ".mzML", full.names = FALSE)
    mzml_filelist[[idx_plate]] <- mzml_filelist[[idx_plate]][!grepl("COND|Blank|ISTDs", mzml_filelist[[idx_plate]])]
  }
  return(mzml_filelist)
}


#' process_plates
#'
#' This function processes mzML files for each plate in the master list, extracting relevant information and updating the master list.
#'
#' @param master_list A list containing project details and data.
#' @param mzml_filelist A list of mzML files for each plate.
#' @return The updated `master_list` object with processed mzML data.
#' @examples
#' \dontrun{
#' process_plates(master_list, mzml_filelist)
#' }
process_plates <- function(master_list, mzml_filelist) {
  for (idx_plate in master_list$project_details$plateID) {
    master_list$data[[idx_plate]]$mzR <- list()
    for (idx_mzML in mzml_filelist[[idx_plate]]) {
      master_list$data[[idx_plate]]$mzR[[idx_mzML]] <- list()
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object <- mzR::openMSfile(filename = paste0(master_list$project_details$project_dir, "/", idx_plate, "/data/mzml/", idx_mzML))
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_header <- mzR::chromatogramHeader(master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object)
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_chromatogram <- mzR::chromatograms(master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object)
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_timestamp <- master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object@backend$getRunStartTimeStamp()
    }
    master_list$data$global_timestamp[[idx_plate]] <- extract_timestamp(master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_timestamp)
    master_list$project_details$mzml_sample_list[[idx_plate]] <- update_sample_list(master_list, idx_plate)
  }
  return(master_list)
}


#' extract_timestamp
#'
#' This function extracts the year from a timestamp string and converts it to a numeric value.
#'
#' @param timestamp A string representing the timestamp.
#' @return A numeric value representing the year extracted from the timestamp.
#' @examples
#' \dontrun{
#' extract_timestamp("2025-06-17T13:31:58Z")
#' }
extract_timestamp <- function(timestamp) {
  return(timestamp %>% str_sub(., 1, 4) %>% as.numeric())
}


#' update_sample_list
#'
#' This function updates the sample list for a given plate in the master list.
#'
#' @param master_list A list containing project details and data.
#' @param idx_plate The index of the plate to update the sample list for.
#' @return The updated sample list for the specified plate.
#' @examples
#' \dontrun{
#' update_sample_list(master_list, idx_plate)
#' }
update_sample_list <- function(master_list, idx_plate) {
  if (is.null(master_list$project_details$mzml_sample_list[[idx_plate]])) {
    master_list$project_details$mzml_sample_list[[idx_plate]] <- character()
  }
  return(c(master_list$project_details$mzml_sample_list[[idx_plate]], names(master_list$data[[idx_plate]]$mzR)))
}

#.----
#Peak Picking Functions ----

###Primary Function----
#' peak_picking
#'
#' This function processes mzML files for each plate, optimizes retention times, updates peak boundaries, and checks for SIL internal standards.
#'
#' @param plateID Plate ID for the current plate.
#' @param master_list Master list generated internally.
#' @return The updated `master_list` object with peak picking details.
#' @examples
#' \dontrun{
#' peak_picking(plateID, master_list)
#' }
peak_picking <- function(plateID, master_list) {
  validate_master_list_project_directory(master_list)
  plate_idx <- master_list$project_details$plateID
  print(paste("Processing plate:", plate_idx))
  sil_found <- FALSE

  for (version in names(master_list$templates$mrm_guides)) {
    if (sil_found == TRUE ) break
    print(paste("Trying mrm_guide version:", version))
    master_list$project_details$is_ver <- version
    master_list$summary_tables$project_summary <- create_summary_table(master_list, plate_idx)
    master_list$templates$mrm_guides$by_plate[[plate_idx]] <- optimise_retention_times(master_list, plate_idx)
    export_files(master_list, plate_idx)
    execute_skyline_command(master_list, plate_idx)
    master_list$data$skyline_report[[plate_idx]] <- reimport_skyline_file(master_list, plate_idx)
    sil_found <- check_sil_standards(master_list, plate_idx, version)
    if (sil_found == TRUE) {
      save_plate_data(master_list, plate_idx)
    } else {
      print(paste("No SIL standards detected with version", version, "- trying next version"))
    }
  }

  if (sil_found == FALSE) {
    stop(print(paste("No SIL internal standards detected in plate", plate_idx, "after trying all method versions. Please ensure your mrm_guide is the transitions used in the project!")))
  }


  setwd(master_list$project_details$project_dir)
  master_list <- update_script_log(master_list, "peak_picking", "mzR_mzml_import", "next_plate_for_processing")
  return(master_list)
}

###Sub Functions----

#' create_summary_table
#'
#' This function creates a summary table for a given plate in the master list, including various project details.
#'
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to create the summary table for.
#' @return A tibble containing the summary table with project details and their values.
#' @examples
#' \dontrun{
#' create_summary_table(master_list, plate_idx)
#' }
create_summary_table <- function(master_list, plate_idx) {
  Temp_list <- master_list$project_details[c("project_dir", "lipidExploreR_version", "user_name", "project_name","qc_type", "plateID", "is_ver")]
  Temp_list$plateID <- paste(plate_idx)
  project_summary <- tibble(unlist(Temp_list)) %>%
    add_column("Project detail" = c("local directory", "lipidExploreR version", "user initials", "project name", "project QC", "plateID", "int. std. version"), .before = 1)
  project_summary <- setNames(project_summary, c("Project detail", "value"))
  return(project_summary)
}


#' optimise_retention_times
#'
#' This function optimises retention times for each plate in the master list using the mzR_mrm_findR function and updates the MRM guide.
#'
#' @param master_list A list containing project details and data.
#' @param plate_idx A vector of plate indices to optimise retention times for.
#' @return A list containing the optimised retention times and updated MRM guide for each plate.
#' @examples
#' \dontrun{
#' optimise_retention_times(master_list, plate_idx)
#' }
optimise_retention_times <- function(master_list, plate_idx) {
  by_plate <- list()
  for (idx in plate_idx) {
    result <- mzR_mrm_findR(
      FUNC_mzR = master_list$data[[idx]],
      FUNC_mrm_guide = master_list$templates$mrm_guides[[master_list$project_details$is_ver]]$mrm_guide %>% clean_names(),
      FUNC_OPTION_qc_type = master_list$project_details$qc_type
    ) %>% append(master_list$templates$mrm_guides[[master_list$project_details$is_ver]])
    by_plate[[plate_idx]] <- result
  }
  by_plate[[plate_idx]]$mrm_guide_updated <- setNames(by_plate[[plate_idx]]$mrm_guide_updated, names(by_plate[[plate_idx]]$mrm_guide))
  return(by_plate)
}


#' export_files
#'
#' This function exports various files related to the project for a given plate, including updated MRM guides, peak boundaries, and default templates.
#'
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to export files for.
#' @return Exports CSV, SKY, and TSV files to the specified project directory.
#' @examples
#' \dontrun{
#' export_files(master_list, plate_idx)
#' }
export_files <- function(master_list, plate_idx) {
  write_csv(x = master_list$templates$mrm_guides$by_plate[[plate_idx]][[plate_idx]]$mrm_guide_update,
            file = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_RT_update_",
                          master_list$project_details$project_name, "_", plate_idx, ".csv"))
  write_csv(x = master_list$templates$mrm_guides$by_plate[[plate_idx]][[plate_idx]]$peak_boundary_update,
            file = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_peak_boundary_update_",
                          master_list$project_details$project_name, "_", plate_idx, ".csv"))
  file.copy(from = system.file("templates", "default_skyline_file.sky", package = "MetaboExploreR"),
            to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline"))
  file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_skyline_file.sky"),
              to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_",master_list$project_details$project_name,"_",plate_idx,".sky"))
  file.copy(from = system.file("templates", "default_csv.csv", package = "MetaboExploreR"),
            to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline"))
  file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_csv.csv"),
              to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_xskylineR_1_",master_list$project_details$project_name,"_",plate_idx,".csv"))
  file.copy(from = system.file("templates", "default_tsv.tsv", package = "MetaboExploreR"),
            to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
  file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_tsv.tsv"),
              to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_",master_list$project_details$project_name,"_",plate_idx,"_chromatograms.tsv"))
  file.copy(from = system.file("templates", "YYYY-MM-DD_xskylineR_1_project_name.skyr", package = "MetaboExploreR"),
            to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
}


#' execute_skyline_command
#'
#' This function executes a Skyline system command to process mzML files and generate various reports for a given plate.
#'
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to execute the Skyline command for.
#' @return Executes the Skyline command and generates reports and chromatogram files saving to project directory.
#' @examples
#' \dontrun{
#' execute_skyline_command(master_list, plate_idx)
#' }
execute_skyline_command <- function(master_list, plate_idx) {
  exe_path <- shQuote("C:/Program Files/Skyline/SkylineCmd.exe")
  in_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
  import_transition_list <- shQuote(paste0("skyline/", Sys.Date(), "_RT_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
  import_peak_boundaries <- shQuote(paste0("skyline/", Sys.Date(), "_peak_boundary_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
  out_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
  report_file <- shQuote(paste0("skyline/", Sys.Date(), "_xskylineR_1_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
  chromatogram_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, "_chromatograms.tsv"))
  report_template <- shQuote("skyline/YYYY-MM-DD_xskylineR_1_project_name.skyr")
  mzml_path <- shQuote("mzml")

  setwd(paste0(master_list$project_details$project_dir,"/",plate_idx,"/data"))
  cmd <- paste(
    exe_path,
    paste0('--in=', in_file),
    paste0('--import-transition-list=', import_transition_list),
    paste0('--import-all=', mzml_path),
    paste0('--import-peak-boundaries=', import_peak_boundaries),
    paste0('--save-settings'),
    paste0('--overwrite'),
    paste0('--out=', out_file),
    paste0('--report-conflict-resolution=overwrite'),
    paste0('--report-name=YYYY-MM-DD_xskylineR_1_project_name'),
    paste0('--report-file=', report_file),
    paste0('--report-add=', report_template),
    paste0('--report-format=csv'),
    paste0('--report-invariant'),
    paste0('--chromatogram-file=', chromatogram_file),
    paste0('--chromatogram-precursors'),
    paste0('--chromatogram-products'),
    paste0('--chromatogram-base-peaks'),
    paste0('--chromatogram-tics'),
    sep = " "
  )

  system(cmd)
}


#' reimport_skyline_file
#'
#' This function reimports a Skyline file for a given plate, converts specific columns to numeric, and cleans the column names.
#'
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to reimport the Skyline file for.
#' @return A data frame containing the reimported Skyline data with cleaned column names.
#' @examples
#' \dontrun{
#' reimport_skyline_file(master_list, plate_idx)
#' }
reimport_skyline_file <- function(master_list, plate_idx) {
  skyline_data <- read.csv(
    file = list.files(
      paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline"),
      pattern = paste0("xskylineR_1_", master_list$project_details$project_name),
      full.names = TRUE))
  cols_to_convert <- c("PrecursorMz", "ProductMz", "RetentionTime", "StartTime", "EndTime", "Area", "Height")
  suppressWarnings(skyline_data[cols_to_convert] <- lapply(skyline_data[cols_to_convert], as.numeric))
  skyline_data <- janitor::clean_names(skyline_data)
  return(skyline_data)
}


#' check_sil_standards
#'
#' This function checks if the SIL standards on a given plate match the SIL standards for a specified mrm_guide version.
#'
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to check the SIL standards for.
#' @param version The version of the MRM guide to compare against.
#' @return A logical value indicating whether the SIL standards on the plate match the SIL standards for the specified version.
#' @examples
#' \dontrun{
#' check_sil_standards(master_list, plate_idx, version)
#' }
check_sil_standards <- function(master_list, plate_idx, version) {
  sil_on_plate <- master_list$data$skyline_report[[plate_idx]][grepl("SIL", master_list$data$skyline_report[[plate_idx]][["molecule_name"]], ignore.case = TRUE),] %>%
                  select(contains("molecule_name")) %>% unique() %>% rename("SIL" = "molecule_name")
  sil_for_version <- master_list$templates$mrm_guides[[version]][["mrm_guide"]][grepl("SIL", master_list$templates$mrm_guides[[version]][["mrm_guide"]][["Precursor Name"]], ignore.case = TRUE),] %>% select(contains("Precursor Name")) %>% unique() %>% rename("SIL" = "Precursor Name")
  sil_found <- ifelse(nrow(sil_on_plate) == nrow(sil_for_version), TRUE, FALSE)
  return(sil_found)
}


#' save_plate_data
#'
#' This function saves the master list data for a given plate to an RDA file.
#'
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to save the data for.
#' @return Saves the master list data to an RDA file in the specified project directory.
#' @examples
#' \dontrun{
#' save_plate_data(master_list, plate_idx)
#' }
save_plate_data <- function(master_list, plate_idx) {
  save(master_list, file = paste0(
    master_list$project_details$project_dir, "/", plate_idx, "/data/rda/",
    Sys.Date(), "_", master_list$project_details$user_name, "_", master_list$project_details$project_name, "_", plate_idx,
    "_skylineR.rda"))
}

#.----
#Archive Raw Files ----

###Primary Function----
#' Archive Raw Files
#'
#' This function moves raw files (wiff and mzML) to an archive directory after processing is complete.
#'
#' @param project_directory Path to the directory for the project parsed from SkylineR.
#' @return None. The function performs the archive operation and prints a message upon successful completion.
#' @examples
#' \dontrun{
#' archive_raw_files("path/to/project_directory")
#' }
archive_raw_files <- function(project_directory) {
  validate_project_directory(project_directory)
  archive_files(project_directory, "raw_data")
  archive_files(project_directory, "msConvert_mzml_output")
  message("\n Skyline R is now finished running all plates :)")
}

###Sub Functions----

#' Archive Files
#'
#' This function moves a specified folder to the archive directory within the project directory.
#'
#' @param project_directory The directory of the project.
#' @param folder_name The name of the folder to be archived.
#' @return Moves the specified folder to the archive directory.
#' @examples
#' \dontrun{
#' archive_files(project_directory, folder_name)
#' }
archive_files <- function(project_directory, folder_name) {
  move_folder(file.path(project_directory, folder_name), file.path(project_directory, "archive"))
}
