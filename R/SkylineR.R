# SkylineR.R
SkylineR <- function(project_directory) {

  #set wiff file paths from project file wiff
  wiff_file_paths <- list.files(path = paste0(project_directory,"/wiff"), pattern = ".wiff$", all.files = FALSE,
                                full.names = TRUE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #set plateIDs
  plateIDs <- str_remove(str_extract(wiff_file_paths, "[^/]+$"), "\\.wiff$")

  #Source externals
  source("R/Utils_Global.R")
  source("R/Utils_SkylineR.R")
  source("R/config.R")

  # Process each plate
  for (plateID in plateIDs) {
    master_list <- setup_project(project_directory, plateID)
    master_list <- mzml_conversion(plateID, master_list)
    master_list <- import_mzml(plateID, master_list)
    master_list <- peak_picking(plateID, master_list)
  }

  # Final cleanup and archiving
  archive_raw_files(project_directory)
  message("Chromatograms and reports are now available per plate in your specified directory.\n
  Please Run qcCheckR to calculate concentrations and QC data")
}



