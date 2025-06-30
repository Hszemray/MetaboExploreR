#'SkylineR
#'
#' @description This function processes `.wiff` files in a specified project directory and converts them to `.mzml` files, followed by targeted analysis using Skyline.
#'
#' @param project_directory Path to project directory containing a folder named 'wiff' with selected .wiff and .wiff.scan files.
#' @param mrm_template_list Path to Multiple reaction monitoring (MRM) guides, must be in specified format. See examples and run load example mrm_guide for structure. May contain more than one template for multi-method projects. e.g mrm_template_list = list("path/to/mrm_guide_v1", "path/to/mrm_guide_v2")
#' @param QC_sample_label User specified tag to filter QC samples.
#' E.g. "ROCIT20_C1_URI_MS-LIPIDS_PLIP01_PLATE_3-PLASMA LTR_19.mzML"
#' QC_sample_label = "LTR" to target files containing LTR for QC.
#' @return Curated project directory containing .wiff and .mzml files, and Skyline exports.
#' @export
#' @examples
#' #mrm_guide structure
#' str(mrm_guide)
#' spc_tbl_[1,243 × 11] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#' $ Molecule List Name            : chr [1:1243] "CE" "CE" "CE" "CE" ... # Molecule Class
#' $ Precursor Name                : chr [1:1243] "CE(14:0)" "CE(16:0)" "CE(16:1)" "CE(18:0)" ... #Molecule Species
#' $ Precursor Mz                  : num [1:1243] 615 643 641 671 669 ...
#' $ Precursor Charge              : num [1:1243] 1 1 1 1 1 1 1 1 1 1 ...
#' $ Product Mz                    : num [1:1243] 369 369 369 369 369 ...
#' $ Product Charge                : num [1:1243] 1 1 1 1 1 1 1 1 1 1 ...
#' $ Explicit Retention Time       : num [1:1243] 11.6 12.3 11.6 12.8 12.3 ...
#' $ Explicit Retention Time Window: num [1:1243] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#' $ Note                          : chr [1:1243] "SIL_20:3 cholesteryl-d7 ester" "SIL_20:3 cholesteryl-d7 ester"... #Internal Standard
#' $ Source                        : chr [1:1243] NA NA NA NA ...
#' $ control_chart                 : logi [1:1243] FALSE FALSE FALSE FALSE FALSE FALSE ... # TRUE = plot for control charts in qcCheckeR
#' - attr(*, "spec")=
#'   .. cols(
#'     ..   `Molecule List Name` = col_character(),
#'     ..   `Precursor Name` = col_character(),
#'     ..   `Precursor Mz` = col_double(),
#'     ..   `Precursor Charge` = col_double(),
#'     ..   `Product Mz` = col_double(),
#'     ..   `Product Charge` = col_double(),
#'     ..   `Explicit Retention Time` = col_double(),
#'     ..   `Explicit Retention Time Window` = col_double(),
#'     ..   Note = col_character(),
#'     ..   Source = col_character(),
#'     ..   control_chart = col_logical()
#'     .. )
#' - attr(*, "problems")=<externalptr>
#'
#' #Load example mrm_guide
#'   file_path <- system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MetaboExploreR")
#'   example_mrm_template <- read_tsv(file_path)
#'
#' #Run SkylineR function
#' SkylineR(project_directory = "USER/PATH/TO/PROJECT/DIRECTORY", mrm_template_list = list("user_mrm_guide_v1.tsv", "user_mrm_guide_v2.tsv"))
#'
#' @details
#' \itemize{
#'  \item \strong{Input Validation:}
#'   \itemize{
#'    \item Validate project_directory
#'    \item Validate mrm_template_list
#'   }
#'  \item \strong{File Handling:}
#'   \itemize{
#'    \item Set wiff file paths from project file wiff
#'    \item Check if wiff files are found
#'    \item Set plateIDs
#'   }
#'  \item \strong{Processing Plates:}
#'   \itemize{
#'    \item For each plateID:
#'     \itemize{
#'      \item Setup project
#'      \item mzml conversion with MSConvert
#'      \item Import mzml
#'      \item Peak picking/intergration with Skyline MS
#'     }
#'   }
#'  \item \strong{Error Handling:}
#'   \itemize{
#'    \item Catch and log errors during plate processing
#'   }
#'  \item \strong{Final Cleanup:}
#'   \itemize{
#'    \item Archive raw files
#'    \item Message about availability of chromatograms and reports
#'   }
#' }
SkylineR <- function(project_directory, mrm_template_list, QC_sample_label) {

  # Validate project_directory
  validate_project_directory(project_directory)

  # Set working directory to project directory
  setwd(project_directory)

  # Validate mrm_template_list
  validate_mrm_template_list(mrm_template_list) # patch to check the mrm_guide has appropriate columns also need to add a check to ensure wiff has a .wiff and a .wiff.scan

  # Validate msconvert and skyline are installed in C:/Program Files/
  validate_proteowizard_skyline()

  # Validate wiff files
  wiff_file_paths <- validate_wiff_file(project_directory)

  # Check if wiff files are found
  if (length(wiff_file_paths) == 0) {
    stop("No .wiff files found in the specified project directory.")
  }

  # Set plateIDs
  plateIDs <- str_remove(str_extract(wiff_file_paths, "[^/]+$"), "\\.wiff$")


  #Set failed/successful plates
  failed_plates <- c()
  successful_plates <- c()

  # Process each plate
  for (plateID in plateIDs) {
    tryCatch({
      master_list <- skyline_setup_project(project_directory, plateID, mrm_template_list, QC_sample_label)
      master_list <- mzml_conversion(plateID, master_list)
      master_list <- import_mzml(plateID, master_list)
      master_list <- peak_picking(plateID, master_list)
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

  # Final cleanup and archiving
  archive_raw_files(project_directory)
  message("\n Chromatograms and reports are now available per plate in ",paste(project_directory),
          ".\n Please run qcCheckR to calculate concentrations and QC the data")
}



