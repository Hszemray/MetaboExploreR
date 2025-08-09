#' Quality Control Check for R
#'
#' This function performs a series of quality control checks on the data within a specified project directory.
#' If you have not used the SkylineR function to generate reports please ensure your report file names contain "_skylineR_1_" to ensure the function can correctly identify the files in your project directory.
#'
#' @param project_directory A character string specifying the path to the project directory.
#' @param mrm_template_list A list of MRM templates and associated concentration guide. Must have specific column names. See examples for structure of mrm_template_list. Must include mrm_guide labelled as "SIL_guide" and associated concentration guide labelled as "conc_guide". Can contain multiple combinations stored as separate lists, see examples.
#' @param QC_sample_label A character string containing the key tags to filter QC samples from file names.  E.g. "qc".
#' @param sample_tags A character vector specifying the tags to filter sample types from file names. E.g. c("sample","control","blank", "qc").
#' @param user_name A character string specifying the name of the user running the QC checks.
#' @param mv_threshold A numeric value  between 0 and 100 specifying the threshold for missing values in the data. Default is 0.5 (50%).
#'
#' @return A list containing the processed data and generated reports.
#' @examples
#' \dontrun{
#'
#' library(MetaboExploreR)
#'
#' #Load example mrm_template_list
#'   file_path <- system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MetaboExploreR")
#'   sample_metadata_example <- read_tsv(file_path)
#'
#' #Load example conc_guide
#'   file_path <- system.file("extdata", "LGW_SIL_batch_Ultimate_2023_03_06.tsv", package = "MetaboExploreR")
#'   sample_metadata_example <- read_tsv(file_path)
#'
#' #Load example report file
#'   file_path <- system.file("extdata", "Example_xskylineR_report.csv", package = "MetaboExploreR")
#'   report_file <- read.csv(file_path)
#'
#' #Run qcCheckR function
#' qcCheckR(user_name = "user1",
#'          project_directory = "path/to/project_directory",
#'          mrm_template_list = list(v1 = list(
#'                                     SIL_guide = path to/mrm_guide1.tsv,
#'                                     conc_guide = path to/SIL_concentration_guide1.tsv),
#'                                   v2 = list(
#'                                     SIL_guide = path to/mrm_guide2.tsv,
#'                                     conc_guide = path to/SIL_concentration_guide2.tsv)
#'                                  ),
#'          QC_sample_label = "qc",
#'          sample_tags = c("sample","control","blank", "qc"),
#'          mv_threshold = 0.5) #default is 50% missing values
#' }
#' @export
qcCheckR <- function(user_name, project_directory, mrm_template_list = NULL,
                     QC_sample_label = "LTR", sample_tags = NULL,
                     mv_threshold = 50) {

  #validate user_name
  if(missing(user_name)){
    stop("user_name parameter is required. Please see documentation for details.")
  }else{
    message(paste0("Welcome ", user_name,"!"))
  }

  # validate project_directory
  validate_project_directory(project_directory)

  # validate templates
  if (user_name!= "ANPC" && missing(mrm_template_list)){
    stop("mrm_template_list parameter is required.")
  }

  # validate QC_sample_label
  if(user_name !=  "ANPC" && missing(QC_sample_label)){
    stop("QC_sample_label parameter is required.")
  }

  # validate sample_tags
  if(user_name != "ANPC" && missing(sample_tags)){
    stop("sample_tags parameter is required.")
  }

  # validate mv_threshold
  if (!is.numeric(mv_threshold) || mv_threshold < 0 || mv_threshold > 100) {
    stop("mv_threshold must be a numeric value between 0 and 100.")
  }

  # process data
    #tryCatch({
      ##project setup
      master_list <- qcCheckR_setup_project(user_name, project_directory, mrm_template_list, QC_sample_label, sample_tags, mv_threshold)
      ##data preparation
      master_list <- qcCheckR_transpose_data(master_list)
      master_list <- qcCheckR_sort_data(master_list)
      master_list <- qcCheckR_impute_data(master_list)
      master_list <- qcCheckR_calculate_response_concentration(master_list)
      master_list <- qcCheckR_statTarget_batch_correction(master_list)
      #filtering
      master_list <- qcCheckR_set_qc(master_list)
      master_list <- qcCheckR_sample_filter(master_list)
      master_list <- qcCheckR_sil_IntStd_filter(master_list)
      master_list <- qcCheckR_lipid_filter(master_list)
      master_list <- qcCheckR_RSD_filter(master_list)
      #summary report
      master_list <- qcCheckR_summary_report(master_list)
      #plot generation
      master_list <- qcCheckR_plot_options(master_list)
      master_list <- qcCheckR_PCA(master_list)
      master_list <- qcCheckR_run_order_plots(master_list)
      master_list <- qcCheckR_target_control_charts(master_list)
      #exports
      master_list <- qcCheckR_export_all(master_list)
    # }, error = function(e) {
    #   message(paste("Error during project QC",  ":", e$message))
    #   log_error(paste("Error during project QC", ":", e$message))
    # })
  }#close of function
