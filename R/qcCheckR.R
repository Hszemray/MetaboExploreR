#' Quality Control Check for R
#'
#' This function performs a series of quality control checks on the data within a specified project directory.
#'
#' @param project_directory A character string specifying the path to the project directory.
#' @param mrm_template_list A list of MRM templates. Default is an empty list.
#' @return A list containing the processed data and generated reports.
#' @examples
#' mrm_template_list <- list(
#'     v1 = list(
#'        SIL_guide = "templates/LGW_lipid_mrm_template_v1.csv",
#'        conc_guide = "templates/LGW_SIL_batch_103.csv"
#'       ),
#'     v2 = list(
#'        SIL_guide = "templates/LGW_lipid_mrm_template_v2.csv",
#'        conc_guide = "templates/LGW_SIL_batch_Ultimate_2023-03-06.csv"
#'       )
#'   )
#' sample_file <-  data.frame(sampleID,sample_type)
#' qcCheckR(project_directory = "path/to/project_directory", mrm_template_list = mrm_template_list)
#' @export
qcCheckR <- function(project_directory, mrm_template_list, sample_file) {
  # validate project_directory
  validate_project_directory(project_directory)

  # Set templates
  if (missing(mrm_template_list)) {
    stop("mrm_template_list parameter is required.")
  }


  # process data
  for (plateID in plateIDs) {
    tryCatch({
      ##project setup
      master_list <- qcCheckR_setup_project(project_directory)
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
      master_list <- qcCheckR_export_xlsx_file(master_list)
      master_list <-qcCheckR_export_html_report(master_list)
      master_list <- qcCheckR_export_master_list_rda(master_list)
    }, error = function(e) {
      message(paste("Error processing plate", plateID, ":", e$message))
      log_error(paste("Error processing plate", plateID, ":", e$message))
    })
  }
}#close of function
