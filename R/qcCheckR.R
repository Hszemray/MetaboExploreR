#' Quality Control Check for R
#'
#' This function performs a series of quality control checks on the data within a specified project directory.
#'
#' @param project_directory A character string specifying the path to the project directory.
#' @param mrm_template_list A list of MRM templates and associated concentration guide. Must have specific column names. See examples for structure of mrm_template_list. Must include mrm_guide labelled as "SIL_guide" and associated concentration guide labelled as "conc_guide".
#' @return A list containing the processed data and generated reports.
#' @examples
#' #Example of mrm_template_list structure for multiple methods in a single project
#' str(mrm_template_list)
#' List of 2
#' $ v1:List of 2
#' ..$ SIL_guide : spc_tbl_ [1,231 × 10] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#' .. ..$ molecule_list_name            : chr [1:1231] "CE" "CE" "CE" "CE" ...
#' .. ..$ precursor_name                : chr [1:1231] "CE(14:0)" "CE(16:0)" "CE(16:1)" "CE(18:0)" ...
#' .. ..$ precursor_mz                  : num [1:1231] 615 643 641 671 669 ...
#' .. ..$ precursor_charge              : num [1:1231] 1 1 1 1 1 1 1 1 1 1 ...
#' .. ..$ product_mz                    : num [1:1231] 369 369 369 369 369 ...
#' .. ..$ product_charge                : num [1:1231] 1 1 1 1 1 1 1 1 1 1 ...
#' .. ..$ explicit_retention_time       : num [1:1231] 11.6 12.3 11.6 12.8 12.3 ...
#' .. ..$ explicit_retention_time_window: num [1:1231] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#' .. ..$ note                          : chr [1:1231] "SIL_CE(16:0)_d7_Lipidyzer" "SIL_CE(16:0)_d7_Lipidyzer" "SIL_CE(16:1)_d7_Lipidyzer" "SIL_CE(18:1)_d7_Lipidyzer" ...
#' .. ..$ control_chart                 : logi [1:1231] FALSE TRUE TRUE FALSE TRUE TRUE ...
#' .. ..- attr(*, "spec")=
#'   .. .. .. cols(
#'     .. .. ..   `Molecule List Name` = col_character(),
#'     .. .. ..   `Precursor Name` = col_character(),
#'     .. .. ..   `Precursor Mz` = col_double(),
#'     .. .. ..   `Precursor Charge` = col_double(),
#'     .. .. ..   `Product Mz` = col_double(),
#'     .. .. ..   `Product Charge` = col_double(),
#'     .. .. ..   `Explicit Retention Time` = col_double(),
#'     .. .. ..   `Explicit Retention Time Window` = col_double(),
#'     .. .. ..   Note = col_character(),
#'     .. .. ..   control_chart = col_logical()
#'     .. .. .. )
#' .. ..- attr(*, "problems")=<externalptr>
#'   ..$ conc_guide: spc_tbl_ [71 × 9] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#' .. ..$ sil_source                       : chr [1:71] "Lipidyzer" "Lipidyzer" "Lipidyzer" "Lipidyzer" ...
#' .. ..$ istd                             : chr [1:71] "dCE(16:0)" "dCE(16:1)" "dCE(18:1)" "dCE(18:2)" ...
#' .. ..$ mw                               : num [1:71] 632 630 658 656 682 ...
#' .. ..$ mg_m_l                           : num [1:71] 0.14 0.14 0.51 1.47 0.16 0.18 0.18 0.22 0.054 0.249 ...
#' .. ..$ u_m                              : num [1:71] 222 222 776 2242 235 ...
#' .. ..$ in_solution_x200_diluted         : num [1:71] 1.11 1.11 3.88 11.21 1.17 ...
#' .. ..$ for_plasma_dilution_correction_x9: num [1:71] 9.99 9.99 34.92 100.89 10.53 ...
#' .. ..$ concentration_factor             : num [1:71] 9.99 9.99 34.92 100.89 10.53 ...
#' .. ..$ sil_name                         : chr [1:71] "SIL_CE(16:0)_d7_Lipidyzer" "SIL_CE(16:1)_d7_Lipidyzer" "SIL_CE(18:1)_d7_Lipidyzer" "SIL_CE(18:2)_d7_Lipidyzer" ...
#' .. ..- attr(*, "spec")=
#'   .. .. .. cols(
#'     .. .. ..   SIL_source = col_character(),
#'     .. .. ..   ISTD = col_character(),
#'     .. .. ..   MW = col_double(),
#'     .. .. ..   `mg/mL` = col_double(),
#'     .. .. ..   uM = col_double(),
#'     .. .. ..   `In solution x200 diluted` = col_double(),
#'     .. .. ..   `For plasma dilution correction x9` = col_double(),
#'     .. .. ..   concentration_factor = col_double(),
#'     .. .. ..   SIL_name = col_character()
#'    .. .. .. )
#' .. ..- attr(*, "problems")=<externalptr>
#'   $ v2:List of 2
#' ..$ SIL_guide : spc_tbl_ [1,243 × 11] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#' .. ..$ molecule_list_name            : chr [1:1243] "CE" "CE" "CE" "CE" ...
#' .. ..$ precursor_name                : chr [1:1243] "CE(14:0)" "CE(16:0)" "CE(16:1)" "CE(18:0)" ...
#' .. ..$ precursor_mz                  : num [1:1243] 615 643 641 671 669 ...
#' .. ..$ precursor_charge              : num [1:1243] 1 1 1 1 1 1 1 1 1 1 ...
#' .. ..$ product_mz                    : num [1:1243] 369 369 369 369 369 ...
#' .. ..$ product_charge                : num [1:1243] 1 1 1 1 1 1 1 1 1 1 ...
#' .. ..$ explicit_retention_time       : num [1:1243] 11.6 12.3 11.6 12.8 12.3 ...
#' .. ..$ explicit_retention_time_window: num [1:1243] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#' .. ..$ note                          : chr [1:1243] "SIL_20:3 cholesteryl-d7 ester" "SIL_20:3 cholesteryl-d7 ester" "SIL_20:3 cholesteryl-d7 ester" "SIL_20:3 cholesteryl-d7 ester" ...
#' .. ..$ source                        : chr [1:1243] NA NA NA NA ...
#' .. ..$ control_chart                 : logi [1:1243] FALSE FALSE FALSE FALSE FALSE FALSE ...
#' .. ..- attr(*, "spec")=
#'   .. .. .. cols(
#'     .. .. ..   `Molecule List Name` = col_character(),
#'     .. .. ..   `Precursor Name` = col_character(),
#'     .. .. ..   `Precursor Mz` = col_double(),
#'     .. .. ..   `Precursor Charge` = col_double(),
#'     .. .. ..   `Product Mz` = col_double(),
#'     .. .. ..   `Product Charge` = col_double(),
#'     .. .. ..   `Explicit Retention Time` = col_double(),
#'     .. .. ..   `Explicit Retention Time Window` = col_double(),
#'     .. .. ..   Note = col_character(),
#'     .. .. ..   Source = col_character(),
#'     .. .. ..   control_chart = col_logical()
#'     .. .. .. )
#' .. ..- attr(*, "problems")=<externalptr>
#'   ..$ conc_guide: spc_tbl_ [80 × 9] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#' .. ..$ sil_source                       : chr [1:80] "UltimateSPLASH" "UltimateSPLASH" "UltimateSPLASH" "UltimateSPLASH" ...
#' .. ..$ istd                             : chr [1:80] "SIL_14:1 cholesteryl-d7 ester" "SIL_16:1 cholesteryl-d7 ester" "SIL_18:1 cholesteryl-d7 ester" "SIL_20:3 cholesteryl-d7 ester" ...
#' .. ..$ mw                               : num [1:80] 602 630 658 682 708 ...
#' .. ..$ ug_m_l                           : num [1:80] 25 50 75 50 25 5.31 75 50 25 50 ...
#' .. ..$ u_m                              : num [1:80] 41.6 79.4 114 73.3 35.3 ...
#' .. ..$ in_solution_x500_diluted         : num [1:80] 0.083 0.159 0.228 0.147 0.071 0.02 0.276 0.175 0.084 0.16 ...
#' .. ..$ for_plasma_dilution_correction_x9: num [1:80] 0.75 1.43 2.05 1.32 0.64 0.18 2.49 1.58 0.75 1.44 ...
#' .. ..$ concentration_factor             : num [1:80] 0.75 1.43 2.05 1.32 0.64 0.18 2.49 1.58 0.75 1.44 ...
#' .. ..$ sil_name                         : chr [1:80] "SIL_14:1 cholesteryl-d7 ester" "SIL_16:1 cholesteryl-d7 ester" "SIL_18:1 cholesteryl-d7 ester" "SIL_20:3 cholesteryl-d7 ester" ...
#' .. ..- attr(*, "spec")=
#'   .. .. .. cols(
#'     .. .. ..   SIL_source = col_character(),
#'     .. .. ..   ISTD = col_character(),
#'     .. .. ..   MW = col_double(),
#'     .. .. ..   `ug/mL` = col_double(),
#'     .. .. ..   uM = col_double(),
#'     .. .. ..   `In solution x500 diluted` = col_double(),
#'     .. .. ..   `For plasma dilution correction x9` = col_double(),
#'     .. .. ..   concentration_factor = col_double(),
#'     .. .. ..   SIL_name = col_character()
#'     .. .. .. )
#' .. ..- attr(*, "problems")=<externalptr>
#'
#' #Load example mrm_guide
#' file_path <- system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MetaboExploreR")
#'   sample_metadata_example <- read_tsv(file_path)
#'
#' #Load example conc_guide
#' file_path <- system.file("extdata", "LGW_SIL_batch_Ultimate_2023_03_06.tsv", package = "MetaboExploreR")
#'   sample_metadata_example <- read_tsv(file_path)
#'
#' #Example of sample_metadata
#' str(sample_metadata_example)
#' spc_tbl_ [274 × 8] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#'  $ sample_run_index  : num [1:274] 1 2 3 4 5 6 7 8 9 10 ...
#'  $ sample_name       : chr [1:274] "xx1" "xx2" "xx3" "xx4" ...
#'  $ sample_timestamp  : POSIXct[1:274], format: "2024-09-25 21:15:33" "2024-09-25 21:31:08" "2024-09-25 21:46:43" "2024-09-25 22:02:19" ...
#'  $ sample_batch      : chr [1:274] "Jane_Doe" "Jane_Doe" "Jane_Doe" "Jane_Doe" ... # cohort name
#'  $ sample_plate_id   : chr [1:274] "p001" "p001" "p001" "p001" ...
#'  $ sample_plate_order: num [1:274] 1 2 3 4 5 6 7 8 9 10 ...
#'  $ sample_type       : chr [1:274] "sample" "sample" "sample" "qc" ... #In this column label your desired quality control as "qc", the rest as "sample".
#'  $ sample_type_factor: chr [1:274] "vltr" "sample" "sample" "pqc" ... #label samples as desired for plotting
#'  - attr(*, "spec")=
#'   .. cols(
#'   ..   sample_run_index = col_double(),
#'   ..   sample_name = col_character(),
#'   ..   sample_timestamp = col_datetime(format = ""),
#'   ..   sample_batch = col_character(),
#'   ..   sample_plate_id = col_character(),
#'   ..   sample_plate_order = col_double(),
#'   ..   sample_type = col_character(),
#'   ..   sample_type_factor = col_character()
#'   .. )
#'  - attr(*, "problems")=<externalptr>
#'
#' #Load sample_metadata_example
#'   file_path <- system.file("extdata", "sample_metadata_example.tsv", package = "MetaboExploreR")
#'   sample_metadata_example <- read_tsv(file_path)
#'
#' #Run qcCheckR function
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
  }#close of function
