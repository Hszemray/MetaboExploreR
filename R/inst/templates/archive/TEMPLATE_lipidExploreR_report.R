#' ---
#' title: "Targeted Lipid ExploreR QC Report"
#' author: ANPC
#' output: html_document
#' 
#' ---
#'
#' ***
#' ### Project summary
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$summary_tables$project_summary)
#' 
#' 
#' ***
#' ### Data import summary
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$summary_tables$transposed_summary)
#'
#'
#' ***
#' ### Plot: PCA scores: Raw skyline imports (no data processing or outlier removal)
#'
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
subplot(master_list$pca_analysis$data_sorted$sample_qc$plot_scores, master_list$pca_analysis$data_sorted$plate$plot_scores)
#'
#'
#' ***
#' 
#' ### Process: Missing value filter 
#' 
#'  * Step 1: Remove all samples that have > 50% missing values 
#'  * Step 2: Remove all metabolite/lipid features that have > 50% missing values (zero, NA, NaN etc) 
#' 
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$summary_tables$missing_value_filter_summary)
#'
#' ***
#' 
#' 
#' The following internal standards failed the missing value filter and were removed from the project. Metabolite targets that use these internal standards for calculation of response ratio and concentrations are also removed.
#'
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$process_lists$missing_value_filter$failed_SIL %>% as_tibble() %>% rename(internal_standard = value))
#' 
#' ***
#' 
#' ### Plot: PCA scores: post-missing value filter
#' PCA scores plot displaying data that has undergone the following process steps: 
#' 
#'  * missing value filtering
#' 
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
subplot(master_list$pca_analysis$missing_value_filter$sample_qc$plot_scores, master_list$pca_analysis$missing_value_filter$plate$plot_scores)
#' 
#' 
#' ***
#' 
#' ### Process: Imputation of remaining zero values and missing data 
#' 
#'  Imputation is completed using x/2, where x is minimum intensity of that feature in the plate  
#' 
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$summary_tables$impute_table)
#'
#'
#' ***
#' 
#' ### Process: Calculation of response ratio and concentration value 
#'  
#'  * Step 1: Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs 
#'  * Step 2: Conversion of response ratio to concentration values using single point calibration 
#' 
#' 
#' ***
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$summary_tables$concentration_summary)
#' 
#' 
#' ***
#' 
#' ### Plot: PCA scores: post-calculation of internal standard ratio's and conversion to concentration values
#' PCA scores plot displaying data that has undergone the following process steps: 
#' 
#'  * missing value filtering 
#'  * imputation of remaining missing values 
#'  * calculation of response ratios (target analyte peak area/internal standard peak area) 
#'  * calculation of concentration values 
#' 
#' 
#' ***
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
subplot(master_list$pca_analysis$concentration$sample_qc$plot_scores, master_list$pca_analysis$concentration$plate$plot_scores)
#'
#'
#' ***
#' 
#' ### Process: Sample outlier filter
#' Filter to remove all outlier samples with excessive principal component (PC) variation: 
#' 
#'  * Step 1 : Create PCA scores for PC 1:3 
#'  * Step 2: Find samples with PC score > 1.5 standard deviation of median PC 
#' 
#' ***
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$summary_tables$pc_filter_summary)
#'
#' ***
#' 
#' 
#' ### Plot: Run order vs PC scores: pre-statTarget signal drift correction
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
master_list$pc_runorder_plots$pre_filter$PC1$plotly
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
master_list$pc_runorder_plots$pre_filter$PC2$plotly
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
master_list$pc_runorder_plots$pre_filter$PC3$plotly
#'
#' ***
#' 
#' ### Plot: PCA scores: post-principal component filter
#' PCA scores plot displaying data that has undergone the following process steps: 
#' 
#'  * missing value filter 
#'  * imputation of remaining missing values 
#'  * calculation of response ratios (target analyte peak area/internal standard peak area) 
#'  * calculation of concentration values 
#'  * principal component filtration 
#' 
#' 
#' ***
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
subplot(master_list$pca_analysis$pc_filter$sample_qc$plot_scores, master_list$pca_analysis$pc_filter$plate$plot_scores)
#'
#'
#' ***
#'
#' ### Process: statTarget signal drift correction of the data 
#' 
#'  * Data from each individual batch undergoes signal drift correction using statTarget package (https://stattarget.github.io/)
#'  * This is performed both within individual plates and across total batch 
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
knitr::kable(master_list$summary_tables$batch_correction_overview)
#' 
#' 
#' ***
#' 
#' ### Plot: Run order vs PC scores: post-statTarget signal drift correction
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
master_list$pc_runorder_plots$post_statTarget$PC1$plotly
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
master_list$pc_runorder_plots$post_statTarget$PC2$plotly
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
master_list$pc_runorder_plots$post_statTarget$PC3$plotly
#'
#' ***
#' 
#' ### Plot: PCA scores: post-signal drift/batch correction 
#' PCA scores plot displaying data that has undergone the following process steps: 
#' 
#'  * missing value filtering 
#'  * imputation of remaining missing values 
#'  * calculation of response ratios (target analyte peak area/internal standard peak area) 
#'  * calculation of concentration values 
#'  * principal component filtration 
#'  * signal drift correction using statTarget package
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
subplot(master_list$pca_analysis$statTarget_corrected$sample_qc$plot_scores, master_list$pca_analysis$statTarget_corrected$plate$plot_scores)
#'
#' 
#' ***
#' 
#' 
#' ### Environment summary
#' R version
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
print(master_list$environment$r_version)
#' Base packages
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
print(master_list$environment$base_packages)
#' User packages
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=5
print(master_list$environment$user_packages)
#' ***






