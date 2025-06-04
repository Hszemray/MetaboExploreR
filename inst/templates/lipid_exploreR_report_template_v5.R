#' ---
#' title: "Targeted Lipid ExploreR QC Report v4"
#' author: ANPC
#' output: html_document
#' 
#' ---
#' <style>
#'.main-container {
#'  max-width: 98%;
#' }
#'</style>
#'
#'
#' ***
#' ### projectOverview
#' 
#' 
#' #### overallSummary
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
knitr::kable(master_list$summary_tables$odsAreaOverview[c(1:14),])
#' 
#' 
#' #### DataSummary
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
knitr::kable(master_list$summary_tables$projectOverview)
#' 
#' ***
#' ### DataQC Plots: 
#' 
#' #### Plot: PCA scores colored by sampleType
#'
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$pca$plot$sample_type_factor
#' 
#' ### Plot: PCA scores colored by plateID
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$pca$plot$sample_plate_id
#'
#'
#' ***
#' 
#' ### Plot: PCA scores run order on x vs PC score on y
#' 
#' #### PC1
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$pca$scoresRunOrder$PC1
#'
#' ***
#' #### PC2
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$pca$scoresRunOrder$PC2
#'
#' ***
#' #### PC3
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$pca$scoresRunOrder$PC3
#'
#' ***
#' 
#' ### Plot: Target lipid control charts
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$control_chart[[1]]
#'
#' ***
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$control_chart[[2]]
#'
#' ***
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$control_chart[[3]]
#'
#' ***
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$control_chart[[4]]
#'
#' ***
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$control_chart[[5]]
#'
#' ***
#' 
#+ 
#' ***
#' 
#' 
#' 
#' 
#' ### Environment summary
#' R version
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=7
print(master_list$environment$r_version)
#' Base packages
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=7
print(master_list$environment$base_packages)
#' User packages
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=7
print(master_list$environment$user_packages)
#' ***

