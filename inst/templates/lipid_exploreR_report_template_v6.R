#' ---
#' title: "Targeted Lipid ExploreR QC Report"
#' author: "Australian National Phenome Centre"
#' date: "`r format(Sys.Date(), '%B %d, %Y')`"
#' output:
#'  html_document:
#'    df_print: paged
#'    toc: true
#'    toc_float: true
#'    code_folding: hide
#' ---
#' <style>
#'.main-container {
#'  max-width: 98%;
#' }
#'</style>
#'
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
#' ### Data QC PCA Scores Plots {.tabset}
#'
#' #### Coloured by sample_type
#'
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$pca$plot$sample_type_factor
#'
#' #### Coloured by plateID
#'
#+ echo=FALSE, message=FALSE, warning=FALSE,  fig.width=14, fig.height=9
master_list$pca$plot$sample_plate_id
#'
#'
#' ***
#'
#' ### PCA scores run order on x vs PC score on y {.tabset}
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
#' ### Target lipid control charts {.tabset}
#'
```{r,control, results='asis'}
for (i in names(master_list$control_chart)) {
  cat("#### ", i,"\n")
  cat("#+ echo=FALSE, message=FALSE, warning=FALSE, fig.width=14, fig.height=9\n")
  cat("master_list$control_chart[[", i, "]]", sep = "")
  cat("#' ***\n\n")
}
```
#'
#' ***
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
