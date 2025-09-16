#' Import specific functions from packages
#' @keywords internal
#' @name install_import_external_functions
#' @importFrom BiocManager install
#' @importFrom utils install.packages update.packages
NULL

#' install_MetaboExploreR
#'
#' @description Helper function to install MetaboExplorer. Ensuring all packages
#' are installed and up to date.
#'
#' @export
#' @examples
#' \dontrun{
#' install_MetaboExploreR()
#'}
install_MetaboExploreR <- function() {
  if (!base::requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

  if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")

  BiocManager::install(ask = FALSE)

  update.packages(ask = FALSE)

  BiocManager::install("Hszemray/MetaboExplorer")
}
