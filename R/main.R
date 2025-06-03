# main.R
source("R/Utils.R")
source("R/config.R")
source("R/SkylineR.R")
source("R/qcCheckR.R")

# Main execution flow
run_lipid_explorer <- function(project_directory) {
  SkylineR(project_directory)
  qcCheckR(project_directory)
}
