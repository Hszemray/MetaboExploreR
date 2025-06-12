# config.R

#' Validate Project Directory
#'
#' This function checks if the `project_directory` parameter is a single string and if the specified directory exists.
#'
#' @param project_directory A character string representing the path to the project directory.
#' @return TRUE if the validation is successful, otherwise an error is thrown.
#' @examples
#' validate_project_directory("path/to/project_directory")
validate_project_directory <- function(project_directory) {
  # Check if project_directory is a single string
  if (!is.character(project_directory) || length(project_directory) != 1) {
    stop("project_directory must be a single string.")
  }

  # Check if the specified directory exists
  if (!dir.exists(project_directory)) {
    stop("The specified project directory does not exist.")
  }

  # Return TRUE if validation is successful
  return(TRUE)
}




#' Validate MRM Template List
#'
#' This function checks if the `mrm_template_list` parameter is a list of strings.
#'
#' @param mrm_template_list A list of character strings representing the paths to MRM templates.
#' @return TRUE if the validation is successful, otherwise an error is thrown.
#' @examples
#' validate_mrm_template_list(list("path/to/template1.csv", "path/to/template2.csv"))
validate_mrm_template_list <- function(mrm_template_list) {
  # Check if mrm_template_list is a list of strings
  if (!is.list(mrm_template_list) || !all(sapply(mrm_template_list, is.character))) {
    stop("mrm_template_list must be a list of strings.")
  }

  # Return TRUE if validation is successful
  return(TRUE)
}


#' Log Error to File
#'
#' This function logs error messages to a file named `error_log.txt`.
#'
#' @param error_message A character string representing the error message to be logged.
#' @return None. The function writes the error message to the log file.
#' @examples
#' log_error("An error occurred while processing the data.")
log_error <- function(error_message) {
  log_file <- "error_log.txt"
  write(error_message, file = log_file, append = TRUE)
}


#' Validate mrm template list
#'
#' This function validates the mrm_template_list list by checking the column headers and ensuring there are no NA or NULL values in the SIL_guide and conc_guide files.
#'
#' @param mrm_template_list A list specifying the file paths for the templates.
#' @return NULL. Stops execution if validation fails.
#' @examples
#' validate_template_info(mrm_template_list)
validate_template_info <- function(mrm_template_list) {
  required_columns <- c("Column1", "Column2") # Replace with actual required column names

  for (version in names(mrm_template_list)) {
    for (guide in c("SIL_guide", "conc_guide")) {
      file_path <- mrm_template_list[[version]][[guide]]
      data <- read.csv(file_path, stringsAsFactors = FALSE)

      # Check column headers
      if (!all(required_columns %in% colnames(data))) {
        stop(paste("Missing required columns in", guide, "for version", version))
      }

      # Check for NA or NULL values
      if (any(is.na(data)) || any(is.null(data))) {
        stop(paste("NA or NULL values found in", guide, "for version", version))
      }
    }
  }
}

