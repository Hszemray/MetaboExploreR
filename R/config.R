# config.R

#Update Script Log Functions----
#' Update Script Log
#'
#' This function updates the script log in the `master_list` object by capturing the current time, calculating the runtime for the current section, and creating a message for the log.
#'
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param previous_section_name A string representing the name of the previous section.
#' @param next_section_name A string representing the name of the next section.
#'
#' @return The updated `master_list` object with the new log information.

#'
#' @examples
#' \dontrun{
#' master_list <- list(project_details = list(script_log = list(timestamps = list(start_time = Sys.time()),
#'                     runtimes = list(), messages = list())))
#' update_script_log(master_list, "section_1", "start_time", "section_2")
#' }
update_script_log <- function(master_list, section_name, previous_section_name, next_section_name) {
  validate_previous_section(master_list, previous_section_name)
  master_list <- capture_current_time(master_list, section_name)
  master_list <- calculate_runtime(master_list, section_name, previous_section_name)
  master_list <- calculate_total_runtime(master_list, section_name)
  master_list <- create_message(master_list, section_name, next_section_name)
  master_list <- print_message(master_list, section_name)
  return(master_list)
}

#' Validate Previous Section
#'
#' This function validates the previous section name.
#'
#' @param master_list A list containing project details and script log information.
#' @param previous_section_name A string representing the name of the previous section.
#'
#' @return updated master list

validate_previous_section <- function(master_list, previous_section_name) {
  if (!previous_section_name %in% names(master_list$project_details$script_log$timestamps)) {
    stop("Invalid previous_section_name")
  }
}

#' Capture Current Time
#'
#' This function captures the current time for the given section.
#'
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#'
#' @return updated master list
capture_current_time <- function(master_list, section_name) {
  master_list$project_details$script_log$timestamps[[section_name]] <- Sys.time()
  return(master_list)
}

#' Calculate Runtime
#'
#' This function calculates the runtime for the given section.
#'
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param previous_section_name A string representing the name of the previous section.
#'
#' @return None

calculate_runtime <- function(master_list, section_name, previous_section_name) {
  master_list$project_details$script_log$runtimes[[section_name]] <- difftime(
    master_list$project_details$script_log$timestamps[[section_name]],
    master_list$project_details$script_log$timestamps[[previous_section_name]],
    units = "mins"
  )
  return(master_list)
}

#' Calculate Total Runtime
#'
#' This function calculates the total runtime from the start.
#'
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#'
#' @return None
calculate_total_runtime <- function(master_list, section_name) {
  master_list$project_details$script_log$runtimes$total_runtime <- difftime(
    master_list$project_details$script_log$timestamps[[section_name]],
    master_list$project_details$script_log$timestamps$start_time,
    units = "mins"
  )
  return(master_list)
}

#' Create Message
#'
#' This function creates a message for the log.
#'
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param next_section_name A string representing the name of the next section.
#'
#' @return None

create_message <- function(master_list, section_name, next_section_name) {
  master_list$project_details$script_log$messages[[section_name]] <- paste0(
    "\n", toupper(gsub("_", " ", section_name)), " complete!",
    "\n\n Section runtime: ", signif(as.numeric(master_list$project_details$script_log$runtimes[[section_name]]), digits = 3), " minutes",
    "\n\n Total runtime: ", signif(as.numeric(master_list$project_details$script_log$runtimes$total_runtime), digits = 3), " minutes",
    "\n",
    "\nInitialising: ", toupper(gsub("_", " ", next_section_name)), "...."
  )
  return(master_list)
}

#' Print Message
#'
#' This function prints the message for the log.
#'
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#'
#' @return None

print_message <- function(master_list, section_name) {
  cat(master_list$project_details$script_log$messages[[section_name]])
  return(master_list)
}


#Validate Project Directories Functiions----
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
  return(paste("Accessing project directory ", project_directory))
}

#' validate_master_list_project_directory
#' This function validates the existence of the project directory specified in the master list.
#' @param master_list A list containing project details.
#' @return Stops execution if the project directory does not exist.
#' @examples
#' \dontrun{
#' validate_master_list_project_directory(master_list)
#' }
validate_master_list_project_directory <- function(master_list) {
  if (!dir.exists(master_list$project_details$project_dir)) {
    stop(paste("Project directory does not exist:", master_list$project_details$project_dir))
  }
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
  return("mrm_template validation complete")
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
      data <- read_tsv(file_path)

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

