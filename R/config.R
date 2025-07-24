# config.R

#Update Script Log Functions----

##Primary Function----
#' Update Script Log
#'
#' This function updates the script log in the `master_list` object by capturing the current time, calculating the runtime for the current section, and creating a message for the log.
#'
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param previous_section_name A string representing the name of the previous section.
#' @param next_section_name A string representing the name of the next section.
#' @return The updated `master_list` object with the new log information.
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

##Seconday Functions----
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


#Validate Parameter Functions----
#' Validate Project Directory
#'
#' This function checks if the `project_directory` parameter is a single string and if the specified directory exists.
#'
#' @param project_directory A character string representing the path to the project directory.
#' @return TRUE if the validation is successful, otherwise an error is thrown.
#' @examples
#' \dontrun{
#' validate_project_directory("path/to/project_directory")
#' }
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
  message(paste("Accessing project directory ", project_directory))
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
#' \dontrun{
#' validate_mrm_template_list(list("path/to/template1.csv", "path/to/template2.csv"))
#' }
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
#' \dontrun{
#' log_error("An error occurred while processing the data.")
#' }
log_error <- function(error_message) {
  log_file <- "error_log.txt"
  write(error_message, file = log_file, append = TRUE)
}


#' Validate qcCheckR mrm template list
#'
#' This function validates the mrm_template_list list by checking the column headers and ensuring there are no NA or NULL values in the SIL_guide and conc_guide files.
#'
#' @param mrm_template_list A list specifying the file paths for the templates.
#' @return TRUE if validation passes. Stops execution if validation fails.
#' @examples
#' \dontrun{
#' validate_qcCheckR_mrm_template_list(master_list)
#' }
validate_qcCheckR_mrm_template_list <- function(master_list) {

    mrm_template_list <- master_list$templates$mrm_guides

    if (!is.list(mrm_template_list)) {
      stop("mrm_template_list must be a list.")
    }

    for (version in names(mrm_template_list)) {
      version_list <- mrm_template_list[[version]]

      # Check that version is a list
      if (!is.list(version_list)) {
        stop(paste("Each version in mrm_template_list must be a list. Problem with:", version))
      }

      for (guide in c("SIL_guide", "conc_guide")) {
        if (!guide %in% names(version_list)) {
          stop(paste("Missing", guide, "in version", version))
        }

        # Check required columns
        if(guide == "SIL_guide") {
          required_columns <- c("Molecule List Name", "Precursor Name",
                                "Precursor Mz", "Precursor Charge", "Product Mz",
                                "Product Charge", "Explicit Retention Time",
                                "Explicit Retention Time Window",
                                "Note", "control_chart")

          data <- mrm_template_list[[version]][[guide]]

          if (!all(required_columns %in% colnames(data))) {
            #store missing columns
            missing_columns <- setdiff(required_columns, colnames(data))
            stop(paste(guide, "for version", version, "\n Missing required columns: ",
                       paste(missing_columns, collapse = "\n")))
          }

          # Exclude 'note' column
          data_to_check <- data[, setdiff(names(data), c("Source","Note"))]

          # # Check for NA or NULL values
          # if (any(is.na(data_to_check))) {
          #   stop(paste("NA values found in", guide, "for version", version))
          # }

        } else if (guide == "conc_guide") {
          required_columns <- c("concentration_factor", "SIL_name")

          data <- mrm_template_list[[version]][[guide]]

          if (!all(required_columns %in% colnames(data))) {
            #store missing columns
            missing_columns <- setdiff(required_columns, colnames(data))
            stop(paste(guide, "for version", version, "\n Missing required columns: ",
                       paste(missing_columns, collapse = "\n")))
          }

          # if (any(is.na(data))) {
          #   stop(paste("NA values found in", guide, "for version", version))
          # }
        }
      }
    }

    message("Validation passed: mrm_template_list structure and contents are valid.")
    return(TRUE)
  }

# Validate Proteowizard and Skyline install----
#' Validate Proteowizard and Skyline install
#'
#' This function check proteowizard and skyline are correctly installed in users C dive program files.
#'
#' @return Returns a program found if installed in correct location or stops script and asks user to install if not.
#' @examples
#' \dontrun{
#' validate_proteowizard_skyline()
#' }
validate_proteowizard_skyline <- function() {
  base_path <- "C:\\Program Files\\"
  folders <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)

  # Check ProteoWizard
  if ("ProteoWizard" %in% folders) {
    message("ProteoWizard found in C:\\Program Files")
  } else {
    stop(message("ProteoWizard not found.\nPlease install it in C:\\Program Files from: https://proteowizard.sourceforge.io/download.html"))
  }

  # Check Skyline
  if ("Skyline" %in% folders) {
    message("Skyline found in C:\\Program Files")
  } else {
    stop(message("Skyline not found.\nPlease install it in C:\\Program Files from: https://skyline.ms/wiki/home/software/Skyline/page.view?name=install-administator-64"))
  }
}

# Validate Raw files----
#' Validate Raw files
#'
#' This function checks project directories contains wiff files and associated wiff.scan files.
#'
#' @return validated paths and returns message on outcome of check.
#' @examples
#' \dontrun{
#' all_file_paths <- validate_file_types()
#' }
validate_file_types <- function(project_directory) {
  file_path <- file.path(project_directory, "raw_data")
  files <- list.files(path = file_path, full.names = TRUE)

  validated_files <- c()
  invalid_files <- c()

  for (file in files) {
    if (grepl("\\.wiff$", file)) {
      scan_file <- paste0(file, ".scan")
      if (file.exists(scan_file)) {
        validated_files <- c(validated_files, file)
      } else {
        message("Missing .wiff.scan for: ", basename(file))
        invalid_files <- c(invalid_wiff_files, file)
      }
    } else if (grepl("\\.raw$", file)) {
      validated_files <- c(validated_files, file)
    } else if (grepl("\\.d$", file) && dir.exists(file)) {
      validated_files <- c(validated_files, file)
    } else if (grepl("\\.RAW$", file)) {
      validated_files <- c(validated_files, file)
    } else if (grepl("\\.yep$", file)) {
      validated_files <- c(validated_files, file)
    } else if (grepl("\\.td2$", file)) {
      validated_files <- c(validated_files, file)
    } else if (!grepl("\\.wiff\\.scan$", file)) {
      message("Unsupported file type found: ", basename(file))
      invalid_files <- c(invalid_files, file)
    }
  }

  if (length(validated_files) > 0) {
    message("Returning validated files for processing:\n", paste(basename(validated_files), collapse = "\n"))
    return(validated_files)
  }

  if (length(invalid_files) > 0) {
    message("Removed following unsupported files:\n", paste(invalid_wiff_files, collapse = "\n"))
  }

}


