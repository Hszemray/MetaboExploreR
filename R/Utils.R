# utils.R

#Function for logging script run time
update_script_log <- function(master_list, ...) {

  # Capture the current time
    master_list$project_details$script_log$timestamps[[section_name]] <- Sys.time()

    # Calculate the runtime
    master_list$project_details$script_log$runtimes[[section_name]] <- difftime(
      master_list$project_details$script_log$timestamps[[section_name]],
      master_list$project_details$script_log$timestamps[[previous_section_name]],
      units = "mins"  # Set units to minutes
    )

    # Calculate the total runtime from the start
    master_list$project_details$script_log$runtimes$total_runtime <- difftime(
      master_list$project_details$script_log$timestamps[[section_name]],
      master_list$project_details$script_log$timestamps$start_time,
      units = "mins"  # Set units to minutes
    )

    # Create the message
    master_list$project_details$script_log$messages[[section_name]] <- paste0(
      "\n",toupper(gsub("_", " ", section_name))," complete!",
      "\n\n Section runtime: ", signif(as.numeric(master_list$project_details$script_log$runtimes[[section_name]]), digits = 3), " minutes",
      "\n\n Total runtime: ", signif(as.numeric(master_list$project_details$script_log$runtimes$total_runtime), digits = 3), " minutes",
      "\n",
      "\nInitialising: ", toupper(gsub("_", " ", next_section_name)), "...."
    )

    # Print the message
    cat(master_list$project_details$script_log$messages[[section_name]])

    # Assign the updated master_list to the global environment
    assign("master_list", master_list, envir = .GlobalEnv)
}

# Function for Imputation in qcCheckR
lgw_impute <- function(x) {
  map(.x = x, .f = ~ (min(.x[.x > 0], na.rm = TRUE))/2) %>%

    #use replace_na to replace NAs with min/2 value
    replace_na(
      data = x %>% mutate_all(~ replace(., . == 0, NA)), #note - replace zeros with NA to make compatible with replace_na()
      replace = .) #note - replace with list of min/2 values generated from map function in pipe (.)
}

#Function for restructuring project directory
move_folder <- function(source_dir, dest_dir, max_wait = 60) {
  # Check if source exists
  if (!dir.exists(source_dir)) {
    stop(paste("Source directory does not exist:", source_dir))
  }

  # Create destination if it doesn't exist
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Copy files
  files_to_copy <- list.files(source_dir, full.names = TRUE)
  success <- file.copy(files_to_copy, dest_dir, recursive = TRUE)

  if (!all(success)) {
    stop("Some files failed to copy.")
  }

  # Wait until files are not in use
  start_time <- Sys.time()
  while (TRUE) {
    # Try renaming a file to check if it's locked
    test_file <- files_to_copy[1]
    if (!file.exists(test_file)) break

    test_rename <- try(file.rename(test_file, paste0(test_file, ".tmp")), silent = TRUE)
    if (!inherits(test_rename, "try-error") && test_rename) {
      # Rename back
      file.rename(paste0(test_file, ".tmp"), test_file)
      break
    }

    # Timeout check
    if (as.numeric(Sys.time() - start_time, units = "secs") > max_wait) {
      stop("Files are still in use after waiting.")
    }

    Sys.sleep(2)  # Wait 2 seconds before retrying
  }

  # Delete source directory
  unlink(source_dir, recursive = TRUE, force = TRUE)

  # Confirm deletion
  if (dir.exists(source_dir)) {
    stop(paste("Failed to delete source directory:", source_dir))
  } else {
    message(paste("Successfully moved and deleted:", source_dir))
  }
}

# Package management
ensure_packages <- function() {
  package_list <- c('statTarget', 'svDialogs', 'ggpubr', 'janitor', 'plotly',
                    'knitr', 'viridisLite', 'mzR', 'httr', 'cowplot',
                    'matrixStats', 'tidyverse')

  installed <- c()
  already_available <- c()

  for (idx_package in package_list) {
    if (!require(package = idx_package, character.only = TRUE)) {
      message("Installing package: ", idx_package)
      install.packages(idx_package, dependencies = TRUE)
      suppressPackageStartupMessages(require(package = idx_package, character.only = TRUE))
      installed <- c(installed, idx_package)
    } else {
      already_available <- c(already_available, idx_package)
    }
  }
  message("Installed: ", paste(installed, collapse = ", "))
  message("Already available: ", paste(already_available, collapse = ", "))
}

