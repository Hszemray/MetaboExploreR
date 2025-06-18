#qcCheckR internal functions ----

#' LGW Impute
#'
#' This function imputes missing values in a tibble of peak area concentrations by replacing NAs with half the minimum non-zero value.
#'
#' @param x A tibble of peak area concentrations.
#'
#' @return A tibble with imputed values.
#'
#' @examples
#' \dontrun{
#' lgw_impute(tibble(a = c(0, 1, 2, NA), b = c(3, 0, NA, 4)))
#' }
lgw_impute <- function(x) {
  # Validate parameter
  if (!is_tibble(x)) {
    stop("Input must be a tibble.")
  }
  if (!all(sapply(x, is.numeric))) {
    stop("All columns in the tibble must be numeric.")
  }

  # Calculate min/2 values for each column
  min_half_values <- map(.x = x, .f = ~ {
    non_zero_values <- .x[.x > 0]
    if (length(non_zero_values) == 0) {
      return(NA)
    }
    min(non_zero_values, na.rm = TRUE) / 2
  })

  # Replace NAs with min/2 values
  x %>%
    mutate_all(~ replace(., . == 0, NA)) %>%
    replace_na(replace = min_half_values)
}

#' qcCheckR Setup Project
#'
#' This function sets up the project for QC by loading necessary packages, setting up the project directory, and loading and combining plate RDA files from SkylineR.
#' @param project_directory project directory inherited from qcCheckR.R
#' @return The updated `master_list` object with the project setup details.
#' @examples
#' \dontrun{
#' qcCheckR_setup_project()
#' }
qcCheckR_setup_project <- function(project_directory) {
  # Clean previous session
  rm(list = c(ls()[which(ls() != "project_directory")]))

  # Initialize start time for logging
  start_time <- Sys.time()

  # Load packages
  cran_packages <- c('svDialogs', 'ggpubr', 'janitor', 'plotly', 'knitr', 'viridisLite', 'httr', 'cowplot', 'matrixStats', 'tidyverse')
  for (pkg in cran_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      suppressMessages(library(pkg, character.only = TRUE))
    }
  }

  if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }

  bioc_packages <- c("mzR", "statTarget", "ropls")
  for (pkg in bioc_packages) {
    if (!require(pkg, character.only = TRUE)) {
      BiocManager::install(pkg, ask = FALSE, update = FALSE)
      suppressMessages(library(pkg, character.only = TRUE))
    }
  }
  rm(bioc_packages, cran_packages, pkg)

  # Project setup / load SkylineR
  skylineR_directory <- project_directory
  tempQCflag <- "all"

  if (tempQCflag == "all") {
    dir.create(paste0(skylineR_directory, "/", tempQCflag))
  }

  if (!dir.exists(paste0(skylineR_directory, "/", tempQCflag))) {
    stop(print(paste0(skylineR_directory, "/", tempQCflag, " does not exist. Check and re-run script.")))
  }

  if (dir.exists(paste0(skylineR_directory, "/", tempQCflag))) {
    skylineR_directory_all <- skylineR_directory
    skylineR_directory <- paste0(skylineR_directory, "/", tempQCflag)
    if (tempQCflag == "all") {
      skylineR_directory <- skylineR_directory_all
    }
  }

  rda_fileList <- list.files(skylineR_directory, pattern = "_skylineR", recursive = TRUE)
  rda_fileList <- rda_fileList[grepl(".rda", rda_fileList, ignore.case = TRUE)]
  rda_fileList <- rda_fileList[!grepl("archive", rda_fileList)]

  cat(paste0("The following .rda are currently selected for QC:\n   ~/", paste0(rda_fileList, collapse = " \n   ~/"), "\n To remove any .rda from processing workflow move them to a sub-folder tagged `archive`."))

  if (length(rda_fileList) > 0) {
    load(paste(skylineR_directory, rda_fileList, sep = "/"))
    master_list$data <- NULL
    master_list$project_details$wiff_file_paths <- NULL
    master_list$project_details$mzml_file_paths <- NULL
    master_list$data$global_timestamp <- NULL
  } else {
    stop(print(paste0("No .rda present in ", skylineR_directory)))
  }

  master_list$project_details$script_log$messages$rda_assesment <- paste0("The following .rda are currently selected for QC:\n   ~/", paste0(rda_fileList, collapse = " \n   ~/"), "\n To remove any .rda from processing workflow move them to a sub-folder tagged `archive`.")
  master_list$project_details$user_name <- "ANPC"
  master_list$project_details$project_name <- master_list$project_details$project_name
  master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "local directory")] <- skylineR_directory
  master_list$project_details$project_dir <- skylineR_directory
  master_list$project_details$github_master_dir <- "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR/refs/heads/main/v4/"
  master_list$project_details$plate_method_versions[[master_list$project_details$plateID]] <- master_list$project_details$is_ver
  master_list$project_details$plate_list[[master_list$project_details$plateID]] <- master_list$project_details$plateID
  master_list$project_details$plate_qc_type[[master_list$project_details$plateID]] <- master_list$project_details$qc_type

  masterListBatch <- master_list
  rm(master_list)
  backup <- masterListBatch

  # Load all plates into masterListBatch
  for (idx_rda in rda_fileList) {
    load(paste(skylineR_directory, idx_rda, sep = "/"))
    masterListBatch$data[[master_list$project_details$plateID]] <- master_list$data[[master_list$project_details$plateID]]
    masterListBatch$project_details$plate_method_versions[[master_list$project_details$plateID]] <- master_list$project_details$is_ver
    masterListBatch$project_details$plate_list[[master_list$project_details$plateID]] <- master_list$project_details$plateID
    masterListBatch$project_details$plate_qc_type[[master_list$project_details$plateID]] <- master_list$project_details$qc_type
    rm(master_list)
  }

  master_list <- masterListBatch
  rm(masterListBatch)
  master_list$project_details$plateID <- NULL
  master_list$project_details$is_ver <- NULL
  master_list$project_details$qc_type <- NULL

  if (tempQCflag == "all") {
    skylineR_directory <- paste0(skylineR_directory, "/", tempQCflag)
    master_list$project_details$plateID <- "all"
  }

  master_list$project_details$project_dir <- skylineR_directory
  master_list$project_details$lipidExploreR_version <- "6.0"
  master_list$project_details$qcCheckR_version <- "6.0"
  master_list$project_details$script_log$timestamps <- NULL
  master_list$project_details$script_log$timestamps <- list()
  master_list$project_details$script_log$timestamps$start_time <- start_time
  rm(start_time)
  master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "lipidExploreR version")] <- "4.0"
  master_list$summary_tables$area_project_summary$value[which(master_list$summary_tables$area_project_summary$`Project detail` == "lipidExploreR version")] <- "4.0"

  # Load templates/guides
  standard_convention <- function(x) {
    if ("precursor_name" %in% colnames(x)) {
      x$precursor_name <- gsub("[():]", ".", x$precursor_name)
    }
    if ("note" %in% colnames(x)) {
      x$note <- gsub("[():]", ".", x$note)
    }
    if ("sil_name" %in% colnames(x)) {
      x$sil_name <- gsub("[():]", ".", x$sil_name)
    }
    return(x)
  }

  master_list$templates <- list()
  master_list$templates$v1$SIL_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_lipid_mrm_template_v1.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  master_list$templates$v1$conc_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_SIL_batch_103.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  master_list$templates$v2$SIL_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_lipid_mrm_template_v2.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  master_list$templates$v2$conc_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_SIL_batch_Ultimate_2023-03-06.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  master_list$templates$v3$SIL_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_lipid_mrm_template_v3.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  master_list$templates$v3$conc_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_SIL_batch_Ultimate_2023-03-06.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  master_list$templates$v4$SIL_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_lipid_mrm_template_v4.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  master_list$templates$v4$conc_guide <- read_tsv(file = paste0(master_list$project_details$github_master_dir, "templates/LGW_SIL_batch_Ultimate_2023-03-06.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    standard_convention()

  # Source packages
  source("https://raw.githubusercontent.com/Hszemray/targeted_lipid_exploreR/main/v6_Automated/functions/FUNC_lipidExploreR_Update_script_log.R")
  master_list$environment$user_functions$update_script_log <- update_script_log

  master_list <- master_list$environment$user_functions$update_script_log(master_list, "project_setup", "start_time", "data_preparation")

  return(master_list)
}


##Phase 1: Data preparation ----
#' qcCheckeR Transpose Data
#'
#' This function transposes peak area data for each plate in the master list, converting it to a wide format and storing the result.
#'
#' @param master_list Master list containing peak area data.
#' @return The updated `master_list` object with transposed peak area data.
#' @examples
#' \dontrun{
#' qcCheckR_transpose_data(master_list)
#' }
qcCheckR_transpose_data <- function(master_list) {
  # Validate parameter
  if (!is.list(master_list) || !is.list(master_list$data$peakArea$skylineReport)) {
    stop("Invalid master_list format.")
  }

  master_list$data$peakArea$transposed <- list()

  # Run loop for each data plate
  for (idx_batch in master_list$project_details$plate_list) {
    message("Transposing plate: ", idx_batch)
    tryCatch({
      # Filter and pivot data
      transposed_data <- master_list$data$peakArea$skylineReport[[idx_batch]] %>%
        pivot_wider(
          id_cols = file_name,
          names_from = molecule_name,
          values_from = area,
          names_glue = "{molecule_name}"
        ) %>%
        dplyr::rename(sample_name = file_name)

      # Remove file extension
      transposed_data$sample_name <- sub(".mzML", "", transposed_data$sample_name)

      # Convert to numeric
      transposed_data[, -1] <- sapply(transposed_data[, -1], as.numeric) %>% as_tibble()

      # Store result
      master_list$data$peakArea$transposed[[idx_batch]] <- data.frame(transposed_data)

      message("Successfully transposed plate: ", idx_batch)

    }, error = function(e) {
      message("Error transposing plate: ", idx_batch)
      message("Error message: ", e$message)
    })
  }

  return(master_list)
}


#' Sort and QC Check Data
#'
#' This function sorts the data in `master_list` by run order and performs QC checks.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with sorted data and QC check results.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_sort_data(master_list)
#' }
qcCheckR_sort_data <- function(master_list) {
  # List for storing concentration data sorted by run order
  master_list$project_details$run_orders <- list()
  # Set up sorted area list
  master_list$data$peakArea$sorted <- list()

  # Run loop
  for (idx_batch in names(master_list$data$peakArea$transposed)) {
    master_list$project_details$run_orders[[idx_batch]] <-
      master_list$data$skyline_report[[idx_batch]] %>%
      select(contains(c("file_name", "acquired_time"))) %>%
      mutate(file_name = sub(".mzML", "", file_name)) %>%
      as_tibble() %>%
      rename(sample_name = file_name, sample_timestamp = acquired_time) %>%
      unique() %>%
      filter(!is.na(sample_timestamp) & sample_timestamp != "") %>%
      arrange(sample_timestamp)

    # Create metadata columns
    master_list$project_details$run_orders[[idx_batch]]$sample_plate_id <- idx_batch # plate_id
    master_list$project_details$run_orders[[idx_batch]]$sample_plate_order <- c(1:nrow(master_list$project_details$run_orders[[idx_batch]])) # sample_plate_order
    master_list$project_details$run_orders[[idx_batch]]$sample_type <- NA
    master_list$project_details$run_orders[[idx_batch]]$sample_matrix <- ifelse(grepl("(?i)SER", master_list$project_details$run_orders[[idx_batch]]$sample_name), "SER",
                                                                                ifelse(grepl("(?i)PLA", master_list$project_details$run_orders[[idx_batch]]$sample_name), "PLA", NA))

    # Set sample_type using mutate
    master_list$project_details$run_orders[[idx_batch]] <- master_list$project_details$run_orders[[idx_batch]] %>%
      mutate(
        sample_type = ifelse(str_detect(string = sample_name, pattern = "(?i)pqc"), "pqc",
                             ifelse(str_detect(string = sample_name, pattern = "(?i)vltr"), "vltr",
                                    ifelse(str_detect(string = sample_name, pattern = "(?i)sltr"), "sltr",
                                           ifelse(str_detect(string = sample_name, pattern = "(?i)ltr"), "ltr",
                                                  ifelse(str_detect(string = sample_name, pattern = "(?i)blank"), "blank",
                                                         ifelse(str_detect(string = sample_name, pattern = "(?i)istds"), "istds",
                                                                ifelse(str_detect(string = sample_name, pattern = "(?i)cond"), "conditioning",
                                                                       "sample"))))))))

    # Sort area_transposed data by run order and then remove conditioning runs
    master_list$data$peakArea$sorted[[idx_batch]] <- master_list$project_details$run_orders[[idx_batch]] %>%
      left_join(master_list$data$peakArea$transposed[[idx_batch]], by = "sample_name") %>%
      filter(sample_type == "pqc" | sample_type == "sample" | sample_type == "ltr" | sample_type == "sltr" | sample_type == "vltr") %>%
      arrange(sample_timestamp) %>%
      add_column(sample_run_index = c(1:nrow(.)), .before = 1)
  }

  # Dynamically set qc_type
  # Flag low number of QCs using ratio of QC to total samples for qc types
  master_list$project_details$qc_passed <- list()
  for (idxPlate in names(master_list$data$peakArea$sorted)) {
    for (qc in c("vltr", "ltr", "pqc")) {
      plateData <- master_list$data$peakArea$sorted[[idxPlate]]
      totalSamples <- length(plateData$sample_name)
      requiredQCs <- 6 / 96 # Minimum of 6 QC for 96 plate
      qcCount <- length(which(tolower(plateData$sample_type) == tolower(qc)))
      qcCountRatio <- qcCount / totalSamples
      if (qcCountRatio < requiredQCs || qcCount < 2) {
        master_list$project_details$qc_passed[[idxPlate]][[qc]] <- "fail"
      } else {
        master_list$project_details$qc_passed[[idxPlate]][[qc]] <- "pass"
      }
    }
  }

  # Summarise qc assessment pass/fail for all plates in project for qc types in master
  master_list$project_details$global_qc_pass <- list(vltr = "pass", ltr = "pass", pqc = "pass")
  for (idxPlate in names(master_list$data$peakArea$sorted)) {
    for (qc in c("vltr", "ltr", "pqc")) {
      if (master_list$project_details$qc_passed[[idxPlate]][[tolower(qc)]] == "fail") {
        master_list$project_details$global_qc_pass[[qc]] <- "fail"
      }
    }
  }

  # Set project qc_type based on qc assessment
  master_list$project_details$qc_type <- if (master_list$project_details$global_qc_pass$ltr == "pass") {
    "ltr"
  } else if (master_list$project_details$global_qc_pass$vltr == "pass") {
    "vltr"
  } else if (master_list$project_details$global_qc_pass$pqc == "pass") {
    "pqc"
  } else {
    "unknown"
  }

  # Stop script and print error if there are no viable QC for QC checker
  if (master_list$project_details$qc_type == "unknown") {
    # Capture the structure of the lists as strings
    global_qc_pass_str <- capture.output(str(master_list$project_details$global_qc_pass))
    plate_qc_passed_str <- capture.output(str(master_list$project_details$qc_passed))

    # Create a formatted message
    error_message <- paste0(
      "STOPPING SCRIPT \n",
      "There are no viable ltr, pqc, or vltr to qc in project:", master_list$project_details$project_name, ".\n",
      "Please refer to the details below:\n",
      "Global QC Assessment: ", paste(global_qc_pass_str, collapse = "\n"), "\n",
      "Plate QC Assessment: ", paste(plate_qc_passed_str, collapse = "\n")
    )

    # Stop execution and print the error message
    stop(error_message)
  } else {
    message <- paste("qcCheckeR has set QC to:", master_list$project_details$qc_type, "\n")
    message(message)
  }

  # Resume formatting of master_list$data$peakArea$sorted
  for (idx_batch in names(master_list$data$peakArea$transposed)) {
    # Add factor column for plotting
    master_list$data$peakArea$sorted[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
      add_column(sample_type_factor = master_list$data$peakArea$sorted[[idx_batch]]$sample_type %>%
                   factor(
                     levels = c("sample", master_list$project_details$qc_type, "ltr", "pqc", "vltr", "sltr")[!duplicated(c("sample", master_list$project_details$qc_type, "ltr", "pqc", "vltr", "sltr"))],
                     ordered = TRUE),
                 .after = "sample_type") %>%
      add_column(
        sample_type_factor_rev = factor(
          .$sample_type_factor,
          levels = rev(levels(.$sample_type_factor)),
          ordered = TRUE),
        .after = "sample_type_factor")

    # Convert sample type to "qc" for selected qc type and "sample" for everything else
    master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]][-which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]]))] <- "sample"
    master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]][which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]]))] <- "qc"

    # Add sample_data_source column
    master_list$data$peakArea$sorted[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
      add_column(sample_data_source = ".peakArea",
                 .after = "sample_type_factor_rev")
  }

  # Add a global run order based on mzML sample timestamp
  tempAllSamples <- master_list$data$peakArea$sorted %>%
    bind_rows() %>%
    arrange(sample_timestamp)
  # Set project master run index
  tempAllSamples$sample_run_index <- c(1:nrow(tempAllSamples))
  # Reset sorted list
  master_list$data$peakArea$sorted <- list()
  for (idx_batch in unique(tempAllSamples$sample_plate_id)) {
    master_list$data$peakArea$sorted[[idx_batch]] <- tempAllSamples %>%
      filter(sample_plate_id == idx_batch) %>%
      select(where(~any(!is.na(.))))
  }
  return(master_list)
}


#' Impute Missing Data
#'
#' This function imputes missing and zero values in the `master_list` data using the minimum intensity of that feature in the batch divided by 2.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with imputed data.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_impute_data(master_list)
#' }
qcCheckR_impute_data <- function(master_list) {
  # Imputation of the all zero value and missing data
  # Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

  # Set data list
  master_list$data$peakArea$imputed <- list()
  # Run loop in all plates
  for (idx_batch in names(master_list$data$peakArea$sorted)) {
    # Set all 0, NaN, is.infinite to a NA value for consistency
    # Create matrix
    master_list$data$peakArea$imputed[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
      column_to_rownames("sample_name") %>%
      select(-contains("sample")) %>%
      as.matrix()

    # Replace 0, NaN and Inf with NA for imputation (min/2)
    master_list$data$peakArea$imputed[[idx_batch]][master_list$data$peakArea$imputed[[idx_batch]] == 0] <- NA
    master_list$data$peakArea$imputed[[idx_batch]][is.infinite(master_list$data$peakArea$imputed[[idx_batch]])] <- NA
    master_list$data$peakArea$imputed[[idx_batch]][is.nan(master_list$data$peakArea$imputed[[idx_batch]])] <- NA

    # Run lgw_impute function
    master_list$data$peakArea$imputed[[idx_batch]] <- master_list$data$peakArea$imputed[[idx_batch]] %>%
      as.data.frame() %>%
      lgw_impute() %>%
      rownames_to_column("sample_name") %>%
      as_tibble() %>%
      # Samples that are 100% missing (zero/na) have imputation error as min/2(x) is inf. So this step replaces inf with zeros (will be flagged for filter later anyway).
      mutate_all(function(x) ifelse(is.infinite(x), 1, x)) %>%
      left_join(
        select(master_list$data$peakArea$sorted[[idx_batch]], contains("sample")),
        .,
        by = "sample_name"
      )

    # Tag data type
    master_list$data$peakArea$imputed[[idx_batch]]$sample_data_source <- ".peakAreaImputed"
  }
  return(master_list)
}


#' Calculate Response and Concentration
#'
#' This function calculates the response ratio and concentration for each sample in the `master_list` data.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with calculated response and concentration data.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_calculate_response_concentration(master_list)
#' }
qcCheckR_calculate_response_concentration <- function(master_list) {
  # Initialise lists
  master_list$data$response <- list()
  master_list$data$concentration <- list()

  # Store batches (plates)
  batches <- unique(names(master_list$data$peakArea$imputed))

  # For loop for SIL template version control by plate
  for (idx_batch in batches) {
    template_version <- master_list$project_details$plate_method_versions[[idx_batch]]

    master_list$project_details$is_ver <- template_version
    master_list[["templates"]][["Plate SIL version"]][[idx_batch]] <- template_version

    # Complete for both sorted and imputed lists
    for (idx_dataType in c("sorted", "imputed")) {
      # Find common cols between data and template
      SIL_filter <- intersect(
        colnames(master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>% select(contains("SIL"))),
        master_list$templates[[template_version]]$SIL_guide$note
      )

      # Select columns containing "SIL" that are in the SIL_filter
      sil_cols <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
        select(all_of(SIL_filter))

      # Select columns that do not contain "SIL"
      non_sil_cols <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
        select(-contains("SIL"))

      # Combine the two sets of columns
      master_list$data$peakArea[[idx_dataType]][[idx_batch]] <- bind_cols(non_sil_cols, sil_cols)

      # Set empty list to store output data for response
      master_list$data$response[[idx_dataType]][[idx_batch]] <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
        select(contains("sample"))

      # Set empty list to store output data for concentration
      master_list$data$concentration[[idx_dataType]][[idx_batch]] <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
        select(contains("sample"))

      # Run loop for each SIL IS
      for (idx_SIL in master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
           select(contains("SIL")) %>%
           names()) {
        if (length(which(master_list$templates[[template_version]]$SIL_guide$note == idx_SIL)) > 0) {
          # Find which SIL is used from the template
          target_lipids <- select(master_list$data$peakArea[[idx_dataType]][[idx_batch]],
                                  sample_name,
                                  any_of(master_list$templates[[template_version]]$SIL_guide$precursor_name[which(master_list$templates[[template_version]]$SIL_guide$note == idx_SIL)])) %>%
            column_to_rownames("sample_name")

          if (ncol(target_lipids) > 0) {
            # Calculate response ratio
            target_lipids_response <- as.matrix(target_lipids / master_list$data$peakArea[[idx_dataType]][[idx_batch]][[idx_SIL]])

            # Standardise data set to remove infinities and NAs
            target_lipids_response[is.na(target_lipids_response)] <- 0
            target_lipids_response[is.infinite(target_lipids_response)] <- 0

            # Rejoin and make master
            master_list$data$response[[idx_dataType]][[idx_batch]] <- left_join(
              by = "sample_name",
              master_list$data$response[[idx_dataType]][[idx_batch]],
              as.data.frame(target_lipids_response) %>%
                rownames_to_column("sample_name")
            )
          } # if ncol()

          ## 1.6.b. Convert response to single point concentration factor
          # Find the concentration factor of SIL
          sil_conc_factor <- master_list$templates[[template_version]]$conc_guide$concentration_factor[which(master_list$templates[[template_version]]$conc_guide$sil_name == idx_SIL)]

          if (length(sil_conc_factor) == 1) {
            # Select response data
            target_lipids <- master_list$data$response[[idx_dataType]][[idx_batch]] %>%
              select(sample_name,
                     any_of(master_list$templates[[template_version]]$SIL_guide$precursor_name[which(master_list$templates[[template_version]]$SIL_guide$note == idx_SIL)])) %>%
              column_to_rownames("sample_name")

            # Calculate concentration
            target_lipids_concentration <- as.matrix(target_lipids * sil_conc_factor)

            # Standardise data set to remove infinities and NAs
            target_lipids_concentration[is.na(target_lipids_concentration)] <- 0
            target_lipids_concentration[is.infinite(target_lipids_concentration)] <- 0

            # Rejoin and make master
            master_list$data$concentration[[idx_dataType]][[idx_batch]] <- left_join(
              by = "sample_name",
              master_list$data$concentration[[idx_dataType]][[idx_batch]],
              as.data.frame(target_lipids_concentration) %>%
                rownames_to_column("sample_name")
            )
          } # close if(length(sil_conc_factor) == 1)
        } # if length()
      } # idx_sil

      master_list$data$response[[idx_dataType]][[idx_batch]]$sample_data_source <- paste0(".response.", idx_dataType)
      master_list$data$concentration[[idx_dataType]][[idx_batch]]$sample_data_source <- paste0("concentration.", idx_dataType)
    } # idx_dataType
  } # SIL version control by plate

  # Find common lipids across all data frames
  # Complete for both sorted and imputed lists
  for (idx_dataType in c("response", "concentration")) {
    for (idx_dataTypeII in names(master_list$data[[idx_dataType]])) {
      # Find common lipids
      common_lipids <- Reduce(intersect, lapply(master_list$data[[idx_dataType]][[idx_dataTypeII]], colnames))

      # Select common lipids in each data frame
      for (idx_batch in names(master_list$data[[idx_dataType]][[idx_dataTypeII]])) {
        master_list$data[[idx_dataType]][[idx_dataTypeII]][[idx_batch]] <- master_list$data[[idx_dataType]][[idx_dataTypeII]][[idx_batch]] %>%
          select(all_of(common_lipids))
      }
    }
  }
  return(master_list)
}


#' Batch Correction and Signal Drift Adjustment
#'
#' This function performs batch correction and signal drift adjustment on the data in `master_list` using the `statTarget` package.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with corrected data.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_statTarget_batch_correction(master_list)
#' }
qcCheckR_statTarget_batch_correction <- function(master_list) {
  # Create batch correction directory
  if (!dir.exists(paste0(skylineR_directory, "/data"))) {
    dir.create(paste0(skylineR_directory, "/data"))
  }

  if (!dir.exists(paste0(skylineR_directory, "/data/batch_correction"))) {
    dir.create(paste0(skylineR_directory, "/data/batch_correction"))
  }

  if (!dir.exists(paste0(skylineR_directory, "/data/rda/"))) {
    dir.create(paste0(skylineR_directory, "/data/rda/"))
  }

  if (!dir.exists(paste0(skylineR_directory, "/data/batch_correction"))) {
    dir.create(paste0(skylineR_directory, "/data/batch_correction"))
  }

  if (!dir.exists(paste0(skylineR_directory, "/xlsx_report"))) {
    dir.create(paste0(skylineR_directory, "/xlsx_report"))
  }

  if (!dir.exists(paste0(skylineR_directory, "/html_report"))) {
    dir.create(paste0(skylineR_directory, "/html_report"))
  }

  # Set qc-type for statTarget
  master_list$project_details$statTarget_qc_type <- master_list$project_details$qc_type

  # Create data list
  FUNC_list <- list()
  FUNC_list$project_dir <- paste0(skylineR_directory, "/data/batch_correction")

  # Set up project folders for batch correct
  if (!dir.exists(paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results"))) {
    dir.create(paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results"))
  }

  setwd(paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results"))

  # Set master data for function
  FUNC_list$master_data <- bind_rows(master_list$data$concentration$imputed) # Apply on concentration data (post-impute)
  FUNC_list$master_data[["sample_type"]] <- "sample"
  FUNC_list$master_data[["sample_type"]][which(tolower(FUNC_list$master_data[["sample_type_factor"]]) == tolower(master_list$project_details$statTarget_qc_type))] <- "qc"

  # Flag QCs that have failed per plate because of mis-injection (very low signal (<10 % of median))
  temp.qcFail <- list()
  for (idx_batch in unique(FUNC_list$master_data$sample_plate_id)) {
    temp.rowSums <- FUNC_list$master_data %>%
      filter(sample_type == "qc", sample_plate_id == idx_batch) %>%
      select(!contains("sample")) %>%
      select(!contains("SIL")) %>%
      rowSums()
    temp.qcFail[[idx_batch]] <- (FUNC_list$master_data %>%
                                   filter(sample_type == "qc") %>%
                                   .$sample_name)[which(temp.rowSums < median(temp.rowSums * 0.1))]
  }

  temp.qcFail <- unlist(temp.qcFail)

  # Reset failed QC injections to "sample" so is not included in statTarget algorithm
  FUNC_list$master_data[["sample_type"]][which(FUNC_list$master_data[["sample_name"]] %in% temp.qcFail)] <- "sample"

  # Set metabolite list
  FUNC_list$metabolite_list <- master_list$data$concentration$imputed %>%
    bind_rows() %>%
    select(-contains("sample")) %>%
    names()

  # Create the required metadata file (PhenoFile) for statTarget::shiftCor
  FUNC_list$PhenoFile <- list()
  FUNC_list$PhenoFile$template <- FUNC_list$master_data %>%
    select(all_of("sample_name")) %>%
    dplyr::rename(sample = all_of("sample_name")) %>%
    add_column(FUNC_list$master_data %>%
                 select(all_of("sample_name"))) %>%
    add_column(FUNC_list$master_data %>%
                 select(all_of("sample_plate_id"))) %>%
    add_column(FUNC_list$master_data %>%
                 select(all_of("sample_type"))) %>%
    dplyr::rename(class = all_of("sample_type")) %>%
    add_column(FUNC_list$master_data %>%
                 select(all_of("sample_type"))) %>%
    add_column(FUNC_list$master_data %>%
                 select(all_of("sample_run_index")))

  FUNC_list$PhenoFile$template <- FUNC_list$PhenoFile$template %>%
    arrange_at("sample_run_index")

  # QC placement
  FUNC_list$PhenoFile$template_qc_order <- NULL
  qc_idx <- NULL
  for (idx_batch in FUNC_list$PhenoFile$template %>%
       select(all_of("sample_plate_id")) %>%
       unique() %>%
       as.matrix() %>%
       c()) {
    loop_temp_data <- FUNC_list$PhenoFile$template %>%
      filter(!!as.symbol("sample_plate_id") == idx_batch)

    loop_qc_idx <- which(loop_temp_data %>%
                           select(all_of("sample_type")) == "qc")

    if (loop_qc_idx > 1) {
      loop_temp_data <- loop_temp_data %>%
        slice(loop_qc_idx, 1:nrow(loop_temp_data)) %>%
        slice(-(loop_qc_idx + 1))
    }

    if (loop_qc_idx[length(loop_qc_idx)] < nrow(loop_temp_data)) {
      loop_temp_data <- loop_temp_data %>%
        slice(1:nrow(loop_temp_data), loop_qc_idx[length(loop_qc_idx)]) %>%
        slice(-loop_qc_idx[length(loop_qc_idx)])
    }

    qc_idx <- c(qc_idx, loop_qc_idx)

    FUNC_list$PhenoFile$template_qc_order <- bind_rows(FUNC_list$PhenoFile$template_qc_order, loop_temp_data)
  }

  FUNC_list$PhenoFile$template_qc_order$sample[which(FUNC_list$PhenoFile$template_qc_order %>%
                                                       select(all_of("sample_type")) == "qc")] <- paste0("QC", rep(1:length(qc_idx)))
  FUNC_list$PhenoFile$template_qc_order$sample[which(FUNC_list$PhenoFile$template_qc_order %>%
                                                       select(all_of("sample_type")) == "sample")] <- paste0("sample", rep(1:(nrow(FUNC_list$PhenoFile$template_qc_order) - length(qc_idx))))
  FUNC_list$PhenoFile$template_qc_order$class[which(FUNC_list$PhenoFile$template_qc_order %>%
                                                      select(all_of("sample_type")) == "qc")] <- NA

  FUNC_list$PhenoFile$template_sample_id <- FUNC_list$PhenoFile$template_qc_order %>%
    dplyr::rename(sample_id = all_of("sample_name"),
                  batch = all_of("sample_plate_id"),
                  order = all_of("sample_run_index")) %>%
    select(sample, batch, class, order, sample_id)

  FUNC_list$PhenoFile$template_sample_id$order <- c(1:nrow(FUNC_list$PhenoFile$template_sample_id))

  temp_batch <- 1
  for (idx_batch_set in unique(FUNC_list$PhenoFile$template_sample_id$batch)) {
    FUNC_list$PhenoFile$template_sample_id$batch[which(FUNC_list$PhenoFile$template_sample_id$batch == idx_batch_set)] <- temp_batch
    temp_batch <- temp_batch + 1
  }

  FUNC_list$PhenoFile$template_sample_id$batch <- FUNC_list$PhenoFile$template_sample_id$batch %>%
    as.numeric()

  FUNC_list$PhenoFile$PhenoFileOut <- FUNC_list$PhenoFile$template_sample_id %>%
    select(-sample_id)

  write_csv(x = FUNC_list$PhenoFile$PhenoFileOut,
            file = paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep = ""))

  # Create data for statTarget::shiftCor
  FUNC_list$ProfileFile <- list()
  FUNC_list$ProfileFile$template <- FUNC_list$master_data %>%
    select(all_of("sample_name"), all_of(FUNC_list$metabolite_list)) %>%
    dplyr::rename(sample_id = !!"sample_name")

  FUNC_list$ProfileFile$template_qc_order <- FUNC_list$PhenoFile$template_sample_id %>%
    select(sample, sample_id) %>%
    left_join(FUNC_list$ProfileFile$template, by = "sample_id") %>%
    select(-sample_id)

  FUNC_list$ProfileFile$ProfileFile <- as_tibble(
    cbind(nms = names(FUNC_list$ProfileFile$template_qc_order),
          t(FUNC_list$ProfileFile$template_qc_order))
  ) %>%
    setNames(.[1,]) %>%
    dplyr::rename(name = sample) %>%
    filter(name != "sample") %>%
    mutate(across(!contains("name", ignore.case = FALSE), as.numeric))

  FUNC_list$ProfileFile$metabolite_list <- FUNC_list$ProfileFile$ProfileFile %>%
    select(name) %>%
    select(!contains("SIL")) %>%
    add_column(metabolite_code = paste0("M", rep(1:nrow(FUNC_list$ProfileFile$ProfileFile))))

  FUNC_list$ProfileFile$ProfileFile <- left_join(
    FUNC_list$ProfileFile$metabolite_list,
    FUNC_list$ProfileFile$ProfileFile,
    by = "name") %>%
    select(-name) %>%
    dplyr::rename(name = metabolite_code)

  write_csv(x = FUNC_list$ProfileFile$ProfileFile,
            file = paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results/ProfileFile.csv"))

  samPeno <- paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep = "")
  samFile <- paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/ProfileFile.csv", sep = "")

  statTarget::shiftCor(samPeno = samPeno,
                       samFile = samFile,
                       Frule = 0,
                       ntree = 500,
                       MLmethod = 'QCRFSC',
                       imputeM = "minHalf",
                       plot = FALSE,
                       coCV = 10000)

  FUNC_list$corrected_data$data <- read_tsv(
    paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/statTarget/shiftCor/After_shiftCor/shift_all_cor.csv"),
    show_col_types = FALSE)

  if ("sample1" %in% colnames(FUNC_list$corrected_data$data)) {
    FUNC_list$corrected_data$data  <- FUNC_list$corrected_data$data  %>%
      filter(sample != "class") %>%
      dplyr::rename(name = sample) %>%
      mutate(across(!contains("name", ignore.case = FALSE), as.numeric))
  }

  if ("M1" %in% colnames(FUNC_list$corrected_data$data)) {
    FUNC_list$corrected_data$data  <- FUNC_list$corrected_data$data  %>%
      t() %>% data.frame() %>%
      rownames_to_column() %>%
      as_tibble() %>%
      setNames(.[1,]) %>%
      filter(sample != "class" & sample != "sample") %>%
      dplyr::rename(name = sample) %>%
      mutate(across(!contains("name", ignore.case = FALSE), as.numeric))
  }

  FUNC_list$corrected_data$data_transposed <- right_join(
    FUNC_list$ProfileFile$metabolite_list %>% dplyr::rename(lipid = metabolite_code),
    FUNC_list$corrected_data$data %>% dplyr::rename(lipid = name),
    by = "lipid"
  ) %>%
    select(-lipid) %>%
    as.matrix() %>%
    t() %>% data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    setNames(.[1,]) %>%
    filter(name != "name") %>%
    mutate(across(!contains("name", ignore.case = FALSE), as.numeric)) %>%
    dplyr::rename(sample = name) %>%
    left_join(x = FUNC_list$PhenoFile$template_sample_id,
              y = .,
              by = "sample") %>%
    dplyr::rename(!!"sample_name" := sample_id) %>%
    left_join(
      x = FUNC_list$master_data %>%
        select(contains("sample")),
      y = .,
      by = "sample_name"
    ) %>%
    select(-all_of(c("sample", "batch", "class", "order")))

  # Rejoin corrected data with master_list
  master_list$data$concentration$corrected <- list()

  for (idx_batch in unique(FUNC_list$corrected_data$data_transposed$sample_plate_id)) {
    master_list$data$concentration$corrected[[idx_batch]] <- FUNC_list$corrected_data$data_transposed %>%
      filter(sample_plate_id == idx_batch) %>%
      select(where(~any(!is.na(.))))
  }

  for (idx_batch in names(master_list$data$concentration$corrected)) {
    master_list$data$concentration$corrected[[idx_batch]]$sample_data_source <- ".peakAreaCorrected"
  }

  # Post-statTarget peak area mean adjustment
  FUNC_list$corrected_data$qc_means <- FUNC_list$master_data %>%
    filter(!!as.symbol("sample_type") == "qc") %>%
    select(-contains("sample")) %>%
    colMeans() %>%
    as_tibble() %>%
    dplyr::rename(original_mean = value) %>%
    add_column(metabolite = FUNC_list$master_data %>%
                 select(-contains("sample")) %>%
                 names(),
               .before = "original_mean") %>%
    left_join(.,
              FUNC_list$corrected_data$data_transposed %>%
                filter(!!as.symbol("sample_type") == "qc") %>%
                select(-contains("sample")) %>%
                colMeans() %>%
                data.frame %>%
                rownames_to_column() %>%
                as_tibble() %>%
                setNames(c("metabolite", "corrected_mean")),
              by = "metabolite") %>%
    add_column(correction_ratio = .$corrected_mean / .$original_mean)

  FUNC_list$corrected_data$data_qc_mean_adjusted <- FUNC_list$corrected_data$data_transposed
  for (idx_metabolite in FUNC_list$corrected_data$qc_means$metabolite) {
    FUNC_list$corrected_data$data_qc_mean_adjusted[[idx_metabolite]] <- FUNC_list$corrected_data$data_qc_mean_adjusted[[idx_metabolite]] /
      FUNC_list$corrected_data$qc_means[["correction_ratio"]][which(FUNC_list$corrected_data$qc_means[["metabolite"]] == idx_metabolite)]
  }

  FUNC_DATA_qc_type <- unique(bind_rows(master_list$data$concentration$imputed)$sample_type_factor[which(bind_rows(master_list$data$concentration$imputed)$sample_type == "qc")]) %>% as.character()
  FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type <- "sample"
  FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type[which(FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type_factor == FUNC_DATA_qc_type)] <- "qc"

  master_list$data$peakArea$statTargetProcessed <- list()
  master_list$data$concentration$statTargetProcessed <- list()
  for (idx_batch in unique(FUNC_list$corrected_data$data_qc_mean_adjusted$sample_plate_id)) {
    master_list$data$peakArea$statTargetProcessed[[idx_batch]] <- FUNC_list$corrected_data$data_qc_mean_adjusted %>%
      filter(sample_plate_id == idx_batch)
    master_list$data$concentration$statTargetProcessed[[idx_batch]] <- FUNC_list$corrected_data$data_qc_mean_adjusted %>%
      filter(sample_plate_id == idx_batch)
    master_list$data$peakArea$statTargetProcessed[[idx_batch]]$sample_data_source <- "concentration.statTarget"
  }

  setwd(master_list$project_details$project_dir)

  rm(list = c(ls()[which(ls() != "master_list")]))

  master_list$environment$user_functions$update_script_log(master_list, "data_preparation", "project_setup", "data_filtering")

  return(master_list)
}


##Phase 2: Data Filtering ----
# mising value data filtering is performed on peakArea data from the skyLine export.
# First the filtering is performed per sample to identify failed samples (e.g. injection, extraction, preparation errors, or if sample is missing from well) it will return a high % of missing values
# Once failed samples have been identified - the filtering then identifies lipids that have >50% missing values
# note: missing also refers to <limit of detection [<LOD]. This refers to instances of peak areas that are <5000 counts, as skyline will sometimes integrate noise giving a small value.


#' Set QC for Filtering
#'
#' This function sets the QC type for filtering based on the global QC pass status in the `master_list` data.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with the QC type set for filtering.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_set_qc(master_list)
#' }
qcCheckR_set_qc <-  function(master_list){
  #Set QC for filtering
  master_list$project_details$qc_type  <- if (master_list$project_details$global_qc_pass$pqc == "pass") {
    "pqc"
  } else if (master_list$project_details$global_qc_pass$ltr == "pass") {
    "ltr"
  } else {
    "unknown"
  }

  #Stop script and print error if there are no viable QC
  if (master_list$project_details$qc_type  == "unknown") {
    # Capture the structure of the lists as strings
    global_qc_pass_str <- capture.output(str(master_list$project_details$global_qc_pass))
    plate_qc_passed_str <- capture.output(str(master_list$project_details$qc_passed))

    # Create a formatted message
    error_message <- paste0(
      "STOPPING SCRIPT \n",
      "There are no viable ltr, pqc, or vltr for RSD filtering:", master_list$project_details$project_name, ".\n",
      "Please refer to the details below:\n",
      "Global QC Assessment: ", paste(global_qc_pass_str, collapse = "\n"), "\n",
      "Plate QC Assessment: ", paste(plate_qc_passed_str, collapse = "\n")
    )

    # Stop execution and print the error message
    stop(error_message)
  } else{
    message <- paste("qcCheckeR has set filtering QC to:", master_list$project_details$qc_type)
    cat(message)
  }
  #Initialise filters list for below filtering
  master_list$filters <- list()

  return(master_list)
}


#' Sample Filter
#'
#' This function flags samples based on missing values and summed signal intensity in the `master_list` data.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with sample filter flags.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_sample_filter(master_list)
#' }
qcCheckR_sample_filter <- function(master_list) {
  # Sample filter flags [missing value and summed signal]
  master_list$project_details$mv_sample_threshold <- "50" %>% as.numeric()

  # For loop generates sample filter flags by plate
  for (idx_batch in names(master_list$data$peakArea$sorted)) {
    master_list$filters$samples.missingValues[[idx_batch]] <- list()

    # Complete on raw uncorrected peakArea data
    master_list$filters$samples.missingValues[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
      bind_rows() %>%
      select(
        sample_run_index,
        sample_name,
        sample_plate_id,
        sample_type_factor
      )

    # Check the version of the plate
    template_version <- master_list$templates$`Plate SIL version`[[idx_batch]]

    # Generate summed target lipid intensity
    master_list$filters$samples.missingValues[[idx_batch]][["summed.lipid.signal"]] <- bind_rows(master_list$data$peakArea$sorted[[idx_batch]]) %>%
      select(!contains("sample")) %>%
      select(!contains("SIL")) %>%
      rowSums(na.rm = TRUE)

    # Generate summed SIL lipid intensity
    master_list$filters$samples.missingValues[[idx_batch]][["summed.SIL.Int.Std.signal"]] <- bind_rows(master_list$data$peakArea$sorted[[idx_batch]]) %>%
      select(contains("SIL")) %>%
      rowSums(na.rm = TRUE)

    # Zero values [samples.target.lipids] (values with a peakArea of <5000 are considered <LOD and are noise)
    master_list$filters$samples.missingValues[[idx_batch]][["missing.lipid[<LOD.peakArea<5000]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(!contains("SIL")) %>%
             as.matrix()) < 5000, na.rm = TRUE)

    # Zero values [samples.SIL.Int.Stds]
    master_list$filters$samples.missingValues[[idx_batch]][["missing.SIL.Int.Std[<LOD.peakArea<5000]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(contains("SIL")) %>%
             as.matrix()) < 5000, na.rm = TRUE)

    # NA values [samples.target.lipids]
    master_list$filters$samples.missingValues[[idx_batch]][["naValues[lipidTarget]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(!contains("SIL")) %>%
             as.matrix() %>%
             is.na()), na.rm = TRUE)

    # NA values [samples.SIL.Int.Stds]
    master_list$filters$samples.missingValues[[idx_batch]][["naValues[SIL.Int.Stds]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(contains("SIL")) %>%
             as.matrix() %>%
             is.na()), na.rm = TRUE)

    # NaN values [samples.target.lipids]
    master_list$filters$samples.missingValues[[idx_batch]][["nanValues[lipidTarget]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(!contains("SIL")) %>%
             as.matrix() %>%
             is.nan()), na.rm = TRUE)

    # NaN values [samples.SIL.Int.Stds]
    master_list$filters$samples.missingValues[[idx_batch]][["nanValues[SIL.Int.Stds]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(contains("SIL")) %>%
             as.matrix() %>%
             is.nan()), na.rm = TRUE)

    # Inf values [samples.target.lipids]
    master_list$filters$samples.missingValues[[idx_batch]][["infValues[lipidTarget]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(!contains("SIL")) %>%
             as.matrix() %>%
             is.infinite()), na.rm = TRUE)

    # Inf values [samples.SIL.Int.Stds]
    master_list$filters$samples.missingValues[[idx_batch]][["infValues[SIL.Int.Stds]"]] <- rowSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             bind_rows() %>%
             select(!contains("sample")) %>%
             select(contains("SIL")) %>%
             as.matrix() %>%
             is.infinite()), na.rm = TRUE)

    # Total missing values [samples.target.lipids]
    master_list$filters$samples.missingValues[[idx_batch]][["totalMissingValues[lipidTarget]"]] <- master_list$filters$samples.missingValues[[idx_batch]] %>%
      select(-contains("sample_")) %>%
      select(-contains("SIL")) %>%
      select(-contains("summed")) %>%
      as.matrix() %>%
      rowSums()

    # Total missing values [samples.SIL.Int.Stds]
    master_list$filters$samples.missingValues[[idx_batch]][["totalMissingValues[SIL.Int.Stds]"]] <- master_list$filters$samples.missingValues[[idx_batch]] %>%
      select(-contains("sample_")) %>%
      select(contains("SIL")) %>%
      select(-contains("summed")) %>%
      as.matrix() %>%
      rowSums()

    # Sample flag?
    master_list$filters$samples.missingValues[[idx_batch]]$sample.lipid.intensity.flag <- 0
    master_list$filters$samples.missingValues[[idx_batch]]$sample.lipid.intensity.flag[
      which(master_list$filters$samples.missingValues[[idx_batch]]$summed.lipid.signal < (median(master_list$filters$samples.missingValues[[idx_batch]]$summed.lipid.signal) * 0.20))
    ] <- 1

    # Low SIL.Int.Std flag indicating incorrect addition of internal standards
    master_list$filters$samples.missingValues[[idx_batch]]$sample.SIL.Int.Std.intensity.flag <- 0
    master_list$filters$samples.missingValues[[idx_batch]]$sample.SIL.Int.Std.intensity.flag[
      which(master_list$filters$samples.missingValues[[idx_batch]]$summed.SIL.Int.Std.signal < (median(master_list$filters$samples.missingValues[[idx_batch]]$summed.SIL.Int.Std.signal) * 0.33))
    ] <- 1

    # Missing value flag
    master_list$filters$samples.missingValues[[idx_batch]]$sample.missing.value.flag <- 0
    master_list$filters$samples.missingValues[[idx_batch]]$sample.missing.value.flag[which(
      master_list$filters$samples.missingValues[[idx_batch]]$`totalMissingValues[lipidTarget]` > ((
        bind_rows(master_list$data$peakArea$sorted[[idx_batch]]) %>%
          select(-contains("sample")) %>%
          select(-contains("SIL")) %>%
          ncol()) * (master_list$project_details$mv_sample_threshold / 100))
    )] <- 1

    master_list$filters$samples.missingValues[[idx_batch]]$sample.missing.value.flag[which(
      master_list$filters$samples.missingValues[[idx_batch]]$`totalMissingValues[SIL.Int.Stds]` > ((bind_rows(
        master_list$data$peakArea$sorted[[idx_batch]]) %>%
          select(-contains("sample")) %>%
          select(contains("SIL")) %>%
          ncol()) * (0.33))
    )] <- 1

    # Create an overall sample flag
    master_list$filters$samples.missingValues[[idx_batch]]$sample.flag <- 0
    master_list$filters$samples.missingValues[[idx_batch]]$sample.flag[
      which(
        (master_list$filters$samples.missingValues[[idx_batch]] %>%
           select(contains("flag")) %>% rowSums(na.rm = TRUE)) > 0
      )
    ] <- 1

    # Create failed sample list
    master_list$filters$failed_samples[[idx_batch]] <- filter(master_list$filters$samples.missingValues[[idx_batch]], sample.flag == 1)[["sample_name"]]
  }

  # Combine samples.missing values from plates
  combined_missing_values <- do.call(rbind, master_list$filters$samples.missingValues)
  master_list$filters$samples.missingValues <- combined_missing_values

  # Combine failed samples
  combined_failed_samples <- do.call(rbind, master_list$filters$failed_samples)
  master_list$filters$failed_samples <- combined_failed_samples

  rm(combined_missing_values, combined_failed_samples)

  return(master_list)
}


#' SIL Internal Standard Filter
#'
#' This function filters SIL internal standards based on missing values in the `master_list` data.
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with SIL internal standard filter flags.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_sil_IntStd_filter(master_list)
#' }
qcCheckR_sil_IntStd_filter <-  function(master_list){
  #sil.IntStd filter [missing value]
  master_list$filters$sil.intStd.missingValues <- list()
  # Initialise allplates as an empty data frame with the desired column names
  master_list$filters$sil.intStd.missingValues$summary <- data.frame(
    lipid = character(),
    template_version = character(),
    plateID = character(),
    peakArea_5000_LOD = numeric(),
    naValues = numeric(),
    nanValues = numeric(),
    infValues = numeric(),
    totalMissingValues = numeric(),
    flag_SIL_intStd_Plate = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop for every batch
  for(idx_batch in names(master_list$data$peakArea$sorted)){
    # Create SIL list
    master_list$filters$sil.intStd.missingValues[[idx_batch]] <- tibble(
      lipid = master_list$data$peakArea$sorted[[idx_batch]] %>%
        select(contains("SIL")) %>%
        names()
    )

    master_list$filters$sil.intStd.missingValues[[idx_batch]][["peakArea<5000[LOD]"]] <- colSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
             select(contains("SIL")) %>%
             as.matrix()) < 5000,
      na.rm = TRUE
    )

    #na values
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["naValues"]] <- colSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
             select(contains("SIL")) %>%
             as.matrix()) %>%
        is.na(), na.rm = TRUE
    )

    #nan values
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["nanValues"]] <- colSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
             select(contains("SIL")) %>%
             as.matrix()) %>%
        is.nan(), na.rm = TRUE
    )

    #inf values
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["infValues"]] <- colSums(
      x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
             filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
             select(contains("SIL")) %>%
             as.matrix()) %>%
        is.infinite(), na.rm = TRUE
    )

    #total missing values per plate
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["totalMissingValues"]] <- rowSums(
      x = (master_list$filters$sil.intStd.missingValues[[idx_batch]] %>%
             select(!contains("lipid"))%>%
             as.matrix()),
      na.rm = TRUE
    )

    #Keep Sil intStd if has <5% missing values
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["flag.SIL.intStd[Plate]"]] <- 0
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["flag.SIL.intStd[Plate]"]][
      which(master_list$filters$sil.intStd.missingValues[[idx_batch]][["totalMissingValues"]] >
              ((master_list$data$peakArea$sorted[[idx_batch]] %>%
                  filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
                  nrow) * 0.05)
      )] <- 1


    # Check the version of the plate and add to SIL list
    template_version <- master_list$templates$`Plate SIL version`[[idx_batch]]
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["template_version"]] <- template_version

    #Add plateID
    master_list$filters$sil.intStd.missingValues[[idx_batch]][["plateID"]] <- idx_batch

    # Bind results
    master_list$filters$sil.intStd.missingValues$summary <- rbind(
      master_list$filters$sil.intStd.missingValues$summary,
      master_list$filters$sil.intStd.missingValues[[idx_batch]]
    )
  }

  #All plates per version
  # Initialise the flag and failed list
  master_list$filters$sil.intStd.missingValues[["PROJECT.flag.SIL.intStd"]] <- list()
  master_list$filters$failed_sil.intStds <- list()

  # Loop over each version of lipid method present in data
  for (version in unique(master_list$filters$sil.intStd.missingValues$summary$template_version)) {

    # Get lipid and plate lists for this version
    lipid_list <- master_list$filters$sil.intStd.missingValues$summary %>%
      filter(template_version == version) %>%
      pull(lipid) %>%
      unique()

    plate_list <- master_list$filters$sil.intStd.missingValues$summary %>%
      filter(template_version == version) %>%
      pull(plateID) %>%
      unique()

    # Initialise the version-specific data frame
    master_list$filters$sil.intStd.missingValues$allPlates[[version]] <- tibble(lipid = lipid_list)

    # Filter and extract SIL matrix
    sil_matrix <- master_list$data$peakArea$sorted %>%
      bind_rows() %>%
      filter(sample_plate_id %in% plate_list) %>%
      filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
      select(contains("SIL")) %>%
      as.matrix()

    # Calculate missing value types
    master_list$filters$sil.intStd.missingValues$allPlates[[version]][["peakArea<5000[LOD]"]] <- colSums(sil_matrix < 5000, na.rm = TRUE)
    master_list$filters$sil.intStd.missingValues$allPlates[[version]][["naValues"]] <- colSums(is.na(sil_matrix), na.rm = TRUE)
    master_list$filters$sil.intStd.missingValues$allPlates[[version]][["nanValues"]] <- colSums(is.nan(sil_matrix), na.rm = TRUE)
    master_list$filters$sil.intStd.missingValues$allPlates[[version]][["infValues"]] <- colSums(is.infinite(sil_matrix), na.rm = TRUE)

    # Total missing values
    master_list$filters$sil.intStd.missingValues$allPlates[[version]][["totalMissingValues"]] <- rowSums(
      master_list$filters$sil.intStd.missingValues$allPlates[[version]] %>%
        select(-lipid) %>%
        as.matrix(),
      na.rm = TRUE
    )

    # Initialise flag vector
    master_list$filters$sil.intStd.missingValues[["PROJECT.flag.SIL.intStd"]][[version]] <- rep(0, length(lipid_list))

    # Count valid samples
    valid_sample_count <- nrow(sil_matrix)

    # Apply flag if >5% missing from samples
    master_list$filters$sil.intStd.missingValues[["PROJECT.flag.SIL.intStd"]][[version]][
      which(master_list$filters$sil.intStd.missingValues$allPlates[[version]]$totalMissingValues > (valid_sample_count * 0.05))
    ] <- 1

    # Create failed internal standard list
    master_list$filters$failed_sil.intStds[[version]] <- master_list$filters$sil.intStd.missingValues$allPlates[[version]] %>%
      filter(master_list$filters$sil.intStd.missingValues[["PROJECT.flag.SIL.intStd"]][[version]] == 1) %>%
      pull(lipid)
  }

  #combine failed internal standard version lists
  master_list$filters$failed_sil.intStds <- unique(unlist(master_list$filters$failed_sil.intStds, use.names = FALSE))

  return(master_list)
}


' Lipid Filter
#'
#' This function filters lipids based on missing values in the `master_list` data.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with lipid filter flags.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_lipid_filter(master_list)
#' }
qcCheckR_lipid_filter <-  function(master_list){
  # 2.3. lipid filter [missing value]
  master_list$filters$lipid.missingValues <- list()

  # Store batches (plates)
  batches <- unique(names(master_list$data$peakArea$sorted))

  for (idx_batch in batches) {
    # Create lipid list for the current batch
    master_list$filters$lipid.missingValues[[idx_batch]] <- tibble(
      lipid = master_list$data$peakArea$sorted[[idx_batch]] %>%
        select(!contains("sample") & !contains("SIL")) %>%
        names())

    # Initialise the flag
    master_list$filters$lipid.missingValues[[idx_batch]][["silFilter.flag.Lipid"]] <- 0
    # Retrieve the SIL version for the current batch
    SIL_version <- master_list[["templates"]][["Plate SIL version"]][[idx_batch]]
    # Find matching indices
    matching_indices <- which(master_list$filters$lipid.missingValues[[idx_batch]]$lipid %in%
                                filter(master_list$templates[[SIL_version]]$SIL_guide,
                                       note %in% master_list$filters$failed_sil.intStds)[["precursor_name"]])

    # Assign the value 1 to the silFilter.flag.Lipid column at the matching indices
    master_list$filters$lipid.missingValues[[idx_batch]][["silFilter.flag.Lipid"]][matching_indices] <- 1

    # Filter and select data
    filtered_data <- master_list$data$peakArea$sorted[[idx_batch]] %>%
      filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
      select(!contains("sample") & !contains("SIL")) %>%
      as.matrix()

    # Peak area < 5000 (LOD)
    master_list$filters$lipid.missingValues[[idx_batch]]
    master_list$filters$lipid.missingValues[[idx_batch]][["peakArea<5000[LOD]"]] <- colSums(filtered_data < 5000, na.rm = TRUE)

    # NA values
    master_list$filters$lipid.missingValues[[idx_batch]][["naValues"]] <- colSums(is.na(filtered_data), na.rm = TRUE)

    # NaN values
    master_list$filters$lipid.missingValues[[idx_batch]][["nanValues"]] <- colSums(is.nan(filtered_data), na.rm = TRUE)

    # Infinite values
    master_list$filters$lipid.missingValues[[idx_batch]][["infValues"]] <- colSums(is.infinite(filtered_data), na.rm = TRUE)

    # Total missing values for plate
    master_list$filters$lipid.missingValues[[idx_batch]][["totalMissingValues"]] <- rowSums(
      master_list$filters$lipid.missingValues[[idx_batch]] %>%
        select(!contains("lipid")) %>%
        as.matrix(),
      na.rm = TRUE
    )

    # Only keep lipids with <50% missing values
    master_list$filters$lipid.missingValues[[idx_batch]][["flag.Lipid[Plate]"]] <- 0
    master_list$filters$lipid.missingValues[[idx_batch]][["flag.Lipid[Plate]"]][which(
      master_list$filters$lipid.missingValues[[idx_batch]][["totalMissingValues"]] >
        (nrow(filtered_data) * 0.5)
    )] <- 1


    # Check the version of the plate and add to SIL list
    template_version <- master_list$templates$`Plate SIL version`[[idx_batch]]
    master_list$filters$lipid.missingValues[[idx_batch]][["template_version"]] <- template_version

    #Add plateID
    master_list$filters$lipid.missingValues[[idx_batch]][["plateID"]] <- idx_batch

    # Bind results
    master_list$filters$lipid.missingValues$summary <- rbind(
      master_list$filters$lipid.missingValues$summary,
      master_list$filters$lipid.missingValues[[idx_batch]]
    )

  }

  # for all plates
  #All plates per version
  # Initialise the flag and failed list
  master_list$filters$lipid.missingValues[["PROJECT.flag.lipid"]] <- list()
  master_list$filters$failed_lipids <- list()

  # Loop over each version of lipid method present in data
  for (version in unique(master_list$filters$lipid.missingValues$summary$template_version)) {

    # Get lipid and plate lists for this version
    lipid_list <- master_list$filters$lipid.missingValues$summary %>%
      filter(template_version == version) %>%
      pull(lipid) %>%
      unique()

    plate_list <- master_list$filters$lipid.missingValues$summary %>%
      filter(template_version == version) %>%
      pull(plateID) %>%
      unique()

    # Initialise the version-specific data frame
    master_list$filters$lipid.missingValues$allPlates[[version]] <- tibble(lipid = lipid_list)

    # Filter and extract lipid matrix
    lipid_matrix <- master_list$data$peakArea$sorted %>%
      bind_rows() %>%
      filter(sample_plate_id %in% plate_list) %>%
      filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
      select(!contains("sample") & !contains("SIL")) %>%
      as.matrix()

    # Calculate missing value types
    master_list$filters$lipid.missingValues$allPlates[[version]][["peakArea<5000[LOD]"]] <- colSums(lipid_matrix < 5000, na.rm = TRUE)
    master_list$filters$lipid.missingValues$allPlates[[version]][["naValues"]] <- colSums(is.na(lipid_matrix), na.rm = TRUE)
    master_list$filters$lipid.missingValues$allPlates[[version]][["nanValues"]] <- colSums(is.nan(lipid_matrix), na.rm = TRUE)
    master_list$filters$lipid.missingValues$allPlates[[version]][["infValues"]] <- colSums(is.infinite(lipid_matrix), na.rm = TRUE)

    # Total missing values
    master_list$filters$lipid.missingValues$allPlates[[version]][["totalMissingValues"]] <- rowSums(
      master_list$filters$lipid.missingValues$allPlates[[version]] %>%
        select(-lipid) %>%
        as.matrix(),
      na.rm = TRUE
    )

    # Initialise flag vector
    master_list$filters$lipid.missingValues[["PROJECT.flag.lipid"]][[version]] <- rep(0, length(lipid_list))

    # Count valid samples
    valid_sample_count <- nrow(lipid_matrix)

    # Apply flag if >5% missing from samples
    master_list$filters$lipid.missingValues[["PROJECT.flag.lipid"]][[version]][
      which(master_list$filters$lipid.missingValues$allPlates[[version]]$totalMissingValues > (valid_sample_count * 0.05))
    ] <- 1

    # Create failed lipids list
    master_list$filters$failed_lipids[[version]] <- master_list$filters$lipid.missingValues$allPlates[[version]] %>%
      filter(master_list$filters$lipid.missingValues[["PROJECT.flag.lipid"]][[version]] == 1) %>%
      pull(lipid)
  }

  #combine failed lipids lists
  master_list$filters$failed_lipids <- unique(unlist(master_list$filters$failed_lipids, use.names = FALSE))

  return(master_list)
}


#' RSD Filter
#'
#' This function filters features per plate with a %RSD >30% based on the precision of measurement in the `master_list` data.
#'
#' @param master_list Master_list from previous functions
#' @return The updated `master_list` with RSD filter flags.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_RSD_filter()
#' }
qcCheckR_RSD_filter <-  function(master_list){
  # 2.4. RSD filter
  # following mising value filtering, replicate QC samples are evaluated to see the precision of measurement (represented by % relative standard deviation)
  # features per plate with a %RSD >30% are flagged for removal

  ### 2.4.a. peakArea
  master_list$filters$rsd <- tibble()

  #per batch/plate
  for(idx_batch in names(master_list$data$peakArea$imputed)){
    loopData <- master_list$data$peakArea$imputed[[idx_batch]] %>%
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      filter(sample_type %in% c("qc")) %>%
      select(!contains("sample")) %>%
      select(!contains("SIL"))
    loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

    #loopMAD <- (apply(X = loopData, MARGIN = 2, FUN = mad) / apply(X = loopData, MARGIN = 2, FUN = median)) * 100 #Median absolute deviation


    #export tibble
    master_list$filters$rsd <- bind_rows(
      master_list$filters$rsd,
      rbind(c("peakArea",idx_batch, loopRSD)) %>%
        as_tibble() %>%
        mutate(across(names(loopRSD), as.numeric))
    )
  }

  #per project
  loopData <- master_list$data$peakArea$imputed %>%
    bind_rows() %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    filter(sample_type %in% c("qc")) %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

  #export tibble
  master_list$filters$rsd <- bind_rows(
    master_list$filters$rsd,
    rbind(c("peakArea", "allBatches", loopRSD)) %>%
      as_tibble() %>%
      mutate(across(names(loopRSD), as.numeric))
  )

  ### 2.4.b. concentration
  #per batch/plate
  for(idx_batch in names(master_list$data$concentration$imputed)){
    loopData <- master_list$data$concentration$imputed[[idx_batch]] %>%
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      filter(sample_type %in% c("qc")) %>%
      select(!contains("sample")) %>%
      select(!contains("SIL"))
    loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

    #export tibble
    master_list$filters$rsd <- bind_rows(
      master_list$filters$rsd,
      rbind(c("concentration",idx_batch, loopRSD)) %>%
        as_tibble() %>%
        mutate(across(names(loopRSD), as.numeric))
    )
  }

  #per project
  loopData <- master_list$data$concentration$imputed %>%
    bind_rows() %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    filter(sample_type %in% c("qc")) %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

  #export tibble
  master_list$filters$rsd <- bind_rows(
    master_list$filters$rsd,
    rbind(c("concentration","allBatches", loopRSD)) %>%
      as_tibble() %>%
      mutate(across(names(loopRSD), as.numeric))
  )

  ### 2.4.c. statTarget concentration
  #per batch/plate
  for(idx_batch in names(master_list$data$concentration$statTargetProcessed)){
    loopData <- master_list$data$concentration$statTargetProcessed[[idx_batch]] %>%
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      filter(sample_type %in% c("qc")) %>%
      select(!contains("sample")) %>%
      select(!contains("SIL"))
    loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

    #export tibble
    master_list$filters$rsd <- bind_rows(
      master_list$filters$rsd,
      rbind(c("concentration[statTarget]",idx_batch, loopRSD)) %>%
        as_tibble() %>%
        mutate(across(names(loopRSD), as.numeric))
    )
  }

  #per project
  loopData <- master_list$data$concentration$statTargetProcessed %>%
    bind_rows() %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    filter(sample_type %in% c("qc")) %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

  #export tibble
  master_list$filters$rsd <- bind_rows(
    master_list$filters$rsd,
    rbind(c("concentration[statTarget]","allBatches", loopRSD)) %>%
      as_tibble() %>%
      mutate(across(names(loopRSD), as.numeric))
  )
  #tidy rsd table
  master_list$filters$rsd <- master_list$filters$rsd %>%
    dplyr::rename(dataSource = V1,
                  dataBatch = V2) %>%
    mutate(across(!contains("data"), as.numeric)) %>%
    mutate(across(!contains("data"), round, 2))

  # Update script_log
  master_list$environment$user_functions$update_script_log(master_list, "data_filtering", "data_preparation", "summary_report")

  return(master_list)
}


##Phase 3: Summary Report----
#' Summary Report
#'
#' This function generates a summary report for the `master_list` data, including metrics for cohorts, matrix types, sample counts, lipid targets, SIL versions, missing value filter flags, and RSD percentages.
#'
#' @param master_list master_list generated by previous functions
#' @return The updated `master_list` with the summary report.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_summary_report()
#' }
qcCheckR_summary_report <-  function(master_list){
  ## 3.1. dataSetSummary
  metric =  c("Cohorts","MatrixType", "totalSamples", "studySamples", "ltrSamples", "vltrSamples", "sltrSamples", "pqcSamples",
              "lipidTargets", "SIL.version","SIL.IntStds",
              "missingValueFilterFlags[samples]", "missingValueFilterFlags[SIL.IS]", "missingValueFilterFlags[lipidTargets]",
              "rsd<30%[peakArea]", "rsd<20%[peakArea]", "rsd<10%[peakArea]",
              "rsd<30%[concentration]", "rsd<20%[concentration]", "rsd<10%[concentration]",
              "rsd<30%[concentration.statTarget]", "rsd<20%[concentration.statTarget]", "rsd<10%[concentration.statTarget]")

  # create tibble
  master_list$summary_tables <- NULL
  master_list$summary_tables$projectOverview <- tibble(metric = metric)

  #fill info for each plate/batch
  for(idx_batch in names(master_list$data$peakArea$sorted)){

    master_list$summary_tables$projectOverview <- left_join(
      master_list$summary_tables$projectOverview,
      rbind(
        #c("Cohorts", paste0(unique(master_list$data$peakArea$sorted[[idx_batch]]$sample_batch), collapse = ",")),
        c("MatrixType", unique(master_list$data$peakArea$sorted[[idx_batch]]$sample_matrix)),
        c("totalSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]])),
        c("studySamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "sample"))),
        c("ltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "ltr"))),
        c("vltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "vltr"))),
        c("sltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "sltr"))),
        c("pqcSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "pqc"))),
        c("lipidTargets", ncol(master_list$data$peakArea$sorted[[idx_batch]] %>% select(-contains("sample"), - contains("SIL")))),
        c("SIL.version", paste0(unique(master_list$templates$`Plate SIL version`[[idx_batch]]), collapse = ",")),
        c("SIL.IntStds", ncol(master_list$data$peakArea$sorted[[idx_batch]] %>% select(contains("SIL")))),
        c("missingValueFilterFlags[samples]", master_list$filters$samples.missingValues %>% filter(sample_plate_id == idx_batch & sample.flag == 1) %>% nrow()),
        c("missingValueFilterFlags[SIL.IS]", which(as.matrix(master_list[["filters"]][["sil.intStd.missingValues"]][[idx_batch]][["flag.SIL.intStd[Plate]"]])[,1] == 1) %>% length()),
        c("missingValueFilterFlags[lipidTargets]", which(as.matrix(master_list$filters$lipid.missingValues[[idx_batch]][["flag.Lipid[Plate]"]])[,1] == 1) %>% length()),
        c("rsd<30%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
        c("rsd<20%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
        c("rsd<10%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
        c("rsd<30%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
        c("rsd<20%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data"))%>% select(!any_of(master_list$filters$failed_lipids))  <20) %>% length()),
        c("rsd<10%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
        c("rsd<30%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids))  <30) %>% length()),
        c("rsd<20%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
        c("rsd<10%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[statTarget]") %>% select(!contains("data"))%>% select(!any_of(master_list$filters$failed_lipids))  <10) %>% length())
      ) %>%
        as_tibble() %>%
        dplyr::rename(metric = V1,
                      !! paste0(idx_batch) := V2),
      by = "metric"
    )
    #change column to numeric
    #master_list$summary_tables$projectOverview[[paste0(idx_batch)]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0(idx_batch)]])
  }

  ## 3.2. interPlate
  master_list$summary_tables$projectOverview <- left_join(
    master_list$summary_tables$projectOverview,
    rbind(
      #c("Cohorts", length(unique(master_list$filters$samples.missingValues$sample_batch))),
      c("MatrixType", paste0(bind_rows(master_list$data$peakArea$sorted)%>% select(contains("sample_matrix")) %>% unique(), collapse = ',')),
      c("totalSamples", nrow(bind_rows(master_list$data$peakArea$sorted))),
      c("studySamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.version", paste0((unique(master_list$templates$`Plate SIL version`)),collapse = ",")),
      c("SIL.IntStds", ncol(bind_rows(master_list$data$peakArea$sorted) %>% select(contains("SIL")))),
      c("missingValueFilterFlags[samples]", master_list$filters$samples.missingValues %>% filter(sample.flag == 1) %>% nrow()),
      c("missingValueFilterFlags[SIL.IS]", length(master_list$filters$failed_sil.intStds)),
      c("missingValueFilterFlags[lipidTargets]", length(master_list$filters$failed_lipids)),
      c("rsd<30%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
      c("rsd<20%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
      c("rsd<10%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
      c("rsd<30%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
      c("rsd<20%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
      c("rsd<10%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
      c("rsd<30%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
      c("rsd<20%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20)  %>% length()),
      c("rsd<10%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length())
    ) %>%
      as_tibble() %>%
      dplyr::rename(metric = V1,
                    !! paste0("allBatches") := V2),
    by = "metric"
  )

  # Update script_log
  master_list$environment$user_functions$update_script_log(master_list, "summary_report", "data_filtering", "plot_generation")

  return(master_list)
}

##Phase 4: Plot Generation----

#' Set Plot Options
#'
#' This function sets the plot color, fill, shape, and size options for the `master_list` data.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with plot options set.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_plot_options(master_list)
#' }
qcCheckR_plot_options <-  function(master_list){
  # 4.1. Set plot color/fill/shape/size
  #set plots colours
  master_list$project_details$plot_fill <-  c("sample" = "white",
                                              "ltr" = "steelblue2",
                                              "pqc" = "darkorange",
                                              "sltr" = "purple1",
                                              "vltr" = "seagreen")
  #set plots colours
  master_list$project_details$plot_colour <-  c("sample" = "black",
                                                "ltr" = "black",
                                                "pqc" = "black",
                                                "sltr" = "black",
                                                "vltr" = "black")
  #set preferred LTR type for subsequent QC filters
  master_list$project_details$plot_colour[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_colour)))] <- "red"

  #set plot shapes
  master_list$project_details$plot_shape <- c("sample" = 21,
                                              "ltr" = 21,
                                              "sltr" = 21,
                                              "vltr" = 21,
                                              "pqc" = 21)
  #set preferred LTR type for subsequent QC filters
  master_list$project_details$plot_shape[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_shape)))] <- 23
  #set plot size
  master_list$project_details$plot_size <- c("sample" = 2,
                                             "ltr" = 2,
                                             "sltr" = 2,
                                             "vltr" = 2,
                                             "pqc" = 2)
  #set preferred LTR type for subsequent QC filters
  master_list$project_details$plot_size[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_size)))] <- 3

  return(master_list)
}


#' PCA Analysis and Plotting
#'
#' This function performs PCA analysis on the `master_list` data and generates PCA plots using `plotly`.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with PCA models, scores, and plots.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_PCA(master_list)
#'}
qcCheckR_PCA <-  function(master_list){
  ### 4.2.a. create models and extract scores
  #setLists
  master_list$pca <- list()
  master_list$pca$models <- list()
  master_list$pca$scores <- list()
  for(idx_pca in c("peakArea", "concentration")){
    pca_x <- bind_rows(master_list$data[[idx_pca]]$imputed) %>%
      column_to_rownames("sample_name") %>%
      select(-contains("sample"), -contains("SIL")) %>%
      as.matrix()

    #build PCA
    master_list$pca$models[[idx_pca]] <- ropls::opls(
      x = pca_x,
      y=NULL,
      crossvalI = 1,
      predI = 3,
      algoC = "nipals",
      log10L = FALSE,
      scale = "pareto",
      plotSubC = NA,
      fig.pdfC = "none",
      subset = NULL)

    #extract scores data
    master_list$pca$scores[[idx_pca]] <- master_list$pca$models[[idx_pca]]@scoreMN %>%
      as_tibble() %>%
      dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
      add_column(.before = 1, sample_name = rownames(pca_x)) %>%
      left_join(by = "sample_name",
                .,
                bind_rows(master_list$data[[idx_pca]]$imputed) %>%
                  select(contains("sample"))
      )

    master_list$pca$scores[[idx_pca]]$sample_data_source <- paste0(unique(master_list$pca$scores[[idx_pca]]$sample_data_source),
                                                                   ": s=",
                                                                   dim(pca_x)[1],
                                                                   ", l=",
                                                                   dim(pca_x)[2]
    )

  }

  #statTarget model
  pca_x <- bind_rows(master_list$data$concentration$statTargetProcessed) %>%
    column_to_rownames("sample_name") %>%
    select(-contains("sample"), -contains("SIL")) %>%
    as.matrix()

  #build PCA
  master_list$pca$models[["concentration.statTarget"]] <- ropls::opls(
    x = pca_x,
    y=NULL,
    crossvalI = 1,
    predI = 3,
    algoC = "nipals",
    log10L = FALSE,
    scale = "pareto",
    plotSubC = NA,
    fig.pdfC = "none",
    subset = NULL)

  #extract scores data
  master_list$pca$scores[["concentration.statTarget"]] <- master_list$pca$models[["concentration.statTarget"]]@scoreMN %>%
    as_tibble() %>%
    dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
    add_column(.before = 1, sample_name = rownames(pca_x)) %>%
    left_join(by = "sample_name",
              .,
              bind_rows(master_list$data$concentration$statTargetProcessed) %>%
                select(contains("sample"))
    )


  master_list$pca$scores$concentration.statTarget$sample_data_source <- paste0(
    "concentration.statTarget",
    ": s=",
    dim(pca_x)[1],
    ", l=",
    dim(pca_x)[2]
  )


  #processed and filtered models
  for(idx_pca in c("peakArea", "concentration")){
    loopLipidFilter <- c(
      master_list$filters$failed_lipids,
      master_list$filters$rsd %>%
        filter(dataBatch == "allBatches" & dataSource == idx_pca) %>%
        select(-dataBatch) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        setNames(c("lipid", "value")) %>%
        .[-1,] %>%
        as_tibble() %>%
        mutate_at("value", as.numeric) %>%
        filter(value >30) %>%
        .$lipid
    ) %>%
      unique()

    pca_x <- bind_rows(master_list$data[[idx_pca]]$imputed) %>%
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      column_to_rownames("sample_name") %>%
      select(-contains("sample"), -contains("SIL"),
             -any_of(loopLipidFilter)) %>%
      as.matrix()

    #build PCA
    master_list$pca$models[[paste0(idx_pca, ".preProcessed")]] <- ropls::opls(
      x = pca_x,
      y=NULL,
      crossvalI = 1,
      predI = 3,
      algoC = "nipals",
      log10L = FALSE,
      scale = "pareto",
      plotSubC = NA,
      fig.pdfC = "none",
      subset = NULL)

    #extract scores data
    master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]] <- master_list$pca$models[[paste0(idx_pca, ".preProcessed")]]@scoreMN %>%
      as_tibble() %>%
      dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
      add_column(.before = 1, sample_name = rownames(pca_x)) %>%
      left_join(by = "sample_name",
                .,
                bind_rows(master_list$data[[idx_pca]]$imputed) %>%
                  select(contains("sample"))
      )

    master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]]$sample_data_source <- paste0(
      gsub(":.*", "", unique(master_list$pca$scores[[idx_pca]]$sample_data_source)),
      ".preProcessed: ",
      "s=",
      dim(pca_x)[1],
      ", l=",
      dim(pca_x)[2])

    # if(idx_pca =="peakArea"){
    #   master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]]$sample_data_source <- paste0(".", master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]]$sample_data_source)
    # }
  }

  #statTarget
  pca_x <- bind_rows(master_list$data$concentration$statTargetProcessed) %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    select(!any_of(master_list$filters$failed_lipids)) %>%
    select(!any_of(
      (master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
    )) %>%
    column_to_rownames("sample_name") %>%
    select(-contains("sample"), -contains("SIL")) %>%
    as.matrix()

  #build PCA
  master_list$pca$models[["concentration.statTarget.preProcessed"]] <- ropls::opls(
    x = pca_x,
    y=NULL,
    crossvalI = 1,
    predI = 3,
    algoC = "nipals",
    log10L = FALSE,
    scale = "pareto",
    plotSubC = NA,
    fig.pdfC = "none",
    subset = NULL)

  #extract scores data
  master_list$pca$scores[["concentration.statTarget.preProcessed"]] <- master_list$pca$models[["concentration.statTarget.preProcessed"]]@scoreMN %>%
    as_tibble() %>%
    dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
    add_column(.before = 1, sample_name = rownames(pca_x)) %>%
    left_join(by = "sample_name",
              .,
              bind_rows(master_list$data$concentration$statTargetProcessed) %>%
                select(contains("sample"))
    )

  master_list$pca$scores[["concentration.statTarget.preProcessed"]]$sample_data_source <- paste0(
    gsub(":.*","",unique(master_list$pca$scores$concentration.statTarget$sample_data_source)),
    ".preProcessed: ",
    "s=",
    dim(pca_x)[1],
    ", l=",
    dim(pca_x)[2])

  ### 4.2.b. create plotly scores
  #make plotly scores
  master_list$pca$plot <- list()
  for(idx_fill in c("sample_type_factor", "sample_plate_id")){
    #ggplotly
    master_list$pca$plot[[idx_fill]] <- ggplotly(
      ggplot(
        data = bind_rows(master_list$pca$scores),
        aes(x = PC1, y = PC2,
            group  = sample_name,
            fill = get(idx_fill),
            color = sample_type_factor,
            shape = sample_type_factor,
            size = sample_type_factor)) +
        geom_vline(xintercept = 0, colour = "darkgrey") +
        geom_hline(yintercept = 0, color = "darkgrey")+
        geom_point() +
        theme_bw() +
        scale_shape_manual(values = master_list$project_details$plot_shape) +
        (if(idx_fill == "sample_type_factor"){scale_fill_manual(values = master_list$project_details$plot_fill)}) +
        scale_color_manual(values = master_list$project_details$plot_colour) +
        scale_size_manual(values = master_list$project_details$plot_size) +
        guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=paste0(idx_fill))) +
        facet_wrap(facets = "sample_data_source", scales = "free", ncol = 2, nrow = 3)
    ) %>% layout(
      title = list(
        text = paste0("PCA scores; coloured by ", idx_fill, "; ", master_list$project_details$project_name),
        y = 1.1,
        x=0.05),
      margin = list(
        l = 10, r = 10, b=65, t=85),
      legend = list(
        orientation = "v",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        y = 0.5,
        x= 1.075)           # put legend in center of x-axis
    )}
  return(master_list)
}

#' Run Order Plots
#'
#' This function generates run order plots for the `master_list` data, showing PCA scores versus run order.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with run order plots.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_run_order_plots(master_list)
#' }
qcCheckR_run_order_plots <-  function(master_list){
  ### runOrder plots
  #### find plateBoundary
  #find plate boundaries for vline
  plate_boundary = 0.5
  annotate_coordinate = NULL
  boundary_finder <- bind_rows(master_list$pca$scores) %>%
    select(sample_run_index, sample_plate_id) %>%
    distinct()

  for(idx_plate in unique(boundary_finder$sample_plate_id)){
    plate_boundary <- c(
      plate_boundary,
      c(min((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]]) - 0.5,
        max((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]]) + 0.5)
    )

    annotate_coordinate <- c(
      annotate_coordinate,
      median((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]])
    )
  }
  #remove duplicates
  plate_boundary <- unique(plate_boundary)
  #finalise annotation co-ordinate
  boundary_finder$annotation <- paste0(boundary_finder$sample_plate_id)
  annotate_coordinate <- setNames(annotate_coordinate, paste0(unique(boundary_finder$annotation)))

  #make data.frame
  annotate_label <- tibble(
    sample_data_source = rep("x.plateID", length(names(annotate_coordinate))),
    sample_plate_id = names(annotate_coordinate),
    sample_run_index = annotate_coordinate,
    PC1=0,PC2 =0, PC3=0, value =0
  )

  #pca score vs run order
  #plot PCA scores vs run order on x
  #make plotly
  master_list$pca$scoresRunOrder <- list()
  for(idx_pca in c("PC1", "PC2", "PC3")){
    master_list$pca$scoresRunOrder[[idx_pca]] <- ggplotly(
      ggplot(
        data = bind_rows(master_list$pca$scores),
        aes(x = sample_run_index, y = get(idx_pca),
            group = sample_name,
            fill = sample_type_factor,
            color = sample_type_factor,
            shape = sample_type_factor,
            size = sample_type_factor,
        )) +
        geom_vline(xintercept = plate_boundary, linetype = "dashed") +
        geom_point() +
        theme_bw() +
        scale_shape_manual(values = master_list$project_details$plot_shape) +
        scale_fill_manual(values = master_list$project_details$plot_fill) +
        scale_color_manual(values = master_list$project_details$plot_colour) +
        scale_size_manual(values = master_list$project_details$plot_size) +
        ylab(paste0(idx_pca)) +
        guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=paste0(idx_fill))) +
        geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = sample_run_index,  y = get(idx_pca), label = sample_plate_id), col = "black") +
        facet_wrap(facets = "sample_data_source", ncol = 1, scales = "free_y")
    ) %>% layout(
      title = list(
        text = paste0(idx_pca, "; run order (x) vs PCA scores (y) ; ", master_list$project_details$project_name),
        y = 1.1,
        x=0.05),
      margin = list(
        l = 10, r = 10, b=65, t=85),
      legend = list(
        orientation = "v",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 1.1,
        y= 0.5
      )           # put legend in center of x-axis
    )
  }

  return(master_list)
}



#' Target Control Charts
#'
#' This function generates control charts for the `master_list` data, showing the values of target metabolites and SIL internal standards across different sample types and data sources.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with control charts.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_target_control_charts(master_list)
#' }
qcCheckR_target_control_charts <-  function(master_list){
  #setLists
  master_list$control_charts <- list()
  precursor_lists <- list()
  Control_SIL_List <- list()
  Control_SIL_List_Common <- list()

  # Create guide for SILS in common through versions
  SIL_versions <- unique(unlist(master_list[["templates"]][["Plate SIL version"]]))

  for (SIL_ver in SIL_versions) {
    SIL_Temp <- master_list$templates[[SIL_ver]]$SIL_guide[grepl("TRUE", master_list$templates[[SIL_ver]]$SIL_guide$control_chart),]
    precursor_lists[[SIL_ver]] <- SIL_Temp$precursor_name
    Control_SIL_List[[SIL_ver]] <- SIL_Temp
  }

  common_control_charts <- Reduce(intersect, precursor_lists)
  Control_SIL_List_Common <- bind_rows(Control_SIL_List)
  Control_SIL_List_Common <- Control_SIL_List_Common %>% filter(Control_SIL_List_Common$precursor_name %in% common_control_charts)

  for (idx_metabolite in common_control_charts) {
    # Extract SIL int.std
    idx_sil <- Control_SIL_List_Common %>% filter(Control_SIL_List_Common$precursor_name == idx_metabolite)

    # Plot generation
    master_list$control_charts[[idx_metabolite]] <- ggplotly(
      ggplot(
        data = bind_rows(
          bind_rows(master_list$data$peakArea$imputed) %>% select(contains("sample"), any_of(idx_metabolite)) %>%
            mutate(sample_data_source = ".peakArea"),

          # Add SIL data from various versions
          bind_rows(master_list$data$peakArea$imputed) %>%
            select(contains("sample"), any_of(idx_sil$note)) %>%
            rowwise() %>%
            mutate(SIL = paste(na.omit(c_across(contains("SIL"))), collapse = " ")) %>%
            ungroup() %>%
            mutate(SIL = as.numeric(SIL)) %>%
            dplyr::rename(!!paste0(idx_metabolite) := SIL) %>%
            select(-contains("SIL")) %>%
            mutate(sample_data_source = ".SIL.peakArea"),

          # Add concentration data
          bind_rows(master_list$data$concentration$imputed) %>%
            select(contains("sample"), any_of(idx_metabolite)) %>%
            mutate(sample_data_source = "concentration.preprocessed"),

          # Add statTarget concentration data
          bind_rows(master_list$data$concentration$statTargetProcessed) %>%
            select(contains("sample"), any_of(idx_metabolite)) %>%
            mutate(sample_data_source = "statTargetConcentration.preprocessed"),

        )
        #rename metabolite
        %>% dplyr::rename(!!paste0("value") := !!paste0(idx_metabolite)),

        #Structure ggplot
        aes(x = sample_run_index, y = value,
            group = sample_name,
            fill = sample_type_factor,
            color = sample_type_factor,
            shape = sample_type_factor,
            size = sample_type_factor)) +
        geom_vline(xintercept = plate_boundary, linetype = "dashed") +
        geom_point() +
        theme_bw() +
        scale_shape_manual(values = master_list$project_details$plot_shape) +
        scale_fill_manual(values = master_list$project_details$plot_fill) +
        scale_color_manual(values = master_list$project_details$plot_colour) +
        scale_size_manual(values = master_list$project_details$plot_size) +
        ylab(paste0(idx_metabolite)) +
        guides(shape = "none", size = "none", color = "none", fill = guide_legend(title = paste0("sample_type"))) +
        geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = sample_run_index, y = value, label = sample_plate_id), col = "black") +
        facet_wrap(facets = "sample_data_source", ncol = 1, scales = "free_y")
    ) %>% layout(
      title = list(
        text = paste0(idx_metabolite, "; control chart; ", master_list$project_details$project_name),
        y = 1.1,
        x = 0.05),
      margin = list(
        l = 10, r = 10, b = 65, t = 85),
      legend = list(
        orientation = "v",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 1.1,
        y = 0.5
      )           # put legend in center of x-axis
    )
  }

  # Update script_log
  master_list$environment$user_functions$update_script_log(master_list, "plot_generation", "summary_report", "data_exports")

  return(master_list)
}

##Phase 5: Exports----


#' Export XLSX File
#'
#' This function exports the `master_list` data to an XLSX file with multiple tabs, including user guides, QC performance, sample missing values, lipid missing values, lipid QC RSD, and various data sets.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with the exported XLSX file.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_export_xlsx_file(master_list)
#' }
qcCheckR_export_xlsx_file <-  function(master_list){
  ## 5.1. xlsx file
  ### 5.1.a. create peakArea user guide
  master_list$summary_tables$odsAreaOverview <- rbind(
    c("projectName", master_list$project_details$project_name),
    c("user", master_list$project_details$user_name),
    c("lipidExploreR_version", master_list$project_details$lipidExploreR_version),
    c("qcType_preProcessing|filtering", master_list$project_details$qc_type),
    c("SIL_Int.Std_version", paste(unique(master_list[["templates"]][["Plate SIL version"]]), collapse = ',', sep = ',')),
    c("total.StudyPlates", bind_rows(master_list$data$peakArea$sorted)[["sample_plate_id"]] %>% unique() %>% length()),
    c("total.Samples", bind_rows(master_list$data$peakArea$sorted) %>% nrow()),
    c("studySamples", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sample") %>% nrow()),
    c("ltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "ltr") %>% nrow()),
    c("vltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "vltr") %>% nrow()),
    c("sltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sltr") %>% nrow()),
    c("pqc", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "pqc") %>% nrow()),
    c("total.LipidFeatures", bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol()),
    c("total.SIL.Int.Stds", bind_rows(master_list$data$peakArea$sorted) %>% select(contains("SIL")) %>% ncol()),
    c("", ""),
    c("TAB_DESCRIPTION:", ""),
    c("QC.platePerformance", "overview of project quality performance (per plate)"),
    c("QC.samplesMV", "detailed overview of sample quality (missing values)"),
    c("QC.lipidsMV", "detailed overview of lipid quality (missing values)"),
    c("QC.lipidQcRsd", "detailed overview of lipid quality (% RSD in QC samples)"),
    c("DATA.lipidPeakArea", "lipid target peak area integrals (skylineMS)"),
    c("DATA.silPeakArea", "stable isotope labelled internal standard peak area integrals (skylineMS)"),
    c("DATA.all.concentration", "peakArea >> missing value imputed [min/2] >> ratio with SIL IS >> single point concentration factor adjusted"),
    c("DATA.all.concentration.S.T.", "peakArea >> missing value imputed [min/2]  >> ratio with SIL IS >> signalDrift|batch correction using the statTarget r package >> single point concentration factor adjusted"),
    c("DATA.preProcessed.conc.S.T.", "peakArea >> missing value imputed [min/2]  >> ratio with SIL IS >> signalDrift|batch correction using the statTarget r package >> single point concentration factor adjusted >> filtered")
  ) %>%
    as_tibble()

  ### 5.1.b. write .xlsx tabbed file

  openxlsx::write.xlsx(
    file = paste0(master_list$project_details$project_dir,
                  "/xlsx_report/",
                  Sys.Date(), "_", master_list$project_details$user_name, "_",
                  master_list$project_details$project_name,
                  "_",
                  master_list$project_details$plateID,
                  "_lipidData_qcCheckeR_v4.xlsx"),
    overwrite = TRUE,
    x = list(
      "userGuide" = master_list$summary_tables$odsAreaOverview,
      #summary
      "QC.platePerformance" = master_list$summary_tables$projectOverview,
      "QC.sampleMV" = master_list$filters$samples.missingValues,
      "QC.lipidsMV" = master_list$filters$lipid.missingValues$allPlates%>% bind_rows() ,
      "QC.lipidQcRsd" = master_list$filters$rsd %>% mutate(across(!contains("data"), round, 2)) %>%
        add_column(data = paste0(.$dataSource, ".", .$dataBatch), .before = 1) %>%
        select(-dataSource, - dataBatch) %>% t() %>% as.data.frame() %>% rownames_to_column() %>% setNames(.[1,]) %>% filter(data != "data")%>% as_tibble() %>% mutate(across(!contains("data"), as.numeric)),
      #data
      "DATA.peakArea" = bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("SIL")),
      "DATA.silPeakArea" = bind_rows(master_list$data$peakArea$sorted) %>% select(contains("sample") | contains("SIL")),
      "DATA.all.concentration" = bind_rows(master_list$data$concentration$sorted),
      "DATA.preProcessed.concentration" = bind_rows(master_list$data$concentration$imputed) %>%
        filter(!sample_name %in% master_list$filters$failed_samples) %>%
        select(!any_of(master_list$filters$failed_lipids)) %>%
        select(!any_of(
          (master_list$filters$rsd %>% filter(dataSource == "concentration" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
        )),
      "DATA.all.concentration.S.T." = bind_rows(master_list$data$concentration$statTargetProcessed),
      "DATA.preProcessed.conc.S.T." = bind_rows(master_list$data$concentration$statTargetProcessed) %>%
        filter(!sample_name %in% master_list$filters$failed_samples) %>%
        select(!any_of(master_list$filters$failed_lipids)) %>%
        select(!any_of(
          (master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
        ))
    )
  )

  return(master_list)
}


#' Export HTML Report
#'
#' This function exports the `master_list` data to an HTML report using a specified RMarkdown template.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with the exported HTML report.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_export_html_report(master_list)
#' }
qcCheckR_export_html_report <-  function(master_list){
  # ### 5.2.a. download template
  # fileConn<-file(paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v5 Developmental/templates/lipid_exploreR_report_template_v5.R"))
  # writeLines(httr::GET(url = paste0(master_list$project_details$github_master_dir, "templates/lipid_exploreR_report_template_v4.R")) %>%
  #              httr::content(as = "text"), fileConn)
  # close(fileConn)

  ### 2. render template
  rmarkdown::render(input = system.file("templates", "lipid_exploreR_report_template_v5.R", package = "MetaboExploreR"),
                    output_format = "html_document",
                    output_dir = paste0(master_list$project_details$project_dir, "/html_report"),
                    output_file = paste0(master_list$project_details$project_dir,
                                         "/html_report/",
                                         Sys.Date(), "_", master_list$project_details$user_name, "_",
                                         master_list$project_details$project_name,"_",master_list$project_details$plateID,
                                         "_lipidExploreR_qcCheckeR_report_v5.html")
  )

  ### 3. browse template
  browseURL(url = paste0(master_list$project_details$project_dir,
                         "/html_report/",
                         Sys.Date(), "_", master_list$project_details$user_name, "_",
                         master_list$project_details$project_name,"_",master_list$project_details$plateID,
                         "_lipidExploreR_qcCheckeR_report_v5.html")
  )

  return(master_list)
}



#' Export Master List RDA
#'
#' This function exports the `master_list` data to an RDA file.
#'
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with the exported RDA file.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_export_master_list_rda(master_list)
#' }
qcCheckR_export_master_list_rda <-  function(master_list){
  ## 5.3. rda file of master list

  #clean environment
  rm(list = c(ls()[which(ls() != "master_list")]))
  # Update script_log
  invisible(capture.output(master_list$environment$user_functions$update_script_log(master_list, "data_exports", "plot_generation", "Script Complete \n\n\n Thank you for choosing Lipid Explore R")))

  ### 5.3.a. export rda of master_list

  save(master_list,
       file = paste0(
         master_list$project_details$project_dir,
         "/data/rda/", Sys.Date(), "_", master_list$project_details$user_name, "_",
         master_list$project_details$project_name,
         "_",
         master_list$project_details$plateID,
         "_qcCheckeR_v4.rda"))

  master_list$environment$user_functions$update_script_log(master_list, "data_exports", "plot_generation", "Script Complete \n\n\n Thank you for choosing Lipid Explore R")

  return(master_list)
}
