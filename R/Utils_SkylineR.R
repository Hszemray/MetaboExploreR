#SkylineR Internal Helper Functions ----

#Function for project setup
setup_project <- function(project_directory, plateID) {

  #set wiff file paths dynamically from project file wiff
  wiff_file_paths <- list.files(path = paste0(project_directory,"/wiff"), pattern = ".wiff$", all.files = FALSE,
                                full.names = TRUE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #set plateIDs
  plateIDs <- str_remove(str_extract(wiff_file_paths, "[^/]+$"), "\\.wiff$")

  ##set up project master list
  master_list <- list(); master_list$environment <- list(); master_list$environment$user_functions <- list(); master_list$templates <- list(); master_list$templates$mrm_guides <- list(); master_list$project_details <- list();    master_list$data <- list(); master_list$data$mzR <- list(); master_list$summary_tables <- list(); master_list$process_lists <- list()
  #Start process counter
  start_time <- Sys.time()
  ##store environment details
  master_list$environment$r_version <- sessionInfo()$R.version$version.string
  master_list$environment$base_packages <- sessionInfo()$basePkgs
  master_list$environment$user_packages <- paste0(names(sessionInfo()$otherPkgs), ": ", paste0(installed.packages()[names(sessionInfo()$otherPkgs), "Version"]))

  #set project directory
  master_list$project_details$project_dir <- project_directory
  #set lipidExploreR version
  master_list$project_details$lipidExploreR_version <- "Automated 1.0.0"
  #set user
  master_list$project_details$user_name <- "Australian National Phenome Centre"
  #set project name
  master_list$project_details$project_name <- stringr::str_extract(master_list$project_details$project_dir, "[^/]*$")
  #set wiff file paths from project file wiff
  master_list$project_details$wiff_file_paths <- list.files(path = paste0(master_list$project_details$project_dir,"/wiff"), pattern = paste0(plateID,".wiff$"), all.files = FALSE,
                                                            full.names = TRUE, recursive = FALSE,
                                                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #set plateIDs
  master_list$project_details$plateID <- plateID

  #set script log
  master_list$project_details$script_log$timestamps$start_time <- start_time;rm(start_time)

  ##read in mrm_guide
  versions <- c("v1", "v2", "v3", "v4")
  for (version in versions) {
    master_list$templates$mrm_guides[[version]]$mrm_guide <- read_csv(
      system.file("templates", paste0("LGW_lipid_mrm_template_",version,".csv"), package = "MetaboExploreR"),
      show_col_types = FALSE
    )
  }

  #setup project directories
  for (plate_ID in master_list$project_details$plateID){
    #plate
    if(!dir.exists(paste0(master_list$project_details$project_dir, "/",plate_ID))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID))}
    #data
    if(!dir.exists(paste0(master_list$project_details$project_dir, "/",plate_ID,"/data"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data"))}
    #mzml
    if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/mzml"))){dir.create(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/mzml"))}
    #rda
    if(!dir.exists(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/rda"))){dir.create(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/rda"))}
    #skyline
    if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/skyline"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/skyline"))}
    #sciex
    if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/sciex_raw"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/sciex_raw"))}
    #batch_correct
    if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/batch_correction"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/batch_correction"))}
    #html_reports
    if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/html_report"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/html_report"))}
  }

  #update script log
  master_list <- update_script_log(master_list, "project_setup", "start_time", "ms_convert")

}#End of setup_project function

#msConvert No-UI Interaction
mzml_conversion <- function(plateID, master_list) {

  #setwd to project directory
  setwd(master_list$project_details$project_dir)
  #Construct command for terminal
  # replace all "/" with "\\"
  file_paths <- gsub("/", "\\\\", master_list$project_details$wiff_file_paths)
  # define the base command
  base_command <- "\"C:\\Program Files\\ProteoWizard\\ProteoWizard 3.0.25015.b6222f2\\msconvert.exe\" --zlib --filter \"titleMaker <RunId>.<ScanNumber>.<ScanNumber>.<ChargeState> File:\\\"<SourcePath>\\\", NativeID:\\\"<Id>\\\"\""
  # define the output directory
  output_dir <- gsub("/", "\\\\", master_list$project_details$project_dir)%>% paste0(.,"\\msConvert_mzml_output")
  # quote each file path
  quoted_file_paths <- sapply(file_paths, shQuote)
  # construct the full command
  full_command <- paste(
    base_command,
    paste(quoted_file_paths, collapse = " "),
    "--outdir", shQuote(output_dir)
  )
  # execute the command
  system(full_command)

  #restructure directory
  #move wiff files into correct location
  temp_path_1 <- list.files(path = file.path(master_list$project_details$project_dir,"wiff"), pattern = ".wiff$", all.files = FALSE,
                            full.names = TRUE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

  temp_path_2 <- list.files(path = file.path(master_list$project_details$project_dir,"wiff"), pattern = ".wiff.scan$", all.files = FALSE,
                            full.names = TRUE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

  #Move .wiff to correct locations
  path1 <- str_subset(string = temp_path_1, pattern = paste0(plateID))
  file.copy(from = paste0(path1), to = paste0(master_list$project_details$project_dir,"/",plateID, "/data/sciex_raw" ))
  #Move .wiff.scan
  path2 <- str_subset(string = temp_path_2, pattern = paste0(plateID))
  file.copy(from = paste0(path2), to = paste0(master_list$project_details$project_dir,"/",plateID, "/data/sciex_raw" ))


  # Move mzml files to correct locations
  master_list$project_details$mzml_file_paths <- list.files(path = file.path(master_list$project_details$project_dir, "msConvert_mzml_output"),pattern = ".mzML$",
                                                            all.files = FALSE,full.names = TRUE,recursive = FALSE,ignore.case = FALSE,include.dirs = FALSE,
                                                            no.. = FALSE)
  # Exclude files containing "COND" or "Blank" or "ISTDs"
  master_list$project_details$mzml_file_paths <- master_list$project_details$mzml_file_paths[!grepl("COND|Blank|ISTDs", master_list$project_details$mzml_file_paths)]

  temp_paths <- master_list$project_details$mzml_file_paths
  project_dir <- master_list$project_details$project_dir

  path <- str_subset(string = temp_paths, pattern = plateID)
  destination <- file.path(project_dir, plateID, "data", "mzml")
  file.copy(from = path, to = destination)

  #update script log
  master_list <- update_script_log(master_list, "msconvert", "project_setup", "mzR_mzml_import")
}#End of mzml_conversion function

# RT findeR
mzR_mrm_findR <- function(FUNC_mzR, #list from master_list containing $mzR object for each sample; $mzR_header; $mzR_chromatogram
                          FUNC_mrm_guide, #tibble of mrm details
                          FUNC_OPTION_qc_type #qc_type used in the experiment LTR; PQC; none

){

  #create output list
  FUNC_output <- list()

  #list mzR objects
  mzML_filelist <- NULL
  for(idx_plate in names(FUNC_mzR)){
    mzML_filelist <- c(mzML_filelist, names(FUNC_mzR[[idx_plate]]))
  }

  #list mzR objects matching qc type
  mzML_filelist_qc <- mzML_filelist[grep(FUNC_OPTION_qc_type, mzML_filelist)]

  # PROCESS: Find peak apex and boundaries using QC mzR (mzML) data

  #set output files
  FUNC_tibble <- list()

  #loop to perform individually for each file
  for(idx_mzML in mzML_filelist_qc){
    FUNC_tibble[[idx_mzML]] <- tibble()

    #find file (data in master list stored in plate sublists)
    for(idx_plate in names(FUNC_mzR)){
      if(length(grep(idx_mzML, names(FUNC_mzR[[idx_plate]]))) == 1){

        #loop to perform individually for each mrm transition
        for(idx_mrm in 3:nrow(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header)){

          #run if statement - only perform on transitions containing data
          if(nrow(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]) > 0){

            #store precursor and product information
            precursor_mz <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$precursorIsolationWindowTargetMZ[idx_mrm]
            product_mz <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$productIsolationWindowTargetMZ[idx_mrm]


            # find baseline of transition window
            baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2] %>%
              median()

            #if mrm transition contains lots of 0 median will = 0 so use mean
            if(baseline_value < 1){
              baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2] %>%
                mean()
            }

            #find scan indeces

            #find scan index of max intensity within mrm channel
            peak_apex_idx <- which.max(
              FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]
            )

            #re-calculate and re-centre if in wings of window
            if(peak_apex_idx < 5 |
               peak_apex_idx > (length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])-5)){
              peak_apex_idx <-  which.max(
                (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])[5:(length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])-5)]
              )+4
            }

            # find scan index of scans below baseline noise
            baseline_idx <- which(
              (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]) < baseline_value
            )

            # identify scan index where peak returns to baseline after peak apex
            peak_start_idx <- baseline_idx[which(baseline_idx < peak_apex_idx)][(length(which(baseline_idx < peak_apex_idx)))-3]

            #if peak_start_idx is beyond idx range set at 1.
            if(length(peak_start_idx) == 0){
              peak_start_idx <- 1
            }
            if(peak_start_idx < 1){
              peak_start_idx <- 1
            }

            # identify scan index where peak returns to baseline after peak apex
            peak_end_idx <- baseline_idx[which(baseline_idx > peak_apex_idx)][3]

            #if peak_start_idx is beyond idx range set at max length of scan index
            if(length(peak_end_idx) == 0){
              peak_end_idx <- length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])
            }
            if(peak_end_idx > length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]) |
               is.na(peak_end_idx)==TRUE){
              peak_end_idx <- length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])
            }


            #find retention times according to scan indices
            #1. find rt of peak_apex
            mzml_rt_apex <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time[peak_apex_idx] %>% round(2)

            #2. find rt of peak start time
            mzml_rt_start <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time[peak_start_idx] %>% round(2)

            #3. find rt of peak end time
            mzml_rt_end <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time[peak_end_idx] %>% round(2)


            #find target lipid name and family from mrm_guide
            lipid_idx <- which(
              FUNC_mrm_guide$precursor_mz == precursor_mz &
                FUNC_mrm_guide$product_mz == product_mz
            )

            #relax tolerances if first match fails or gets multiple hits - e.g. isomer with same MRM transition, introduces RT thresholds
            if(length(lipid_idx) != 1){
              lipid_idx <- which(
                FUNC_mrm_guide$precursor_mz > (precursor_mz - 0.25) &
                  FUNC_mrm_guide$precursor_mz < (precursor_mz + 0.25) &
                  FUNC_mrm_guide$product_mz > (product_mz - 0.25) &
                  FUNC_mrm_guide$product_mz < (product_mz + 0.25) &
                  FUNC_mrm_guide$explicit_retention_time > (min(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time)-0.1)&
                  FUNC_mrm_guide$explicit_retention_time < (max(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time)+0.1)
              )
            }

            if(length(lipid_idx) > 1){
              #only print multiple matches for first file
              if(idx_mzML == mzML_filelist_qc[1]){print(paste("multiple matches for precursor =", precursor_mz, "; product =", product_mz, "; retention time =", mzml_rt_apex, "; transitions", paste(lipid_idx, collapse = " ")))}
              lipid_class <- "multiple match"
              lipid <- "multiple match"
            }

            if(length(lipid_idx) == 0){
              #only print multiple matches for first file
              if(idx_mzML == mzML_filelist_qc[1]){print(paste("no match for precursor =", precursor_mz, "; product =", product_mz, "; retention time =", mzml_rt_apex, "; transitions"))}
              lipid_class <- "no match"
              lipid <- "no match"
            }

            if(length(lipid_idx) == 1){
              lipid_class <- FUNC_mrm_guide$molecule_list_name[lipid_idx]
              lipid <- FUNC_mrm_guide$precursor_name[lipid_idx]
            }



            FUNC_tibble[[idx_mzML]] <- FUNC_tibble[[idx_mzML]] %>%
              bind_rows(
                bind_cols("mzml" = idx_mzML,
                          "lipid_class" = lipid_class,
                          "lipid" = lipid,
                          "precursor_mz" = precursor_mz,
                          "product_mz" = product_mz,
                          "peak_apex" = mzml_rt_apex,
                          "peak_start" = mzml_rt_start,
                          "peak_end" = mzml_rt_end)
              )
          }
        }
      }
    }
  }

  #create master list
  FUNC_tibble <- bind_rows(FUNC_tibble) %>%
    filter(lipid_class != "no match") %>%
    filter(lipid_class != "multiple match")


  FUNC_output$mrm_guide_updated <- tibble()
  FUNC_output$peak_boundary_update <- tibble()

  for(idx_lipid in unique(FUNC_tibble$lipid)){

    FUNC_output$mrm_guide_updated <- bind_rows(FUNC_output$mrm_guide_updated,
                                               bind_cols(
                                                 "Molecule List Name" = (FUNC_tibble %>% filter(lipid == idx_lipid))[["lipid_class"]] %>% unique(),
                                                 "Precursor Name" = idx_lipid,
                                                 "Precursor Mz" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["precursor_mz"]],
                                                 "Precursor Charge" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["precursor_charge"]],
                                                 "Product Mz" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["product_mz"]],
                                                 "Product Charge" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["product_charge"]],
                                                 "Explicit Retention Time" = (FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_apex"]] %>% median(),
                                                 "Explicit Retention Time Window" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["explicit_retention_time_window"]],
                                                 "Note" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["note"]]
                                               ))


    FUNC_output$peak_boundary_update <- bind_rows(FUNC_output$peak_boundary_update,
                                                  bind_cols(
                                                    "FileName" = mzML_filelist,
                                                    "FullPeptideName" =  rep(idx_lipid, length(mzML_filelist)),
                                                    "MinStartTime" = rep(
                                                      ((FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_start"]] %>% summary())[["1st Qu."]],
                                                      length(mzML_filelist)
                                                    ),
                                                    "MaxEndTime" =  rep(
                                                      ((FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_end"]] %>% summary())[["3rd Qu."]],
                                                      length(mzML_filelist)
                                                    )
                                                  )
    )


  }

  FUNC_output$mrm_guide_updated <-  FUNC_output$mrm_guide_updated %>% dplyr::arrange(`Precursor Name`)

  FUNC_output$peak_boundary_update <- FUNC_output$peak_boundary_update %>% arrange(`FullPeptideName`)

  FUNC_output

}#End of mzR_mrm_findR function

# import mzml files using mzR
import_mzml <- function(plateID, master_list) {
  #initialise mzml_filelist
  mzml_filelist <- list()
  #Initialise loop to go over each plate
  for(idx_plate in master_list$project_details$plateID){
    #Generate list of mzml files for the plate
    mzml_filelist[[idx_plate]] <- list.files(file.path(master_list$project_details$project_dir,
                                                       idx_plate,
                                                       "data/mzml"),
                                             pattern = ".mzML",
                                             full.names = FALSE)

    # Exclude files containing "COND" or "Blank" or "ISTDs"
    mzml_filelist[[idx_plate]] <- mzml_filelist[[idx_plate]][!grepl("COND|Blank|ISTDs", mzml_filelist[[idx_plate]])]

    master_list$data[[idx_plate]]$mzR <- list()
    #read in mzML files using mzR
    for(idx_mzML in mzml_filelist[[idx_plate]]){
      master_list$data[[idx_plate]]$mzR[[idx_mzML]] <- list()
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object <- mzR::openMSfile(
        filename = paste0(master_list$project_details$project_dir,
                          "/",
                          idx_plate,
                          "/data/mzml/", idx_mzML))
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_header <- mzR::chromatogramHeader(master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object)
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_chromatogram <- mzR::chromatograms(master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object)
      master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_timestamp <- master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object@backend$getRunStartTimeStamp()
    }
    # Extract single timestamp and add to global_timestamp
    timestamp <- master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_timestamp
    master_list$data$global_timestamp[[idx_plate]] <- timestamp %>%
      str_sub(., 1, 4) %>%
      as.numeric()

    #Store sample lists in project details
    if (is.null(master_list$project_details$mzml_sample_list[[idx_plate]])) {
      master_list$project_details$mzml_sample_list[[idx_plate]] <- character()
    }

    master_list$project_details$mzml_sample_list[[idx_plate]] <- c(master_list$project_details$mzml_sample_list[[idx_plate]], names(master_list$data[[idx_plate]]$mzR))
  }

  #update script log
  master_list <- update_script_log(master_list, "mzR_mzml_import", "ms_convert", "mzml_file_processing")
}#End of import_mzml function

#peak_picking with skyline
peak_picking <- function(plateID, master_list) {
  #initialise loop per plate
  for (plate_idx in master_list$project_details$plateID){
    print(paste("Processing plate:",plate_idx))

    #parameters method version control
    # Check and set qc_type
    if (any(grepl("LTR", master_list$project_details$mzml_sample_list[[plate_idx]]))) {
      master_list$project_details$qc_type <- "LTR"
    } else if (any(grepl("PQC", master_list$project_details$mzml_sample_list[[plate_idx]]))) {
      master_list$project_details$qc_type <- "PQC"
    } else {
      print("mzml files contain neither LTR or PQC. SkylineR is unable to process your data.")
    }

    #set SIL internal standard version
    # Logic for template selection
    plate_timestamp <- master_list$data$global_timestamp[[plate_idx]]
    versions_to_try  <- if(plate_timestamp < 2023){
      c("v1","v2","v3","v4")
    } else if(plate_timestamp >= 2023 & plate_timestamp < 2025){
      c("v2","v1","v3","v4")
    } else if(plate_timestamp >= 2025){
      c("v4","v3","v2","v1")
    }

    #Set sil_found bool
    sil_found <- FALSE

    for (is_ver_current in versions_to_try) {
      if (sil_found) break

      print(paste("Trying SIL version:", is_ver_current))

      # Set current template version
      master_list$project_details$is_ver <- is_ver_current


      #create summary table for report
      Temp_list <- master_list$project_details[c("project_dir", "lipidExploreR_version", "user_name", "project_name", "plateID", "qc_type", "is_ver")]
      Temp_list$plateID <- paste(plate_idx)
      master_list$summary_tables$project_summary <- tibble(unlist(Temp_list)) %>%
        add_column("Project detail" = c(
          "local directory", "lipidExploreR version", "user initials", "project name", "plateID", "project qc type", "int. std. version"),
          .before = 1)

      master_list$summary_tables$project_summary <- setNames(master_list$summary_tables$project_summary, c("Project detail", "value"))

      #retention time optimiser
      #run function



      master_list$templates$mrm_guides$by_plate[[plate_idx]] <- list()
      for (idx in plate_idx) {
        result <- mzR_mrm_findR(
          FUNC_mzR = master_list$data[[idx]],
          FUNC_mrm_guide = master_list$templates$mrm_guides[[master_list$project_details$is_ver]]$mrm_guide %>% clean_names(),
          FUNC_OPTION_qc_type = master_list$project_details$qc_type
        ) %>% append(master_list$templates$mrm_guides[[master_list$project_details$is_ver]])

        master_list$templates$mrm_guides$by_plate[[plate_idx]] <- result
      }


      #set names from original template so that skyline recognises columns
      master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated <- setNames(master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated,
                                                                                           names(master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide))


      #export updated optimised RT times
      write_csv(x = master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated,
                file = paste0(master_list$project_details$project_dir,"/", plate_idx, "/data/skyline/", Sys.Date(), "_RT_update_",
                              master_list$project_details$project_name,"_",plate_idx, ".csv"))

      #export peak boundary output
      write_csv(x = master_list$templates$mrm_guides$by_plate[[plate_idx]]$peak_boundary_update,
                file = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_peak_boundary_update_",
                              master_list$project_details$project_name,"_",plate_idx, ".csv"))

      ##4. skyline command

      #Import blank files into structure
      ##Blank .sky file
      file.copy(from = system.file("templates", "default_skyline_file.sky", package = "MetaboExploreR"),
                to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
      file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_skyline_file.sky"),
                  to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_",master_list$project_details$project_name,"_",plate_idx,".sky"))
      ##Blank .csv file for report
      file.copy(from = system.file("templates", "default_csv.csv", package = "MetaboExploreR"),
                to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
      file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_csv.csv"),
                  to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_xskylineR_1_",master_list$project_details$project_name,"_",plate_idx,".csv"))
      ##Blank .tsv for chromatograms
      file.copy(from =system.file("templates", "default_tsv.tsv", package = "MetaboExploreR"),
                to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
      file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_tsv.tsv"),
                  to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_",master_list$project_details$project_name,"_",plate_idx,"_chromatograms.tsv"))
      ##Report template
      file.copy(from = paste0system.file("templates", "YYYY-MM-DD_xskylineR_1_project_name.skyr", package = "MetaboExploreR"),
                  to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))

      # Construct system command dynamically
      ## Define the paths and arguments separately
      exe_path <- shQuote("C:/Program Files/Skyline/SkylineCmd.exe")
      in_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
      import_transition_list <- shQuote(paste0("skyline/", Sys.Date(), "_RT_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
      import_peak_boundaries <- shQuote(paste0("skyline/", Sys.Date(), "_peak_boundary_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
      out_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
      report_file <- shQuote(paste0("skyline/", Sys.Date(), "_xskylineR_1_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
      chromatogram_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, "_chromatograms.tsv"))
      report_template <- shQuote("skyline/YYYY-MM-DD_xskylineR_1_project_name.skyr")
      mzml_path <- shQuote("mzml")

      #set working directory
      setwd(paste0(master_list$project_details$project_dir,"/",plate_idx,"/data"))
      ## Combine the arguments into a single command with a space separator
      cmd <- paste(
        exe_path,
        paste0('--in=', in_file),
        paste0('--import-transition-list=', import_transition_list),
        paste0('--import-all=', mzml_path),
        paste0('--import-peak-boundaries=', import_peak_boundaries),
        paste0('--save-settings'),
        paste0('--overwrite'),
        paste0('--out=', out_file),
        paste0('--report-conflict-resolution=overwrite'),
        paste0('--report-name=YYYY-MM-DD_xskylineR_1_project_name'),
        paste0('--report-file=', report_file),
        paste0('--report-add=', report_template),
        paste0('--report-format=csv'),
        paste0('--report-invariant'),
        paste0('--chromatogram-file=', chromatogram_file),
        paste0('--chromatogram-precursors'),
        paste0('--chromatogram-products'),
        paste0('--chromatogram-base-peaks'),
        paste0('--chromatogram-tics'),
        sep = " "
      )

      # execute the command
      system(cmd)

      # re-import skyline file
      skyline_data <- read.csv(
        file = list.files(
          paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline"),
          pattern = paste0("xskylineR_1_", master_list$project_details$project_name),
          full.names = TRUE))
      cols_to_convert <- c("PrecursorMz", "ProductMz", "RetentionTime", "StartTime", "EndTime", "Area", "Height")
      suppressWarnings(skyline_data[cols_to_convert] <- lapply(skyline_data[cols_to_convert], as.numeric))
      skyline_data <- janitor::clean_names(skyline_data)
      master_list$data$skyline_report[[plate_idx]] <- skyline_data

      # Check for SIL internal standards
      sil_on_plate <- master_list$data$skyline_report[[plate_idx]][grepl("SIL", master_list$data$skyline_report[[plate_idx]][["molecule_name"]], ignore.case = TRUE),] %>% select(contains("molecule_name"))%>% unique() %>% rename("SIL" = "molecule_name")
      sil_for_version <- master_list$templates$mrm_guides[[is_ver_current]][["mrm_guide"]][grepl("SIL|sil", master_list$templates$mrm_guides[[is_ver_current]][["mrm_guide"]][["Precursor Name"]], ignore.case = TRUE),] %>% select(contains("Precursor Name"))%>% unique() %>% rename("SIL" = "Precursor Name")
      common_SIL <- intersect(sil_on_plate,sil_for_version)
      sil_found <- ifelse(length(common_SIL) >= (length(sil_for_version)*0.85), TRUE,FALSE )

      if (sil_found) {
        print(paste("Successfully found SIL standards using version:", is_ver_current))

        #export .rda for individual plate
        save(master_list, file = paste0(
          master_list$project_details$project_dir, "/", plate_idx,"/data/rda/",
          Sys.Date(), "_", master_list$project_details$user_name, "_", master_list$project_details$project_name, "_", plate_idx,
          "_skylineR.rda"))

      } else {
        print(paste("No SIL standards detected with version", is_ver_current, "- trying next version"))
      }
    }

    if (!sil_found) {
      print(paste("No SIL internal standards detected in plate", plate_idx, "after trying all method versions. Moving to the next plate."))
    }

    #reset working directory
    setwd(master_list$project_details$project_dir)

    #update script log
    master_list <- update_script_log(master_list, "mzml_file_processing", "mzR_mzml_import", "Next plate")

    #Clean environment for next plate
    rm(list = ls()[!ls() %in% c("project_directory", "wiff_file_paths", "plateIDs")])

  }#Close of plate loop
}#End of peak_picking function

archive_raw_files <- function(project_directory) {
  #Alter directory
  ##wiff files
  move_folder(file.path(project_directory,"wiff"), file.path(project_directory,"archive"))
  ##mzml files
  move_folder(file.path(project_directory,"msConvert_mzml_output"), file.path(project_directory,"archive"))

  message("Skyline R is now finished running all plates :)")
}#End of archive_raw_files
