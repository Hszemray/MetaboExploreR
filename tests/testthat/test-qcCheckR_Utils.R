library(mockery)
library(dplyr)
library(tidyr)

#Tests for setup_project function ----
test_that("qcCheckR_setup_project returns a master_list with expected structure", {
  # Create dummy inputs
  user_name <- "test_user"
  project_directory <- "dummy/path"
  mrm_template_list <- list(list(guide = "dummy"))
  QC_sample_label <- "QC"
  sample_tags <- c("SampleA", "SampleB")
  mv_threshold <- 50

  # Create dummy master_list
  dummy_master_list <- list(initialised = TRUE)

  # Mock all dependencies
  stub(qcCheckR_setup_project, "validate_project_directory", NULL)
  stub(qcCheckR_setup_project, "initialise_master_list", dummy_master_list)
  stub(qcCheckR_setup_project, "store_environment_details", function(x) x)
  stub(qcCheckR_setup_project, "qcCheckR_set_project_details", function(x, ...) x)
  stub(qcCheckR_setup_project, "qcCheckR_read_mrm_guides", function(x, ...) x)
  stub(qcCheckR_setup_project, "qcCheckR_setup_project_directories", function(x) NULL)
  stub(qcCheckR_setup_project, "qcCheckR_import_skyline_reports", function(x) x)
  stub(qcCheckR_setup_project, "find_method_version", function(x) x)
  stub(qcCheckR_setup_project, "update_script_log", function(x, ...) x)

  # Run the function
  result <- qcCheckR_setup_project(user_name, project_directory, mrm_template_list,
                                   QC_sample_label, sample_tags, mv_threshold)

  # Expectations
  expect_type(result, "list")
  expect_true(result$initialised)
})

test_that("qcCheckR_set_project_details correctly updates master_list", {
  # Dummy master_list structure
  master_list <- list(project_details = list(script_log = list(timestamps = list())))

  # Inputs
  user_name <- "test_user"
  project_directory <- "some/path/projectX"
  QC_sample_label <- "QC"
  sample_tags <- c("sample1", "sample2")
  mv_threshold <- 40

  # Run function
  result <- qcCheckR_set_project_details(master_list, user_name, project_directory,
                                         QC_sample_label, sample_tags, mv_threshold)

  # Expectations
  expect_equal(result$project_details$project_dir, project_directory)
  expect_equal(result$project_details$user_name, user_name)
  expect_equal(result$project_details$project_name, "projectX")
  expect_equal(result$project_details$qc_type, QC_sample_label)
  expect_equal(result$project_details$mv_sample_threshold, mv_threshold)
  expect_equal(result$project_details$sample_tags, sample_tags)
  expect_type(result$project_details$script_log$timestamps$start_time, "double") # Sys.time returns POSIXct (double)
})

test_that("qcCheckR_set_project_details sets default sample_tags for ANPC", {
  master_list <- list(project_details = list(user_name = "ANPC", script_log = list(timestamps = list())))

  result <- qcCheckR_set_project_details(master_list, "ANPC", "dir", "QC", NULL, 50)

  expect_equal(result$project_details$sample_tags,
               c("pqc", "qc", "vltr", "sltr", "ltr", "blank", "istds", "cond", "sample"))
})


test_that("qcCheckR_read_mrm_guides calls validation and loads user-supplied guides", {
  master_list <- list(project_details = list(user_name = "test_user"))

  mrm_template_list <- list(
    v1 = list(SIL_guide = "path/to/sil.tsv", conc_guide = "path/to/conc.tsv")
  )

  # Mock read_tsv to return dummy tibble
  stub(qcCheckR_read_mrm_guides, "read_tsv", function(path) tibble::tibble(file = path))

  # Mock validation function
  validation_mock <- mockery::mock(master_list)
  stub(qcCheckR_read_mrm_guides, "validate_qcCheckR_mrm_template_list", validation_mock)

  result <- qcCheckR_read_mrm_guides(master_list, mrm_template_list)

  # Check guides loaded
  expect_equal(result$templates$mrm_guides$v1$SIL_guide$file, "path/to/sil.tsv")
  expect_equal(result$templates$mrm_guides$v1$conc_guide$file, "path/to/conc.tsv")

  # Check validation was called
  mockery::expect_called(validation_mock, 1)
})

test_that("qcCheckR_setup_project_directories does nothing if directories already exist", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))

  # Mock dir.exists to always return TRUE
  stub(qcCheckR_setup_project_directories, "dir.exists", function(...) TRUE)
  stub(qcCheckR_setup_project_directories, "dir.create", function(...) stop("Should not be called"))

  expect_message(qcCheckR_setup_project_directories(master_list),
                 "qcCheckR directory set up at: dummy/project/all")
})

test_that("qcCheckR_setup_project_directories creates directories if they do not exist", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))

  # Simulate dir.exists returning FALSE initially, then TRUE after creation
  exists_calls <- c(FALSE, FALSE, FALSE, FALSE, TRUE)
  exists_mock <- mockery::mock(FALSE, FALSE, FALSE, FALSE, TRUE)
  create_mock <- mockery::mock(TRUE, cycle = TRUE)

  stub(qcCheckR_setup_project_directories, "dir.exists", exists_mock)
  stub(qcCheckR_setup_project_directories, "dir.create", create_mock)

  expect_message(qcCheckR_setup_project_directories(master_list),
                 "qcCheckR directory set up at: dummy/project/all")

  # Check that dir.create was called 4 times (main + 3 subdirs)
  mockery::expect_called(create_mock, 4)
})

test_that("qcCheckR_setup_project_directories throws error if directory creation fails", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))

  # Simulate dir.exists always returning FALSE
  stub(qcCheckR_setup_project_directories, "dir.exists", function(...) FALSE)
  stub(qcCheckR_setup_project_directories, "dir.create", function(...) TRUE)

  expect_error(qcCheckR_setup_project_directories(master_list),
               "Failed to create qcCheckR directory at: dummy/project/all")
})

test_that("qcCheckR_import_skyline_reports stops if no files found", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))
  stub(qcCheckR_import_skyline_reports, "list.files", function(...) character(0))

  expect_error(qcCheckR_import_skyline_reports(master_list),
               "No report files found in the specified project directory")
})

test_that("qcCheckR_import_skyline_reports reads and stores CSV and TSV files", {
  master_list <- list(
    project_details = list(project_dir = "dummy/project", plateIDs = character()),
    data = list(skylineReport = list())
  )

  files <- c("dummy/project/plate1_xskylineR_1_report.csv", "dummy/project/plate2_xskylineR_1_report.tsv")
  stub(qcCheckR_import_skyline_reports, "list.files", function(...) files)
  stub(qcCheckR_import_skyline_reports, "dir.exists", function(...) TRUE)
  stub(qcCheckR_import_skyline_reports, "read.csv", function(...) data.frame(FileName = "plate1_xskylineR_1_report.csv"))
  stub(qcCheckR_import_skyline_reports, "read.delim", function(...) data.frame(FileName = "plate2_xskylineR_1_report.tsv"))

  result <- qcCheckR_import_skyline_reports(master_list)

  expect_named(result$data$skylineReport, c("plate1_report", "plate2_report"))
  expect_equal(result$project_details$plateIDs, c("plate1_report", "plate2_report"))
})

test_that("qcCheckR_import_skyline_reports reads and stores CSV and TSV files", {
  master_list <- list(
    project_details = list(project_dir = "dummy/project", plateIDs = character()),
    data = list(skylineReport = list())
  )

  files <- c("dummy/project/plate1_xskylineR_1_report.csv", "dummy/project/plate2_xskylineR_1_report.tsv")
  stub(qcCheckR_import_skyline_reports, "list.files", function(...) files)
  stub(qcCheckR_import_skyline_reports, "read.csv", function(...) data.frame(FileName = "file1"))
  stub(qcCheckR_import_skyline_reports, "read.delim", function(...) data.frame(FileName = "file2"))

  result <- qcCheckR_import_skyline_reports(master_list)

  expect_named(result$data$skylineReport, c("plate1_report", "plate2_report"))
  expect_equal(result$project_details$plateIDs, c("plate1_report", "plate2_report"))
})

test_that("find_method_version assigns correct version when all SILs match", {
  suppressMessages({
  master_list <- list(
    data = list(skylineReport = list(
      plate1 = data.frame(MoleculeName = c("SIL_A", "SIL_B"),check.names = FALSE)
    )),
    templates = list(mrm_guides = list(
      v1 = list(SIL_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B", "Other"), check.names = FALSE))
    )),
    project_details = list(plateIDs = "plate1", plate_method_versions = list())
  )

  result <- find_method_version(master_list)

  expect_equal(result$project_details$plate_method_versions[["plate1"]], "v1")
 })
})

test_that("find_method_version warns when some plates are unmatched", {
  suppressMessages({
  master_list <- list(
    data = list(skylineReport = list(
      plate1 = data.frame(MoleculeName = c("SIL_A", "SIL_B"), check.names = FALSE),
      plate2 = data.frame(MoleculeName = c("SIL_X"), check.names = FALSE)
    )),
    templates = list(mrm_guides = list(
      v1 = list(SIL_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B"), check.names = FALSE))
    )),
    project_details = list(plateIDs = c("plate1", "plate2"), plate_method_versions = list())
  )

  expect_message(find_method_version(master_list), "No method version found for the following plates: plate2")
  })
})

test_that("find_method_version stops when no plate matches any guide", {
  master_list <- list(
    data = list(skylineReport = list(
      plate1 = data.frame(MoleculeName = c("SIL_X", "SIL_Y"), check.names = FALSE)
    )),
    templates = list(mrm_guides = list(
      v1 = list(SIL_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B"), check.names = FALSE))
    )),
    project_details = list(plateIDs = "plate1", plate_method_versions = list())
  )

  expect_error(find_method_version(master_list), "No method version found for the plates")
})

#Tests for phase 1 data prep----

##Transpose tests----
test_that("qcCheckR_transpose_data successfully transposes a matching plate", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(skylineReport = list(plate1 = data.frame())),
    data = list(peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) "plate1")
  stub(qcCheckR_transpose_data, "transpose_plate_data", function(x) data.frame(transposed = TRUE))

  result <- qcCheckR_transpose_data(master_list)

  expect_true("plate1" %in% names(result$data$peakArea$transposed))
  expect_equal(result$data$peakArea$transposed[["plate1"]]$transposed, TRUE)
  })
})

test_that("qcCheckR_transpose_data handles no matching report", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(skylineReport = list(plate1 = data.frame())),
    data = list(peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) character(0))

  expect_message(qcCheckR_transpose_data(master_list), "No matching skyline report found for plate: plate1")
  })
})

test_that("qcCheckR_transpose_data handles multiple matching reports", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(skylineReport = list(plate1 = data.frame())),
    data = list(peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) c("plate1", "plate1_dup"))

  expect_message(qcCheckR_transpose_data(master_list), "Multiple matching skyline reports found for plate: plate1")
  })
})

test_that("qcCheckR_transpose_data catches error during transposition", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(skylineReport = list(plate1 = data.frame())),
    data = list(peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) "plate1")
  stub(qcCheckR_transpose_data, "transpose_plate_data", function(x) stop("Simulated error"))

  expect_message(qcCheckR_transpose_data(master_list), "Error transposing plate: plate1")
  expect_message(qcCheckR_transpose_data(master_list), "Error message: Simulated error")
  })
})

test_that("transpose_plate_data returns a tibble with correct structure", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    MoleculeName = c("MolA", "MolA"),
    Area = c("100", "200")
  )

  result <- transpose_plate_data(input)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("sample_name", "MolA"))
  expect_equal(nrow(result), 2)
  expect_equal(result$sample_name, c("sample1", "sample2"))
  expect_type(result$MolA, "double")
})

test_that("transpose_plate_data handles multiple molecules correctly", {
  input <- tibble(
    FileName = rep(c("sample1.mzML", "sample2.mzML"), each = 2),
    MoleculeName = rep(c("MolA", "MolB"), times = 2),
    Area = c("100", "300", "200", "400")
  )

  result <- transpose_plate_data(input)

  expect_named(result, c("sample_name", "MolA", "MolB"))
  expect_equal(result$MolA, c(100, 200))
  expect_equal(result$MolB, c(300, 400))
})

test_that("transpose_plate_data removes .mzML extension from sample names", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    MoleculeName = c("MolA", "MolA"),
    Area = c("100", "200")
  )

  result <- transpose_plate_data(input)

  expect_false(any(grepl("\\.mzML$", result$sample_name)))
})

test_that("transpose_plate_data converts Area to numeric", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    MoleculeName = c("MolA", "MolA"),
    Area = c("100", "not_a_number")
  )

  result <- suppressWarnings(transpose_plate_data(input))

  expect_true(result$MolA[2] == 0 )
  expect_type(result$MolA, "double")
})



##Sort data tests----
test_that("qcCheckR_sort_data populates run_orders and sorted data", {
  # Minimal mock master_list
  master_list <- list(
    project_details = list(
      sample_tags = tibble(),
      run_orders = NULL
    ),
    data = list(
      skylineReport = list(plate1 = tibble()),
      peakArea = list(transposed = list(plate1 = tibble(sample_name = "S1")))
    )
  )

  # Mock dependencies
  local_mocked_bindings(
    extract_run_order = function(report, plate_id) tibble(sample_name = "S1"),
    assign_sample_type = function(tags, run_order) run_order,
    validate_qc_types = function(run_order, tags) NULL,
    sort_and_filter_data = function(run_order, data, tags) tibble(sample_name = "S1"),
    extract_sample_id = function(name) "ID1",
    assess_qc_coverage = function(ml) { ml$qc_checked <- TRUE; ml },
    set_project_qc_type = function(ml) { ml$qc_type <- "TypeA"; ml },
    finalise_sorted_data = function(ml) { ml$finalised <- TRUE; ml }
  )

  result <- qcCheckR_sort_data(master_list)

  expect_true("plate1" %in% names(result$project_details$run_orders))
  expect_true("plate1" %in% names(result$data$peakArea$sorted))
  expect_equal(result$data$peakArea$sorted$plate1$sample_ID, "ID1")
  expect_true(result$qc_checked)
  expect_equal(result$qc_type, "TypeA")
  expect_true(result$finalised)
})

test_that("extract_run_order returns expected columns and structure", {
  input <- tibble(
    FileName = c("sample_SER_01.mzML", "sample_PLA_02.mzML"),
    AcquiredTime = c("2023-01-01 10:00:00", "2023-01-01 09:00:00")
  )

  result <- extract_run_order(input, plate_id = "PlateA")

  expect_named(result, c("sample_name", "sample_timestamp", "sample_plate_id",
                         "sample_plate_order", "sample_matrix"))
  expect_equal(result$sample_name, c("sample_PLA_02", "sample_SER_01"))  # Sorted by time
  expect_equal(result$sample_plate_id, rep("PlateA", 2))
  expect_equal(result$sample_plate_order, 1:2)
  expect_equal(result$sample_matrix, c("PLA", "SER"))
})

test_that("extract_run_order filters out missing timestamps", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    AcquiredTime = c("2023-01-01 10:00:00", NA)
  )

  result <- extract_run_order(input, plate_id = "PlateB")

  expect_equal(nrow(result), 1)
  expect_equal(result$sample_name, "sample1")
})

test_that("extract_run_order handles unknown matrix types", {
  input <- tibble(
    FileName = c("sample_XYZ_01.mzML"),
    AcquiredTime = c("2023-01-01 10:00:00")
  )

  result <- extract_run_order(input, plate_id = "PlateC")

  expect_true(is.na(result$sample_matrix))
})

test_that("assign_sample_type assigns correct types based on sample_tags", {
  sample_tags <- c("QC", "BLANK")
  run_order <- tibble(sample_name = c("QC_01", "BLANK_02", "SAMPLE_03"))

  result <- assign_sample_type(sample_tags, run_order)

  expect_equal(result$sample_type, c("QC", "BLANK", "sample"))
})

test_that("assign_sample_type is case-insensitive", {
  sample_tags <- c("qc")
  run_order <- tibble(sample_name = c("QC_01", "qc_02", "Qc_03", "sample_04"))

  result <- assign_sample_type(sample_tags, run_order)

  expect_equal(result$sample_type, c("qc", "qc", "qc", "sample"))
})

test_that("assign_sample_type throws error with NULL or empty sample_tags", {
  run_order <- tibble(sample_name = c("QC_01"))

  expect_error(assign_sample_type(NULL, run_order),
               "sample_tags must be a character vector with at least one QC type.")
  expect_error(assign_sample_type(character(0), run_order),
               "sample_tags must be a character vector with at least one QC type.")
})

test_that("validate_qc_types passes with valid QC types", {
  run_order <- tibble(sample_type = c("QC", "sample", "BLANK"))
  sample_tags <- c("QC", "BLANK")

  expect_error(validate_qc_types(run_order, sample_tags), NA)
})

test_that("validate_qc_types throws error for invalid QC type", {
  run_order <- tibble(sample_type = c("QC", "INVALID", "sample"))
  sample_tags <- c("QC")

  expect_error(suppressMessages(validate_qc_types(run_order, sample_tags),
               "Invalid QC sample_type detected."))
})

test_that("validate_qc_types throws error when no QC types are present", {
  run_order <- tibble(sample_type = c("sample", "sample"))
  sample_tags <- c("QC")

  expect_error(validate_qc_types(run_order, sample_tags),
               "No QC types were identified.")
})

test_that("sort_and_filter_data joins and sorts by sample_timestamp", {
  run_order <- tibble(
    sample_name = c("S1", "S2"),
    sample_timestamp = as.POSIXct(c("2023-01-02", "2023-01-01"))
  )
  transposed_data <- tibble(
    sample_name = c("S1", "S2"),
    MolA = c(100, 200)
  )
  sample_tags <- c("QC")

  result <- sort_and_filter_data(run_order, transposed_data, sample_tags)

  expect_equal(result$sample_name, c("S2", "S1"))  # Sorted by timestamp
  expect_equal(result$sample_run_index, 1:2)
})

test_that("sort_and_filter_data handles unmatched samples in transposed_data", {
  run_order <- tibble(
    sample_name = c("S1", "S2"),
    sample_timestamp = as.POSIXct(c("2023-01-01", "2023-01-02"))
  )
  transposed_data <- tibble(
    sample_name = c("S1"),
    MolA = c(100)
  )
  sample_tags <- c("QC")

  result <- sort_and_filter_data(run_order, transposed_data, sample_tags)

  expect_true("MolA" %in% names(result))
  expect_true(is.na(result$MolA[2]))  # S2 has no match
})

test_that("sort_and_filter_data assigns correct sample_run_index", {
  run_order <- tibble(
    sample_name = c("S1", "S2", "S3"),
    sample_timestamp = as.POSIXct(c("2023-01-03", "2023-01-01", "2023-01-02"))
  )
  transposed_data <- tibble(
    sample_name = c("S1", "S2", "S3"),
    MolA = c(100, 200, 300)
  )
  sample_tags <- c("QC")

  result <- sort_and_filter_data(run_order, transposed_data, sample_tags)

  expect_equal(result$sample_run_index, c(1:3))
})

test_that("extract_sample_id removes common tokens and retains unique identifiers", {
  filenames <- c("plate1_SER_001.mzML", "plate1_SER_002.mzML", "plate1_SER_003.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(result, c("001", "002", "003"))
})

test_that("extract_sample_id handles mixed delimiters", {
  filenames <- c("plate1-SER.001.mzML", "plate1-SER.002.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(result, c("001", "002"))
})

test_that("extract_sample_id returns full identifier when no common tokens", {
  filenames <- c("A_1.mzML", "B_2.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(result, c("A_1", "B_2"))
})

test_that("extract_sample_id works with a single filename", {
  filenames <- c("plate1_SER_001.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(unlist(result), "plate1_SER_001.mzML")
})

test_that("assess_qc_coverage marks QC as pass when ratio and count are sufficient", {
  master_list <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_type = c(rep("QC", 10), rep("sample", 94))
          )))))

  result <- assess_qc_coverage(master_list)

  expect_equal(result$project_details$qc_passed$plate1$QC, "pass")
  expect_equal(result$project_details$global_qc_pass$QC, "pass")
})

test_that("assess_qc_coverage marks QC as fail when ratio or count is insufficient", {
  master_list <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_type = c("QC", "sample", "sample")
          )))))

  result <- assess_qc_coverage(master_list)

  expect_equal(result$project_details$qc_passed$plate1$QC, "fail")
  expect_equal(result$project_details$global_qc_pass$QC, "fail")
})

test_that("assess_qc_coverage handles multiple QC types correctly", {
  master_list <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_type = c(rep("QC1", 8), rep("QC2", 1), rep("sample", 95))
          )))))

  result <- assess_qc_coverage(master_list)

  expect_equal(result$project_details$qc_passed$plate1$QC1, "pass")
  expect_equal(result$project_details$qc_passed$plate1$QC2, "fail")
  expect_equal(result$project_details$global_qc_pass$QC1, "pass")
  expect_equal(result$project_details$global_qc_pass$QC2, "fail")
})

test_that("set_project_qc_type uses user-specified QC type when it passes", {
  suppressMessages({
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      global_qc_pass = list(qc1 = "pass"),
      qc_passed = list(plate1 = list(qc1 = "pass"))
    )
  )

  result <- set_project_qc_type(master_list)
  expect_equal(result$project_details$qc_type, "qc1")
  })
})

test_that("set_project_qc_type selects alternative QC type when user-specified fails", {
  suppressMessages({
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      global_qc_pass = list(qc1 = "fail", qc2 = "pass"),
      qc_passed = list(plate1 = list(qc2 = "pass"), plate2 = list(qc2 = "pass"))
    )
  )

  result <- suppressWarnings(set_project_qc_type(master_list))
  expect_warning(set_project_qc_type(master_list),
                 "User-specified QC type qc1 did not pass QC checks.")

  expect_equal(result$project_details$qc_type, "qc2")
  })
})

test_that("set_project_qc_type stops when no QC types pass", {
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      global_qc_pass = list(qc1 = "fail"),
      qc_passed = list(plate1 = list(qc1 = "fail"))
    )
  )

  expect_error(
    suppressWarnings(set_project_qc_type(master_list)),
    "No QC types passed. Stopping script")
})

test_that("finalise_sorted_data adds factor columns and standardises sample_type", {
  master_list <- list(
    project_details = list(qc_type = "qc1"),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_name = c("S1", "S2"),
            sample_type = c("qc1", "sample"),
            sample_timestamp = as.POSIXct(c("2023-01-01", "2023-01-02")),
            sample_plate_id = "plate1"
          )))))

  result <- finalise_sorted_data(master_list)
  sorted <- result$data$peakArea$sorted$plate1

  expect_s3_class(sorted$sample_type_factor, "factor")
  expect_equal(levels(sorted$sample_type_factor), c("sample", "qc1", "ltr", "pqc", "vltr", "sltr"))
  expect_equal(as.character(sorted$sample_type), c("qc", "sample"))
  expect_equal(sorted$sample_data_source, rep(".peakArea", 2))
})

test_that("finalise_sorted_data creates reversed factor levels", {
  master_list <- list(
    project_details = list(qc_type = "qc1"),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_name = c("S1", "S2"),
            sample_type = c("qc1", "sample"),
            sample_timestamp = as.POSIXct(c("2023-01-01", "2023-01-02")),
            sample_plate_id = "plate1"
          )))))

  result <- finalise_sorted_data(master_list)
  sorted <- result$data$peakArea$sorted$plate1

  expect_s3_class(sorted$sample_type_factor_rev, "factor")
  expect_equal(levels(sorted$sample_type_factor_rev), rev(levels(sorted$sample_type_factor)))
})

test_that("finalise_sorted_data assigns sample_run_index and splits by plate", {
  master_list <- list(
    project_details = list(qc_type = "qc1"),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_name = "S1",
            sample_type = "qc1",
            sample_timestamp = as.POSIXct("2023-01-01"),
            sample_plate_id = "plate1"
          ),
          plate2 = tibble(
            sample_name = "S2",
            sample_type = "sample",
            sample_timestamp = as.POSIXct("2023-01-02"),
            sample_plate_id = "plate2"
          )))))

  result <- finalise_sorted_data(master_list)

  expect_equal(result$data$peakArea$sorted$plate1$sample_run_index, 1)
  expect_equal(result$data$peakArea$sorted$plate2$sample_run_index, 2)
})


##Impute tests----
test_that("qcCheckR_impute_data populates imputed data for each plate", {
  master_list <- list(
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(sample_name = "S1"),
          plate2 = tibble(sample_name = "S2")
        )
      )
    )
  )

  local_mocked_bindings(
    prepare_imputation_matrix = function(data) matrix(1, nrow = 1, ncol = 1),
    apply_lgw_imputation = function(mat) tibble(MolA = 0.5),
    merge_metadata = function(meta, imputed) bind_cols(meta, imputed)
  )

  result <- qcCheckR_impute_data(master_list)

  expect_true("plate1" %in% names(result$data$peakArea$imputed))
  expect_true("plate2" %in% names(result$data$peakArea$imputed))
  expect_named(result$data$peakArea$imputed$plate1, c("sample_name", "MolA"))
})

test_that("prepare_imputation_matrix sets rownames and removes sample columns", {
  input <- tibble(
    sample_name = c("S1", "S2"),
    MolA = c(1, 2),
    sample_type = c("qc", "sample"),
    sample_data_source = ".peakArea"
  )

  mock_replace <- function(df) df

  local_mocked_bindings(replace_problematic_values = mock_replace)

  result <- prepare_imputation_matrix(input)

  expect_equal(rownames(result), c("S1", "S2"))
  expect_named(result, "MolA")
})

test_that("prepare_imputation_matrix replaces problematic values", {
  input <- tibble(
    sample_name = c("S1", "S2"),
    MolA = c(0, Inf),
    sample_type = c("qc", "sample")
  )

  mock_replace <- function(df) {
    df$MolA <- c(0.5, 0.5)
    df
  }

  local_mocked_bindings(replace_problematic_values = mock_replace)

  result <- prepare_imputation_matrix(input)

  expect_equal(result$MolA, c(0.5, 0.5))
})

test_that("lgw_impute replaces zeros with half the minimum non-zero value", {
  input <- tibble(a = c(0, 2, 4), b = c(0, 0, 6))
  result <- lgw_impute(input)
  expect_equal(result$a[1], 1)  # half of 2
  expect_equal(result$b[1], 3)  # half of 6
  expect_equal(result$b[2], 3)
})

test_that("lgw_impute handles columns with all zeros", {
  input <- tibble(a = c(0, 0, 0))
  result <- lgw_impute(input)
  expect_true(all(is.na(result$a)))
})

test_that("lgw_impute handles mixed zero and NA values", {
  input <- tibble(a = c(0, NA, 4))
  result <- lgw_impute(input)
  expect_equal(result$a[1], 2)
  expect_equal(result$a[2], 2)
})

test_that("lgw_impute returns a data.frame", {
  input <- tibble(a = c(0, 2, 4))
  result <- lgw_impute(input)
  expect_s3_class(result, "data.frame")
})

test_that("lgw_impute coerces non-numeric columns", {
  input <- tibble(a = c("0", "2", "4"))
  result <- lgw_impute(input)
  expect_type(result$a, "double")
  expect_equal(result$a[1], 1)  # half of 2

})

test_that("apply_lgw_imputation returns a tibble with sample_name column", {
  input <- tibble(row.names = c("sample1", "sample2"),
                  a = c(0, 2),
                  b = c(4, 0))
  result <- apply_lgw_imputation(input)
  expect_s3_class(result, "tbl_df")
  expect_true("sample_name" %in% colnames(result))
})

test_that("apply_lgw_imputation replaces zeros with half the minimum non-zero value", {
  input <- tibble(row.names = c("s1", "s2"),
                  a = c(0, 2),
                  b = c(4, 0))
  result <- apply_lgw_imputation(input)
  expect_equal(result$a[1], 1)  # half of 2
  expect_equal(result$b[2], 2)  # half of 4
})

test_that("apply_lgw_imputation replaces Inf values with 1", {
  input <- tibble(row.names = c("s1", "s2"),
                  a = c(Inf, 2),
                  b = c(4, Inf))
  result <- apply_lgw_imputation(input)
  expect_equal(result$a[1], 1)
  expect_equal(result$b[2], 1)
})


test_that("matrix conversion preserves column structure", {
  input <- matrix(c(0, 2, 4, 0), nrow = 2, byrow = TRUE)
  colnames(input) <- c("a", "b")
  df <- as.data.frame(input)
  expect_equal(colnames(df), c("a", "b"))
})

test_that("merge_metadata correctly merges by sample_name", {
  original <- tibble(sample_name = c("s1", "s2"), sample_meta = c("A", "B"), Lipx = c("1","2"))
  imputed <- tibble(sample_name = c("s1", "s2"), a = c(1, 2))
  result <- merge_metadata(original, imputed)
  expect_equal(result$sample_meta, c("A", "B"))
  expect_equal(result$a, c(1, 2))
  expect_equal(result$sample_name, c("s1","s2"))
})

test_that("merge_metadata adds sample_data_source column", {
  original <- tibble(sample_name = c("s1"), meta = "A")
  imputed <- tibble(sample_name = c("s1"), a = 1)
  result <- merge_metadata(original, imputed)
  expect_true("sample_data_source" %in% colnames(result))
  expect_equal(result$sample_data_source, ".peakAreaImputed")
})

test_that("merge_metadata handles unmatched sample_name values", {
  original <- tibble(sample_name = c("s1"), meta = "A")
  imputed <- tibble(sample_name = c("s2"), a = 2)
  result <- merge_metadata(original, imputed)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$a))
})


##Calculate response and concentration tests----
test_that("qcCheckR_calculate_response_concentration returns updated master_list", {
  local_mocked_bindings(
    calculate_plate_response_concentration = function(ml, pid, dt, tv) {
      ml$data$response[[dt]] <- paste("response", pid, dt)
      ml$data$concentration[[dt]] <- paste("concentration", pid, dt)
      return(ml)
    },
    harmonise_lipid_columns = function(ml) {
      ml$harmonised <- TRUE
      return(ml)
    }
  )

  # Minimal mock master_list
  master_list <- list(
    data = list(
      peakArea = list(
        imputed = list("plate1" = data.frame())
      )
    ),
    project_details = list(
      plate_method_versions = list("plate1" = "v1")
    ),
    templates = list("Plate SIL version" = list())
  )

  result <- qcCheckR_calculate_response_concentration(master_list)

  expect_type(result, "list")
  expect_true("response" %in% names(result$data))
  expect_true("concentration" %in% names(result$data))
  expect_equal(result$data$response$sorted, "response plate1 sorted")
  expect_equal(result$data$concentration$imputed, "concentration plate1 imputed")
  expect_equal(result$project_details$is_ver, "v1")
  expect_equal(result$templates[["Plate SIL version"]][["plate1"]], "v1")
  expect_true(result$harmonised)
})

test_that("calculate_plate_response_concentration processes SIL columns and updates master_list", {
  local_mocked_bindings(
    process_sil_target = function(ml, plate_id, data_type, template_version, sil) {
      ml$processed_targets <- c(ml$processed_targets, sil)
      return(ml)
    }
  )

  # Create mock data
  mock_data <- data.frame(sample_1 = 1:3,sample_2 = 4:6,SIL_A = 7:9,SIL_B = 10:12)

  master_list <- list(
    data = list(
      peakArea = list(
        imputed = list(
          plate1 = mock_data
        ))),
    templates = list(
      mrm_guides = list(
        v1 = list(
          SIL_guide = list(
            Note = c("SIL_A", "SIL_B", "SIL_C")  # SIL_C not in data
          )))))

  result <- calculate_plate_response_concentration(master_list, "plate1", "imputed", "v1")

  # Check that SIL columns were reordered
  reordered_cols <- colnames(result$data$peakArea$imputed$plate1)
  expect_true(all(c("SIL_A", "SIL_B") %in% reordered_cols))
  expect_true(!"SIL_C" %in% reordered_cols)

  # Check that response and concentration were created
  expect_true("response" %in% names(result$data))
  expect_true("concentration" %in% names(result$data))
  expect_true("plate1" %in% names(result$data$response$imputed))
  expect_true("plate1" %in% names(result$data$concentration$imputed))

  # Check sample_data_source tags
  expect_equal(
    unique(result$data$response$imputed$plate1$sample_data_source),
    ".response.imputed")

  expect_equal(
    unique(result$data$concentration$imputed$plate1$sample_data_source),
    "concentration.imputed")

  # Check that process_sil_target was called for SIL_A and SIL_B only
  expect_equal(result$processed_targets, c("SIL_A", "SIL_B"))
})

test_that("harmonise_lipid_columns retains only common columns across plates", {
  # Create mock data frames with overlapping and non-overlapping columns
  df1 <- data.frame(A = 1:3, B = 4:6, C = 7:9)
  df2 <- data.frame(B = 10:12, C = 13:15, D = 16:18)

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      )
    )
  )

  result <- harmonise_lipid_columns(master_list)

  # Expect only common columns B and C to be retained
  expect_equal(colnames(result$data$response$subtype1$plate1), c("B", "C"))
  expect_equal(colnames(result$data$response$subtype1$plate2), c("B", "C"))
  expect_equal(colnames(result$data$concentration$subtype1$plate1), c("B", "C"))
  expect_equal(colnames(result$data$concentration$subtype1$plate2), c("B", "C"))
})

test_that("harmonise_lipid_columns retains all columns when only one plate is present", {
  df <- data.frame(X = 1:3, Y = 4:6)

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df
        )
      )
    )
  )

  result <- harmonise_lipid_columns(master_list)

  expect_equal(colnames(result$data$response$subtype1$plate1), c("X", "Y"))
  expect_equal(colnames(result$data$concentration$subtype1$plate1), c("X", "Y"))
})

test_that("harmonise_lipid_columns returns empty data frames when no common columns exist", {
  df1 <- data.frame(A = 1:3)
  df2 <- data.frame(B = 4:6)

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        ))))

  result <- harmonise_lipid_columns(master_list)

  expect_equal(ncol(result$data$response$subtype1$plate1), 0)
  expect_equal(ncol(result$data$response$subtype1$plate2), 0)
  expect_equal(ncol(result$data$concentration$subtype1$plate1), 0)
  expect_equal(ncol(result$data$concentration$subtype1$plate2), 0)
})

test_that("harmonise_lipid_columns handles empty or NULL data frames gracefully", {
  df1 <- data.frame(A = 1:3, B = 4:6)
  df2 <- data.frame()  # empty

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        ))))

  result <- harmonise_lipid_columns(master_list)

  # Expect empty data frames due to no common columns
  expect_equal(ncol(result$data$response$subtype1$plate1), 0)
  expect_equal(ncol(result$data$response$subtype1$plate2), 0)
  expect_equal(ncol(result$data$concentration$subtype1$plate1), 0)
  expect_equal(ncol(result$data$concentration$subtype1$plate2), 0)
})


## Stattarget batch correction tests----
test_that("qcCheckR_statTarget_batch_correction runs statTarget workflow correctly", {
  # Mock each helper function to simulate expected behavior
  local_mocked_bindings(
    initialise_statTarget_environment = function(master_list) {
      list(env = "initialised", log = list())
    },
    prepare_statTarget_files = function(FUNC_list) {
      FUNC_list$files_prepared <- TRUE
      return(FUNC_list)
    },
    run_statTarget_shiftCor = function(FUNC_list, master_list) {
      FUNC_list$correction_done <- TRUE
      return(FUNC_list)
    },
    integrate_corrected_data = function(master_list, FUNC_list) {
      master_list$corrected_data <- TRUE
      return(master_list)
    },
    update_script_log = function(master_list, ...) {
      master_list$log <- list(steps = c(...))
      return(master_list)
    }
  )

  # Minimal mock master_list
  master_list <- list()

  # Run the function
  result <- qcCheckR_statTarget_batch_correction(master_list)

  # Assertions
  expect_type(result, "list")
  expect_true(result$corrected_data)
  expect_equal(result$log$steps, c("data_preparation", "project_setup", "data_filtering"))
})

test_that("initialise_statTarget_environment sets up environment and returns correct FUNC_list", {
  local_mocked_bindings(
    check_dir_exists = function(path) FALSE,
    create_dir = function(path) TRUE,
    flag_failed_qc_injections = function(FUNC_list) {
      FUNC_list$flagged <- TRUE
      return(FUNC_list)
    }
  )

  master_list <- list(
    project_details = list(
      project_dir = "mock_project",
      qc_type = "QC"
    ),
    data = list(
      concentration = list(
        imputed = list(
          plate1 = data.frame(
            sample_type_factor = c("Sample", "QC"),
            lipid1 = c(1, 2),
            lipid2 = c(3, 4)
          )))))

  FUNC_list <- initialise_statTarget_environment(master_list)

  expect_equal(FUNC_list$project_dir, file.path("mock_project", "all", "data/batch_correction"))
  expect_s3_class(FUNC_list$master_data, "data.frame")
  expect_equal(nrow(FUNC_list$master_data), 2)
  expect_equal(FUNC_list$master_data$sample_type, c("sample", "qc"))
  expect_false(any(grepl("sample", FUNC_list$metabolite_list, ignore.case = TRUE)))
  expect_true(FUNC_list$flagged)
})


