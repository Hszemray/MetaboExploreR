library(testthat)
library(mockery)
library(stringr)

test_that("msConvertR runs successfully with valid input", {
  suppressMessages({
  # Mock dependencies
  mock_validate_input_directory <- function(dir) TRUE
  mock_validate_file_types <- function(dir) c("sample1.wiff", "sample2.wiff")
  mock_check_docker <- function() TRUE
  mock_msConvertR_mzml_conversion <- function(input, output, plateIDs, pattern) TRUE

  # Temporarily override functions
  stub(msConvertR, "validate_input_directory", mock_validate_input_directory)
  stub(msConvertR, "validate_file_types", mock_validate_file_types)
  stub(msConvertR, "check_docker", mock_check_docker)
  stub(msConvertR, "msConvertR_mzml_conversion", mock_msConvertR_mzml_conversion)

  expect_message(
    msConvertR("input_dir", "output_dir"),
    "Converted mzML files are located"
    )
  })
})

test_that("msConvertR stops when no vendor files are found", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "validate_file_types", function(dir) character(0))

  expect_error(
    msConvertR("input_dir", "output_dir"),
    "No files supported found"
  )
  })
})

test_that("msConvertR handles errors during conversion gracefully", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "validate_file_types", function(dir) c("sample1.wiff"))
  stub(msConvertR, "check_docker", function() TRUE)
  stub(msConvertR, "msConvertR_mzml_conversion", function(...) stop("Conversion failed"))

  expect_message(
    msConvertR("input_dir", "output_dir"),
    "Error processing vendor files"
  )
  })
})

test_that("msConvertR gives correct message when input and output directories are the same", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "validate_file_types", function(dir) c("sample1.wiff"))
  stub(msConvertR, "check_docker", function() TRUE)
  stub(msConvertR, "msConvertR_mzml_conversion", function(...) TRUE)

  expect_message(
    msConvertR("same_dir", "same_dir"),
    "Input and output directories are the same"
  )
  })
})

test_that("msConvertR gives correct message when input and output directories are different", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "validate_file_types", function(dir) c("sample1.wiff"))
  stub(msConvertR, "check_docker", function() TRUE)
  stub(msConvertR, "msConvertR_mzml_conversion", function(...) TRUE)

  expect_message(
    msConvertR("input_dir", "output_dir"),
    "Input and output directories are different"
  )
  })
})

test_that("msConvertR_setup_project_directories creates correct structure", {
  suppressMessages({
  output_dir <- file.path(tempdir(), "test_project")
  plateIDs <- c("plate1", "plate2")

  msConvertR_setup_project_directories(output_dir, plateIDs)

  for (plateID in plateIDs) {
    base_path <- file.path(output_dir, plateID)
    expect_true(dir.exists(file.path(base_path, "data", "mzml")))
    expect_true(dir.exists(file.path(base_path, "data", "rda")))
    expect_true(dir.exists(file.path(base_path, "data", "skyline")))
    expect_true(dir.exists(file.path(base_path, "data", "raw_data")))
    expect_true(dir.exists(file.path(base_path, "data", "batch_correction")))
    expect_true(dir.exists(file.path(base_path, "html_report")))
  }
  })
})

test_that("msConvertR_construct_command_for_terminal builds correct Docker command", {
  suppressMessages({
  # Use temporary directories for testing
  input_dir <- file.path(tempdir(), "input_test")
  output_dir <- file.path(tempdir(), "output_test")

  # Create dummy directories to simulate structure
  dir.create(file.path(input_dir, "raw_data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "msConvert_mzml_output"), recursive = TRUE, showWarnings = FALSE)

  # Run the function
  command <- msConvertR_construct_command_for_terminal(input_dir, output_dir)

  # Check that the result is a character string
  expect_type(command, "character")

  # Check that the command contains expected Docker components
  expect_true(grepl("docker run", command))
  expect_true(grepl("proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses", command))
  expect_true(grepl("wine msconvert", command))
  expect_true(grepl("/data/\\*\\*", command))  # Wildcard for all files
  expect_true(grepl("-o /output", command))

  # Check that normalized paths are included
  escape_for_regex <- function(path) {
    gsub("\\\\", "\\\\\\\\", path)
  }

  expected_input_mount <- sprintf('"%s:/data"', escape_for_regex(normalizePath(file.path(input_dir, "raw_data"), mustWork = FALSE)))
  expect_true(grepl(expected_input_mount, command))

  expected_output_mount <- sprintf('"%s:/output"', escape_for_regex(normalizePath(file.path(output_dir, "msConvert_mzml_output"), mustWork = FALSE)))
  expect_true(grepl(expected_output_mount, command))

  })
})

test_that("msConvertR_restructure_directory moves files correctly", {
  suppressMessages({
  # Create a temporary directory structure
  temp_dir <- tempdir()
  plateIDs <- c("Plate1")
  vendor_extension_patterns <- "\\.raw$|\\.d$"

  # Create mock raw_data and mzML directories
  raw_data_dir <- file.path(temp_dir, "raw_data")
  mzml_output_dir <- file.path(temp_dir, "msConvert_mzml_output")
  dir.create(raw_data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(mzml_output_dir, recursive = TRUE, showWarnings = FALSE)

  # Create mock raw files
  raw_file <- file.path(raw_data_dir, "Plate1_sample.raw")
  dir.create(file.path(raw_data_dir, "Plate1_sample.d"), recursive = TRUE, showWarnings = FALSE) # mock .d directory
  file.create(raw_file)

  # Create mock mzML files
  mzml_file <- file.path(mzml_output_dir, "Plate1_sample.mzML")
  file.create(mzml_file)

  # Run the function
  msConvertR_restructure_directory(temp_dir, plateIDs, vendor_extension_patterns)

  # Check if raw files were copied
  raw_data_dest <- file.path(temp_dir, "Plate1", "data", "raw_data")
  expect_true(file.exists(file.path(raw_data_dest, "Plate1_sample.raw")))
  expect_true(file.exists(file.path(raw_data_dest, "Plate1_sample.d")))

  # Check if mzML files were copied
  mzml_dest <- file.path(temp_dir, "Plate1", "data", "mzml")
  expect_true(file.exists(file.path(mzml_dest, "Plate1_sample.mzML")))

  })
})


