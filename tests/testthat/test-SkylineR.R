# Test User exported wrapper function
library(testthat)
library(mockery)
library(mockr)
library(withr)
library(tibble)

# Tests for SkylineR User function ----
test_that("SkylineR handles valid inputs and sets plateIDs correctly", {
  suppressMessages({
  # Create a clean test directory
  base_temp <- tempdir()
  test_project_dir <- file.path(base_temp, "skyline_test_project")
  dir.create(test_project_dir, showWarnings = FALSE)

  # Create required subdirectory and files
  mzml_dir <- file.path(test_project_dir, "msConvert_mzml_output")
  dir.create(mzml_dir, showWarnings = FALSE)
  file.create(file.path(mzml_dir, "PLATE_1_sample.mzML"))
  file.create(file.path(mzml_dir, "PLATE_2_sample.mzML"))

  # Mock dependencies
  stub(SkylineR, "validate_project_directory", function(x) TRUE)
  stub(SkylineR, "validate_mrm_template_list", function(x, y) x)
  stub(SkylineR, "check_docker", function() TRUE)
  stub(SkylineR, "skyline_setup_project", function(...) list())
  stub(SkylineR, "import_mzml", function(...) list())
  stub(SkylineR, "peak_picking", function(...) list())
  stub(SkylineR, "log_error", function(msg) message(msg))
  stub(SkylineR, "archive_raw_files", function(project_directory) message("Archived"))

  # Run function with plateID_outputs
  expect_message(
    SkylineR(
      user_name = "TestUser",
      project_directory = test_project_dir,
      mrm_template_list = list("template1.tsv"),
      QC_sample_label = "LTR",
      plateID_outputs = c("PLATE_1", "PLATE_2")
    ),
    regexp = "Valid plateID_outputs have been provided"
  )
  })
})

test_that("SkylineR handles invalid plateID_outputs appropriately", {
  suppressMessages({
  # Setup test directory and files
  base_temp <- tempdir()
  test_project_dir <- file.path(base_temp, "skyline_test_project_invalid")
  dir.create(test_project_dir, showWarnings = FALSE)

  mzml_dir <- file.path(test_project_dir, "msConvert_mzml_output")
  dir.create(mzml_dir, showWarnings = FALSE)
  file.create(file.path(mzml_dir, "PLATE_1_sample.mzML"))

  # Mock dependencies
  stub(SkylineR, "validate_project_directory", function(x) TRUE)
  stub(SkylineR, "validate_mrm_template_list", function(x, y) x)
  stub(SkylineR, "check_docker", function() TRUE)
  stub(SkylineR, "skyline_setup_project", function(...) list())
  stub(SkylineR, "import_mzml", function(...) list())
  stub(SkylineR, "peak_picking", function(...) list())
  stub(SkylineR, "log_error", function(msg) message(msg))
  stub(SkylineR, "archive_raw_files", function(project_directory) message("Archived"))

  # Run with invalid plateID_outputs
  expect_error(suppressMessages(
    SkylineR(
      user_name = "TestUser",
      project_directory = test_project_dir,
      mrm_template_list = list("template1.tsv"),
      QC_sample_label = "LTR",
      plateID_outputs = c("INVALID_PLATE")
    ),
    regexp = "Zero associated mzml files forbidden. Please check your input for plateID_outputs parameter."
  ))
  })
})








