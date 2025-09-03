# Tests for Update Script Log Function ----
test_that("update_script_log updates master_list correctly", {
  suppressMessages({
  # Setup: create a mock master_list
  start_time <- Sys.time()
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(start_time = start_time),
        runtimes = list(),
        messages = list()
      )
    )
  )
  # Run the function
  updated_list <- update_script_log(
    master_list,
    section_name = "section_1",
    previous_section_name = "start_time",
    next_section_name = "section_2"
  )
  # Expectations
  expect_true("section_1" %in% names(updated_list$project_details$script_log$timestamps))
  expect_true("section_1" %in% names(updated_list$project_details$script_log$runtimes))
  expect_true("section_1" %in% names(updated_list$project_details$script_log$messages))
  # Check that timestamps are POSIXct
  expect_s3_class(updated_list$project_details$script_log$timestamps$section_1, "POSIXct")
  # Check that runtime is numeric
  expect_type(updated_list$project_details$script_log$runtimes$section_1, "double")
  # Check that message is a character string
  expect_type(updated_list$project_details$script_log$messages$section_1, "character")

  })
})

test_that("update_script_log throws error for missing/invalid previous section", {
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(),
        runtimes = list(),
        messages = list()
      )
    )
  )
  expect_error(
    update_script_log(master_list, "section_1", "start_time", "section_2"),
    regexp = "Invalid previous_section_name"
  )
})

test_that("validate_previous_section passes when previous_section_name exists", {
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(start_time = Sys.time())
      )
    )
  )
  expect_silent(validate_previous_section(master_list, "start_time"))
})

test_that("validate_previous_section throws error when previous_section_name is missing", {
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list()
      )
    )
  )
  expect_error(
    validate_previous_section(master_list, "start_time"),
    regexp = "Invalid previous_section_name"
  )
})

test_that("capture_current_time adds a timestamp for the given section", {
  # Setup: create a minimal master_list
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list()
      )
    )
  )
  # Run the function
  updated_list <- capture_current_time(master_list, "section_1")
  # Check that the timestamp was added
  expect_true("section_1" %in% names(updated_list$project_details$script_log$timestamps))
  # Check that the timestamp is of class POSIXct
  expect_s3_class(updated_list$project_details$script_log$timestamps$section_1, "POSIXct")
})

test_that("capture_current_time overwrites existing timestamp", {
  # Setup: create a master_list with an existing timestamp
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(section_1 = as.POSIXct("2020-01-01 00:00:00"))
      )
    )
  )
  # Run the function
  updated_list <- capture_current_time(master_list, "section_1")
  # Check that the timestamp was updated (not equal to the old one)
  expect_false(updated_list$project_details$script_log$timestamps$section_1 ==
                 as.POSIXct("2020-01-01 00:00:00"))
})

test_that("calculate_runtime correctly calculates time difference in minutes", {
  # Setup: create a master_list with two timestamps
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(
          start_time = as.POSIXct("2023-01-01 00:00:00"),
          section_1 = as.POSIXct("2023-01-01 00:05:00")
        ),
        runtimes = list()
      )
    )
  )
  # Run the function
  updated_list <- calculate_runtime(master_list, "section_1", "start_time")
  # Check that the runtime is 5 minutes
  expect_equal(updated_list$project_details$script_log$runtimes$section_1, as.difftime(5, units = "mins"))
})

test_that("calculate_runtime handles identical timestamps", {
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(
          start_time = as.POSIXct("2023-01-01 00:00:00"),
          section_1 = as.POSIXct("2023-01-01 00:00:00")
        ),
        runtimes = list()
      )
    )
  )
  updated_list <- calculate_runtime(master_list, "section_1", "start_time")
  expect_equal(updated_list$project_details$script_log$runtimes$section_1, as.difftime(0, units = "mins"))
})

test_that("calculate_runtime returns master_list with updated runtimes", {
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(
          start_time = Sys.time(),
          section_1 = Sys.time() + 60
        ),
        runtimes = list()
      )
    )
  )
  updated_list <- calculate_runtime(master_list, "section_1", "start_time")
  expect_true("section_1" %in% names(updated_list$project_details$script_log$runtimes))
  expect_s3_class(updated_list$project_details$script_log$runtimes$section_1, "difftime")
})

test_that("calculate_total_runtime correctly calculates total runtime in minutes", {
  # Setup: create a master_list with start_time and section_1 timestamps
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(
          start_time = as.POSIXct("2023-01-01 00:00:00"),
          section_1 = as.POSIXct("2023-01-01 00:10:00")
        ),
        runtimes = list()
      )
    )
  )
  # Run the function
  updated_list <- calculate_total_runtime(master_list, "section_1")
  # Check that total_runtime is 10 minutes
  expect_equal(updated_list$project_details$script_log$runtimes$total_runtime, as.difftime(10, units = "mins"))
})

test_that("calculate_total_runtime handles identical timestamps", {
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(
          start_time = as.POSIXct("2023-01-01 00:00:00"),
          section_1 = as.POSIXct("2023-01-01 00:00:00")
        ),
        runtimes = list()
      )
    )
  )
  updated_list <- calculate_total_runtime(master_list, "section_1")
  expect_equal(updated_list$project_details$script_log$runtimes$total_runtime, as.difftime(0, units = "mins"))
})

test_that("calculate_total_runtime returns master_list with total_runtime", {
  master_list <- list(
    project_details = list(
      script_log = list(
        timestamps = list(
          start_time = Sys.time(),
          section_1 = Sys.time() + 120
        ),
        runtimes = list()
      )
    )
  )
  updated_list <- calculate_total_runtime(master_list, "section_1")
  expect_true("total_runtime" %in% names(updated_list$project_details$script_log$runtimes))
  expect_s3_class(updated_list$project_details$script_log$runtimes$total_runtime, "difftime")
})

test_that("create_message generates correct formatted message", {
  # Setup: create a master_list with runtimes
  master_list <- list(
    project_details = list(
      script_log = list(
        runtimes = list(
          section_1 = as.difftime(5, units = "mins"),
          total_runtime = as.difftime(15, units = "mins")
        ),
        messages = list()
      )
    )
  )
  # Run the function
  updated_list <- create_message(master_list, "section_1", "section_2")
  # Extract the message
  msg <- updated_list$project_details$script_log$messages$section_1
  # Check that message contains expected content
  expect_true(grepl("SECTION 1 complete!", msg))
  expect_true(grepl("Section runtime: 5", msg))
  expect_true(grepl("Total runtime: 15", msg))
  expect_true(grepl("Initialising: SECTION 2", msg))
})

test_that("create_message returns master_list with message entry", {
  master_list <- list(
    project_details = list(
      script_log = list(
        runtimes = list(
          section_1 = as.difftime(3.25, units = "mins"),
          total_runtime = as.difftime(10.75, units = "mins")
        ),
        messages = list()
      )
    )
  )
  updated_list <- create_message(master_list, "section_1", "section_2")

  expect_true("section_1" %in% names(updated_list$project_details$script_log$messages))
  expect_type(updated_list$project_details$script_log$messages$section_1, "character")
})

test_that("print_message outputs the correct message", {
  # Setup: create a master_list with a message
  master_list <- list(
    project_details = list(
      script_log = list(
        messages = list(
          section_1 = "SECTION 1 complete!\n\n Section runtime: 5 minutes\n\n Total runtime: 15 minutes\n\nInitialising: SECTION 2...."
        )
      )
    )
  )
  # Capture the message output
  expect_message(
    result <- print_message(master_list, "section_1"),
    regexp = "SECTION 1 complete!"
  )
  # Check that the returned object is unchanged
  expect_identical(result, master_list)
})

test_that("print_message handles missing message gracefully", {
  master_list <- list(
    project_details = list(
      script_log = list(
        messages = list()
      )
    )
  )
  expect_error(
    print_message(master_list, "section_1"),
    regexp = "No message found for the given section."
  )
})

#Validate Parameter Functions Tests----

##Project_Directory Tests----
test_that("validate_project_directory returns TRUE for valid directory", {
  suppressMessages({
  # Create a temporary directory for testing
  temp_dir <- tempdir()

  expect_message(
    validate_project_directory(temp_dir),
    regexp = paste("Accessing project directory")
    )
  })
})

test_that("validate_project_directory throws error for non-string input", {
  expect_error(
    validate_project_directory(123),
    regexp = "project_directory must be a single string."
  )

  expect_error(
    validate_project_directory(c("dir1", "dir2")),
    regexp = "project_directory must be a single string."
  )
})

test_that("validate_project_directory throws error for non-existent directory", {
  fake_dir <- file.path(tempdir(), "non_existent_subdir")

  expect_error(
    validate_project_directory(fake_dir),
    regexp = "The specified project directory does not exist."
  )
})

##Master_list_porject_directory Tests----
test_that("validate_master_list_project_directory passes for existing directory", {
  # Setup: use a temporary directory
  temp_dir <- tempdir()
  master_list <- list(
    project_details = list(
      project_dir = temp_dir
    )
  )

  expect_silent(validate_master_list_project_directory(master_list))
})

test_that("validate_master_list_project_directory throws error for non-existent directory", {
  fake_dir <- file.path(tempdir(), "non_existent_subdir")
  master_list <- list(
    project_details = list(
      project_dir = fake_dir
    )
  )

  expect_error(
    validate_master_list_project_directory(master_list),
    regexp = paste("Project directory does not exist:", fake_dir),
    fixed = TRUE
  )
})
##SkylineR MRM_template_List Tests----
test_that("ANPC user with NULL input returns default list", {
  suppressMessages({
  result <- validate_mrm_template_list(NULL, "ANPC")
  expect_type(result, "list")
  expect_true(all(c("v1", "v2", "v4") %in% names(result)))
  })
})

test_that("ANPC user with non-NULL input returns NULL", {
  suppressMessages({
  dummy_list <- list(v1 = data.frame())
  result <- validate_mrm_template_list(dummy_list, "ANPC")
  expect_null(result)
  })
})

test_that("Non-ANPC user with NULL input throws error", {
  expect_error(validate_mrm_template_list(NULL, "user"),
               "Please provide a valid mrm_template_list.")
})

test_that("Non-list input throws error", {
  expect_error(validate_mrm_template_list("not_a_list", "user"),
               "mrm_template_list must be a list.")
})

test_that("List with non-data.frame elements throws error", {
  bad_list <- list(v1 = "not_a_df")
  expect_error(validate_mrm_template_list(bad_list, "user"),
               "Each version in mrm_template_list must be a data frame")
})

test_that("List with missing required columns throws error", {
  bad_df <- data.frame(Molecule.List.Name = "A")  # intentionally wrong column name
  bad_list <- list(v1 = bad_df)
  expect_error(validate_mrm_template_list(bad_list, "user"),
               "Missing required columns in version")
})

test_that("Valid input returns NULL", {
  suppressMessages({
  good_df <- data.frame(matrix(ncol = 10, nrow = 1))
  colnames(good_df) <- c(
    "Molecule List Name", "Precursor Name", "Precursor Mz", "Precursor Charge",
    "Product Mz", "Product Charge", "Explicit Retention Time",
    "Explicit Retention Time Window", "Note", "control_chart"
  )
  good_df[1, ] <- list("A", "B", 123.4, 1, 456.7, 1, 5.6, 0.5, "test", TRUE)
  good_list <- list(v1 = good_df)
  expect_null(validate_mrm_template_list(good_list, "user"))
  })
})

##qcChecker MRM_template_List Tests----
make_valid_master_list <- function() {
  sil_guide <- data.frame(
    "Molecule List Name" = "A",
    "Precursor Name" = "B",
    "Precursor Mz" = 123.4,
    "Precursor Charge" = 1,
    "Product Mz" = 456.7,
    "Product Charge" = 1,
    "Explicit Retention Time" = 5.6,
    "Explicit Retention Time Window" = 0.5,
    "Note" = "test",
    "control_chart" = TRUE,
    "Source" = "internal"
  )

  conc_guide <- data.frame(
    concentration_factor = 1,
    SIL_name = "SIL1"
  )

  list(
    templates = list(
      mrm_guides = list(
        v1 = list(
          SIL_guide = sil_guide,
          conc_guide = conc_guide
        )
      )
    )
  )
}





