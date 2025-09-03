#Tests for inputs ----
test_that("qcCheckR throws error when required arguments are missing or invalid", {
  suppressMessages({

  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_error(qcCheckR(project_directory = temp_dir),
               "user_name parameter is required")

  expect_error(qcCheckR(user_name = "user", project_directory = NULL),
               "project_directory must be a single string.")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        QC_sample_label = "qc", sample_tags = c("sample")),
               "mrm_template_list parameter is required")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        mrm_template_list = list(), sample_tags = c("sample")),
               "QC_sample_label parameter is required")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        mrm_template_list = list(), QC_sample_label = "qc"),
               "sample_tags parameter is required")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        mrm_template_list = list(), QC_sample_label = "qc",
                        sample_tags = c("sample"), mv_threshold = 150),
               "mv_threshold must be a numeric value between 0 and 100")
  })
})

#Tests for ANPC Exception logic
test_that("qcCheckR allows missing templates for ANPC user", {
  suppressMessages({
  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_error(qcCheckR(user_name = "ANPC", project_directory = temp_dir, mv_threshold = 50),
               "No report files found in the specified project directory.")
  #If it gets to this point then ANPC exception logic is working
  })
})




