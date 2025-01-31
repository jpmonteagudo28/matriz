# Test case 1: Import a valid CSV with required columns and extra columns
test_that("import_matrix imports valid CSV with required and extra columns", {

  skip_on_cran()

  test_file <- "test_csv.csv"  # Path to a test CSV file

  # Create a mock CSV file for testing
  write.csv(data.frame(
    year = 2020,
    citation = "Test citation",
    keywords = "test, R",
    profession = "Researcher",
    electronic = TRUE,
    purpose = "Study purpose",
    study_design = "Design",
    outcome_var = "Outcome",
    predictor_var = "Predictor",
    sample = 100,
    dropout_rate = 0.1,
    setting = "University",
    inclusion_criteria = "Age > 18",
    ethnicity = "Mixed",
    age = 30,
    sex = "M",
    income = "High",
    education = "PhD",
    measures = "Scale",
    analysis = "ANOVA",
    results = "Significant",
    limitations = "None",
    implications = "Future research",
    ethical_concerns = "None",
    biases = "No",
    notes = "Good study",
    extra_col1 = "Extra1",
    extra_col2 = "Extra2"
  ), test_file, row.names = FALSE)

  result <- import_matrix(test_file, format = "csv", drop_extra = FALSE)

  # Check that required columns are present and extra columns are retained
  expect_true(all(c("year", "citation", "keywords", "profession", "electronic") %in% names(result)))
  expect_true("extra_col1" %in% names(result))
  expect_true("extra_col2" %in% names(result))

  # Clean up the test file after testing
  unlink(test_file)
})

# Test case 2: Import a CSV with missing required columns
test_that("import_matrix raises an error with missing required columns", {

  skip_on_cran()

  # Create a CSV file with missing columns
  write.csv(data.frame(
    year = 2020,
    citation = "Test citation",
    keywords = "test, R"
  ), "missing_columns.csv", row.names = FALSE)

  # Expect an error due to missing columns
  expect_error(import_matrix("missing_columns.csv", format = "csv"),
               "The imported data is missing the following required columns:")

  unlink("missing_columns.csv")
})

# Test case 3: Import a CSV with extra columns and drop them (drop_extra = TRUE)
test_that("import_matrix drops extra columns when drop_extra = TRUE", {

  skip_on_cran()

  write.csv(data.frame(
    year = 2020,
    citation = "Test citation",
    keywords = "test, R",
    profession = "Researcher",
    electronic = TRUE,
    purpose = "Study purpose",
    study_design = "Design",
    outcome_var = "Outcome",
    predictor_var = "Predictor",
    sample = 100,
    dropout_rate = 0.1,
    setting = "University",
    inclusion_criteria = "Age > 18",
    ethnicity = "Mixed",
    age = 30,
    sex = "M",
    income = "High",
    education = "PhD",
    measures = "Scale",
    analysis = "ANOVA",
    results = "Significant",
    limitations = "None",
    implications = "Future research",
    ethical_concerns = "None",
    biases = "No",
    notes = "Good study",
    extra_col1 = "Extra1",
    extra_col2 = "Extra2"
  ), "drop_extra_test.csv", row.names = FALSE)

  result <- import_matrix("drop_extra_test.csv", format = "csv", drop_extra = TRUE)

  # Check that extra columns were dropped
  expect_false("extra_col1" %in% names(result))
  expect_false("extra_col2" %in% names(result))

  unlink("drop_extra_test.csv")
})

# Test case 4: Import a CSV with extra columns and keep them (drop_extra = FALSE)
test_that("import_matrix keeps extra columns when drop_extra = FALSE", {

  skip_on_cran()

  write.csv(data.frame(
    year = 2020,
    citation = "Test citation",
    keywords = "test, R",
    profession = "Researcher",
    electronic = TRUE,
    purpose = "Study purpose",
    study_design = "Design",
    outcome_var = "Outcome",
    predictor_var = "Predictor",
    sample = 100,
    dropout_rate = 0.1,
    setting = "University",
    inclusion_criteria = "Age > 18",
    ethnicity = "Mixed",
    age = 30,
    sex = "M",
    income = "High",
    education = "PhD",
    measures = "Scale",
    analysis = "ANOVA",
    results = "Significant",
    limitations = "None",
    implications = "Future research",
    ethical_concerns = "None",
    biases = "No",
    notes = "Good study",
    extra_col1 = "Extra1",
    extra_col2 = "Extra2"
  ), "keep_extra_test.csv", row.names = FALSE)

  result <- import_matrix("keep_extra_test.csv", format = "csv", drop_extra = FALSE)

  # Check that extra columns are kept
  expect_true("extra_col1" %in% names(result))
  expect_true("extra_col2" %in% names(result))

  unlink("keep_extra_test.csv")
})

# Test case 5: Handling unsupported file format
test_that("import_matrix raises an error for unsupported file formats", {

  skip_on_cran()

  # Create a dummy .md file (Markdown format)
  writeLines(c("# Test file", "Some content here"), "unsupported.md")

  # Expect an error for unsupported format
  expect_error(import_matrix("unsupported.md", format = "md"),
               "Unsupported file format. Please specify csv, tsv, rds, xlsx, xls, or txt.")

  unlink("unsupported.md")
})

# Test case 6: File path is not valid
test_that("import_matrix rasies an error when file path is not valid", {

  skip_on_cran()

  # Expect an error for non-existent file
  expect_error(import_matrix("non_existent_file.csv"),
               "The specified file does not exist.")
})

# Test case 7: Import an RDS file with extra columns
test_that("import_matrix imports RDS file with extra columns", {

  skip_on_cran()

  # Create a mock RDS file for testing
  test_data <- data.frame(
    year = 2020,
    citation = "Test citation",
    keywords = "test, R",
    profession = "Researcher",
    electronic = TRUE,
    purpose = "Study purpose",
    study_design = "Design",
    outcome_var = "Outcome",
    predictor_var = "Predictor",
    sample = 100,
    dropout_rate = 0.1,
    setting = "University",
    inclusion_criteria = "Age > 18",
    ethnicity = "Mixed",
    age = 30,
    sex = "M",
    income = "High",
    education = "PhD",
    measures = "Scale",
    analysis = "ANOVA",
    results = "Significant",
    limitations = "None",
    implications = "Future research",
    ethical_concerns = "None",
    biases = "No",
    notes = "Good study",
    extra_col1 = "Extra1",
    extra_col2 = "Extra2"
  )

  # Save this data to an RDS file
  test_file <- "test_data.rds"
  saveRDS(test_data, test_file)

  # Call import_matrix to test importing RDS with extra columns
  result <- import_matrix(test_file, format = "rds", drop_extra = FALSE, silent = TRUE)

  # Check that required columns are present
  expect_true(all(c("year", "citation", "keywords", "profession", "electronic") %in% names(result)))

  # Check that extra columns are retained
  expect_true("extra_col1" %in% names(result))
  expect_true("extra_col2" %in% names(result))

  # Clean up the test file after testing
  unlink(test_file)
})

# Test case 8: format is left as NULL (auto-detect)
test_that("import_matrix auto detects format from file extension", {

  skip_on_cran()

  # Create a mock CSV file for testing
  test_data <- data.frame(
    year = 2020,
    citation = "Test citation",
    keywords = "test, R",
    profession = "Researcher",
    electronic = TRUE,
    purpose = "Study purpose",
    study_design = "Design",
    outcome_var = "Outcome",
    predictor_var = "Predictor",
    sample = 100,
    dropout_rate = 0.1,
    setting = "University",
    inclusion_criteria = "Age > 18",
    ethnicity = "Mixed",
    age = 30,
    sex = "M",
    income = "High",
    education = "PhD",
    measures = "Scale",
    analysis = "ANOVA",
    results = "Significant",
    limitations = "None",
    implications = "Future research",
    ethical_concerns = "None",
    biases = "No",
    notes = "Good study",
    extra_col1 = "Extra1",
    extra_col2 = "Extra2"
  )

  # Save this data to a CSV file
  test_file <- "test_data.csv"
  write.csv(test_data, test_file, row.names = FALSE)

  # Call import_matrix with format = NULL (auto-detect)
  result <- import_matrix(test_file, format = NULL, drop_extra = FALSE, silent = TRUE)

  # Check that required columns are present
  expect_true(all(c("year", "citation", "keywords", "profession", "electronic") %in% names(result)))

  # Check that extra columns are retained
  expect_true("extra_col1" %in% names(result))
  expect_true("extra_col2" %in% names(result))

  # Clean up the test file after testing
  unlink(test_file)
})

# Test case 9: removing duplicates from literature matrix
test_that("import_matrix handles duplicate columns correctly", {

  skip_on_cran()

  # Create a mock CSV file with duplicate columns
  test_data <- data.frame(
    year = 2020,
    citation = "Test citation",
    keywords = "test, R",
    profession = "Researcher",
    electronic = TRUE,
    purpose = "Study purpose",
    study_design = "Design",
    outcome_var = "Outcome",
    predictor_var = "Predictor",
    sample = 100,
    dropout_rate = 0.1,
    setting = "University",
    inclusion_criteria = "Age > 18",
    ethnicity = "Mixed",
    age = 30,
    sex = "M",
    income = "High",
    education = "PhD",
    measures = "Scale",
    analysis = "ANOVA",
    results = "Significant",
    limitations = "None",
    implications = "Future research",
    ethical_concerns = "None",
    biases = "No",
    notes = "Good study"
  )

  # Create duplicate columns with different values
  test_data$year.2 <- 2021
  test_data$citation.2 <- "Duplicate citation"
  names(test_data)[names(test_data) == "year.2"] <- "year"
  names(test_data)[names(test_data) == "citation.2"] <- "citation"

  # Save this data to a CSV file
  test_file <- "test_data_duplicates.csv"
  write.csv(test_data, test_file, row.names = FALSE)

  # Call import_matrix
  result <- import_matrix(test_file, format = NULL, drop_extra = FALSE, silent = TRUE)

  # Tests
  # Check that duplicate columns were removed
  expect_equal(sum(names(result) == "year"), 1)
  expect_equal(sum(names(result) == "citation"), 1)

  # Check that all required columns are still present
  required_cols <- c("year", "citation", "keywords", "profession", "electronic",
                     "purpose", "study_design", "outcome_var", "predictor_var", "sample",
                     "dropout_rate", "setting", "inclusion_criteria", "ethnicity", "age",
                     "sex", "income", "education", "measures", "analysis", "results",
                     "limitations", "implications", "ethical_concerns", "biases", "notes")
  expect_true(all(required_cols %in% names(result)))

  # Check that the first instance of duplicate columns was kept
  expect_equal(result$year[1], 2020)
  expect_equal(result$citation[1], "Test citation")

  # Check total number of columns (should be exactly the number of required columns)
  expect_equal(ncol(result), length(required_cols))

  # Clean up the test file
  unlink(test_file)
})
