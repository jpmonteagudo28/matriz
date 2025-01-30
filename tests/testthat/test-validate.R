test_that("validate_columns correctly identifies missing columns", {
  data <- data.frame(year = 2020, citation = "Test")  # Missing many required columns

  expect_error(validate_columns(data),
               "The imported data is missing the following required columns")
})

test_that("validate_columns correctly identifies extra columns", {
  data <- data.frame(year = 2020, citation = "Test", keywords = "data", profession = "statistician",
                     electronic = TRUE, purpose = "research", study_design = "RCT",
                     outcome_var = "success", predictor_var = "experience", sample = 100,
                     dropout_rate = 5, setting = "urban", inclusion_criteria = "age>18",
                     ethnicity = "diverse", age = 30, sex = "M", income = 50000,
                     education = "PhD", measures = "survey", analysis = "regression",
                     results = "positive", limitations = "sample size",
                     implications = "policy change", ethical_concerns = "none",
                     biases = "response", notes = "N/A", extra1 = "Extra data")

  expect_message(validate_columns(data, extra_columns = "extra1", drop_extra = FALSE),
                 "Successfully imported matrix with")
})

test_that("validate_columns drops unwanted extra columns when drop_extra = TRUE", {
  data <- data.frame(year = 2020, citation = "Test", keywords = "data", profession = "statistician",
                     electronic = TRUE, purpose = "research", study_design = "RCT",
                     outcome_var = "success", predictor_var = "experience", sample = 100,
                     dropout_rate = 5, setting = "urban", inclusion_criteria = "age>18",
                     ethnicity = "diverse", age = 30, sex = "M", income = 50000,
                     education = "PhD", measures = "survey", analysis = "regression",
                     results = "positive", limitations = "sample size",
                     implications = "policy change", ethical_concerns = "none",
                     biases = "response", notes = "N/A", extra1 = "Extra data",
                     extra2 = "Remove me")

  cleaned_data <- validate_columns(data, extra_columns = "extra1", drop_extra = TRUE)

  expect_false("extra2" %in% colnames(cleaned_data))  # extra2 should be removed
  expect_true("extra1" %in% colnames(cleaned_data))   # extra1 should remain
})

test_that("validate_columns works silently when silent = TRUE", {
  data <- data.frame(year = 2020, citation = "Test", keywords = "data", profession = "statistician",
                     electronic = TRUE, purpose = "research", study_design = "RCT",
                     outcome_var = "success", predictor_var = "experience", sample = 100,
                     dropout_rate = 5, setting = "urban", inclusion_criteria = "age>18",
                     ethnicity = "diverse", age = 30, sex = "M", income = 50000,
                     education = "PhD", measures = "survey", analysis = "regression",
                     results = "positive", limitations = "sample size",
                     implications = "policy change", ethical_concerns = "none",
                     biases = "response", notes = "N/A", extra1 = "Extra data")

  expect_silent(validate_columns(data, extra_columns = "extra1", drop_extra = FALSE, silent = TRUE))
})

test_that("validate_columns returns a correctly structured data frame", {
  data <- data.frame(year = 2020, citation = "Test", keywords = "data", profession = "statistician",
                     electronic = TRUE, purpose = "research", study_design = "RCT",
                     outcome_var = "success", predictor_var = "experience", sample = 100,
                     dropout_rate = 5, setting = "urban", inclusion_criteria = "age>18",
                     ethnicity = "diverse", age = 30, sex = "M", income = 50000,
                     education = "PhD", measures = "survey", analysis = "regression",
                     results = "positive", limitations = "sample size",
                     implications = "policy change", ethical_concerns = "none",
                     biases = "response", notes = "N/A")

  result <- validate_columns(data)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), length(colnames(data)))  # Should retain same columns
})
