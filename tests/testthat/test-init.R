test_that("init_matrix creates a data frame with correct default number of rows", {
  result <- init_matrix()
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)  # Default number of rows
  expect_equal(ncol(result), 26)  # Expected number of predefined columns
})

test_that("init_matrix creates a data frame with specified number of rows", {
  result <- init_matrix(nrow = 5)
  expect_equal(nrow(result), 5)
})

test_that("init_matrix rejects negative nrow values", {
  expect_error(init_matrix(nrow = -1), "nrow argument must be a numeric vector with a length of 1")
})

test_that("init_matrix rejects non-numeric nrow values", {
  expect_error(init_matrix(nrow = "five"), "nrow argument must be a numeric vector with a length of 1")
})

test_that("init_matrix allows additional column names", {
  result <- init_matrix(extra_column, another_column, nrow = 3)
  expect_true("extra_column" %in% names(result))
  expect_true("another_column" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("init_matrix returns empty columns with correct types", {
  result <- init_matrix(nrow = 2)
  expect_type(result$year, "double")
  expect_type(result$citation, "character")
  expect_type(result$electronic, "logical")
  expect_type(result$sex, "integer") # Factors are stored as integers internally
})

test_that("init_matrix prints a message when extra arguments are passed", {
  expect_message(init_matrix(new_col = NA), "Extra arguments passed: NA")
})
