test_that("init_matrix creates a data frame with correct default number of rows", {
  result <- init_matrix()
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)  # Default number of rows
  expect_equal(ncol(result), 26)  # Expected number of predefined columns
})


test_that("init_matrix throws error when no rows are passed", {
  expect_error(init_matrix(new_col = NA), "Length of new column must match number of rows in data frame")
})
