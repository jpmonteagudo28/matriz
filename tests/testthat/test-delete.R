# Sample test dataset
test_data <- data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  stringsAsFactors = FALSE
)

test_that("delete_record removes the correct row", {
  result <- delete_record(test_data, position = 3)
  expect_equal(nrow(result), 4)  # One row should be removed
  expect_false(3 %in% result$ID)  # The deleted row should not be in the result
})

test_that("delete_record handles multiple positions correctly", {
  result <- delete_record(test_data, position = c(1, 5))
  expect_equal(nrow(result), 3)  # Two rows should be removed
  expect_false(1 %in% result$ID)
  expect_false(5 %in% result$ID)
})

test_that("delete_record returns same data when position is NULL", {
  result <- delete_record(test_data, position = NULL)
  expect_equal(nrow(result), nrow(test_data))  # No rows should be deleted
})

test_that("delete_record returns an empty data frame when deleting the only row", {
  single_row <- data.frame(ID = 1, Name = "A")
  result <- delete_record(single_row, position = 1)
  expect_equal(nrow(result), 0)  # Should be empty
})

test_that("delete_record gives an error for non-numeric position", {
  expect_error(delete_record(test_data, position = "three"),
               "The 'position' argument must be a numeric vector")
})

test_that("delete_record gives an error for empty position vector", {
  expect_error(delete_record(test_data, position = numeric(0)),
               "The 'position' is empty and length == 0")
})
