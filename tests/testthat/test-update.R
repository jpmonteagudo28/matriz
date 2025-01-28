test_that("update_record handles basic updates correctly", {
  df <- data.frame(
    id = 1:5,
    value = letters[1:5],
    stringsAsFactors = FALSE
  )

  # Test updating with column name
  result <- update_record(df, column = value, where = id > 3, set_to = "z")
  expect_equal(result$value[4:5], c("z", "z"))
  expect_equal(result$value[1:3], c("a", "b", "c"))

  # Test updating with column index
  result <- update_record(df, column = 2, where = id <= 2, set_to = "x")
  expect_equal(result$value[1:2], c("x", "x"))
  expect_equal(result$value[3:5], c("c", "d", "e"))
})

test_that("update_record handles different data types correctly", {
  df <- data.frame(
    id = 1:5,
    numbers = 1:5,
    letters = letters[1:5],
    stringsAsFactors = FALSE
  )

  # Test updating numeric column
  result <- update_record(df, column = numbers, where = id > 3, set_to = 99)
  expect_equal(result$numbers[4:5], c(99, 99))

  # Test updating character column
  result <- update_record(df, column = letters, where = id < 3, set_to = "test")
  expect_equal(result$letters[1:2], c("test", "test"))
})

test_that("update_record validates inputs correctly", {
  df <- data.frame(
    id = 1:5,
    value = letters[1:5],
    stringsAsFactors = FALSE
  )

  # Test missing column
  expect_error(update_record(df, where = id > 3, set_to = "z"),
               "Column must be specified")

  # Test invalid column name
  expect_error(update_record(df, column = nonexistent, where = id > 3, set_to = "z"),
               "Column nonexistent does not exist in the data frame")

  # Test invalid column index
  expect_error(update_record(df, column = 99, where = id > 3, set_to = "z"),
               "Column index is out of range")

  # Test invalid where condition
  expect_error(update_record(df, column = value, where = c(TRUE, FALSE), set_to = "z"),
               "The `where` condition must evaluate to a logical vector of the same length as the data")
})

test_that("update_record preserves data frame structure", {
  df <- data.frame(
    id = 1:5,
    value = letters[1:5],
    stringsAsFactors = FALSE
  )

  result <- update_record(df, column = value, where = id > 3, set_to = "z")

  # Test that dimensions are preserved
  expect_equal(dim(result), dim(df))

  # Test that column names are preserved
  expect_equal(names(result), names(df))

  # Test that row names are preserved
  expect_equal(rownames(result), rownames(df))
})

test_that("update_record handles edge cases", {
  df <- data.frame(
    id = 1:5,
    value = letters[1:5],
    stringsAsFactors = FALSE
  )

  # Test with all FALSE condition
  result <- update_record(df, column = value, where = id < 0, set_to = "z")
  expect_equal(result$value, df$value)

  # Test with all TRUE condition
  result <- update_record(df, column = value, where = id > 0, set_to = "z")
  expect_equal(result$value, rep("z", 5))

  # Test with NA values in condition
  expect_error(
    update_record(df, column = value, where = 17, set_to = "z"),
    "The `where` condition must evaluate to a logical vector of the same length as the data"
  )
})
