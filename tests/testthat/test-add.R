test_that("add_record basic functionality works", {

  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    stringsAsFactors = FALSE
  )

  # Test adding a record with named arguments
  result <- add_record(df, id = 4, name = "Alice")
  expect_equal(nrow(result), 4)
  expect_equal(result$name[4], "Alice")
  expect_equal(result$id[4], 4)

  # Test adding a record as a vector
  result <- add_record(df, c(5, "Charlie"))
  expect_equal(nrow(result), 4)
  expect_equal(result$name[4], "Charlie")
  expect_equal(result$id[4], "5")
})

test_that("add_record position specifications work", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    stringsAsFactors = FALSE
  )

  # Test adding before specific position
  result <- add_record(df, id = 4, name = "Alice", .before = 2)
  expect_equal(result$name, c("John", "Alice", "Jane", "Bob"))

  # Test adding after specific position
  result <- add_record(df, id = 4, name = "Alice", .after = 2)
  expect_equal(result$name, c("John", "Jane", "Alice", "Bob"))

  # Test adding at the beginning
  result <- add_record(df, id = 4, name = "Alice", .before = 1)
  expect_equal(result$name, c("Alice", "John", "Jane", "Bob"))

  # Test adding at the end
  result <- add_record(df, id = 4, name = "Alice", .after = 3)
  expect_equal(result$name, c("John", "Jane", "Bob", "Alice"))
})

test_that("add_empty_row functionality works", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    stringsAsFactors = FALSE
  )

  # Test adding empty row
  expect_message(
    result <- add_record(df),
    "A single row containing missing values has been added to the data"
  )
  expect_equal(nrow(result), 4)
  expect_true(all(is.na(result[4,])))
})

test_that("add_record handles errors appropriately", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    stringsAsFactors = FALSE
  )

  # Test with wrong number of columns
  expect_error(
    add_record(df, id = 4),
    "New record must contain the same number of columns as the data frame"
  )

  # Test with non-data frame input
  expect_error(
    add_record(list(a = 1, b = 2), id = 4, name = "Alice"),
    "add_record\\(.data = 'must be a data frame'\\)"
  )

  # Test with wrong column names
  result <- add_record(df, wrong_name = 4, another_wrong = "Alice")
  expect_equal(colnames(result), c("id", "name"))
})

test_that("add_record preserves data frame attributes", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    stringsAsFactors = FALSE
  )

  # Test preservation of column classes
  result <- add_record(df, id = 4, name = "Alice")
  expect_equal(class(result$id), "numeric")  # or "numeric" depending on your needs
  expect_equal(class(result$name), "character")

  # Test preservation of column names
  expect_equal(colnames(result), colnames(df))
})

test_that("determine_position helper function works correctly", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    stringsAsFactors = FALSE
  )
  new_row <- data.frame(id = 4, name = "Alice")

  # Test with .before
  result <- determine_position(df, new_row, .before = 2, .after = NULL)
  expect_equal(result$position, 2)

  # Test with .after
  result <- determine_position(df, new_row, .before = NULL, .after = 2)
  expect_equal(result$position, 3)

  # Test with neither
  result <- determine_position(df, new_row, .before = NULL, .after = NULL)
  expect_null(result$position)
  expect_equal(nrow(result$data), nrow(df) + 1)
})
