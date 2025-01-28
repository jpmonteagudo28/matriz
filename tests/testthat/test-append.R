test_that("append_column basic functionality works", {
  # Create test data
  df <- data.frame(
    a = 1:3,
    b = letters[1:3],
    stringsAsFactors = FALSE
  )
  new_values <- 4:6

  # Test simple append (no position specified)
  result <- append_column(df, new_values)
  expect_equal(ncol(result), ncol(df) + 1)
  expect_equal(names(result)[ncol(result)], "new_values")
  expect_equal(result$new_values, 4:6)
})

test_that("append_column position specifications work", {
  df <- data.frame(
    a = 1:3,
    b = letters[1:3],
    stringsAsFactors = FALSE
  )
  new_values <- 4:6

  # Test adding before first column
  result <- append_column(df, new_values, .before = "a")
  expect_equal(names(result), c("new_values", "a", "b"))

  # Test adding after first column
  result <- append_column(df, new_values, .after = "a")
  expect_equal(names(result), c("a", "new_values", "b"))

  # Test adding before last column
  result <- append_column(df, new_values, .before = "b")
  expect_equal(names(result), c("a", "new_values", "b"))

  # Test adding after last column
  result <- append_column(df, new_values, .after = "b")
  expect_equal(names(result), c("a", "b", "new_values"))
})

test_that("append_column handles errors appropriately", {
  df <- data.frame(
    a = 1:3,
    b = letters[1:3],
    stringsAsFactors = FALSE
  )
  new_values <- 4:6

  # Test with non-existent column name
  expect_error(
    append_column(df, new_values, .before = "nonexistent"),
    "Column name not found"
  )

  # Test with non-data frame input
  expect_error(
    append_column(list(a = 1:3), new_values),
    "is\\.data\\.frame\\(.data\\) is not TRUE"
  )

  # Test with wrong length vector
  wrong_length <- 1:4
  expect_error(
    append_column(df, wrong_length),
    "Length of new column must match number of rows in data frame"
  )
})

test_that("append_column preserves data frame attributes", {
  df <- data.frame(
    a = 1:3,
    b = letters[1:3],
    stringsAsFactors = FALSE
  )
  new_values <- 4:6

  result <- append_column(df, new_values)

  # Test preservation of row count
  expect_equal(nrow(result), nrow(df))

  # Test preservation of row names
  expect_equal(rownames(result), rownames(df))

  # Test preservation of original column types
  expect_equal(class(result$a), class(df$a))
  expect_equal(class(result$b), class(df$b))
})

test_that("append_column handles edge cases", {
  df <- data.frame(
    a = 1:3,
    b = letters[1:3],
    stringsAsFactors = FALSE
  )
  new_values <- 4:6

  # Test with empty data frame
  empty_df <- data.frame()
  expect_error(
    append_column(empty_df, new_values),
    "Input data frame is empty"
  )

  # Test with single-column data frame
  single_col_df <- data.frame(a = 1:3)
  result <- append_column(single_col_df, new_values)
  expect_equal(ncol(result), 2)

  # Test adding multiple columns sequentially
  result <- df
  for(i in 1:3) {
    new_col <- rep(i, nrow(df))
    result <- append_column(result, new_col)
  }
  expect_equal(ncol(result), ncol(df) + 1)
})

test_that("append_column naming behavior works", {
  df <- data.frame(
    a = 1:3,
    b = letters[1:3],
    stringsAsFactors = FALSE
  )

  # Test with different variable names
  x <- 4:6
  result <- append_column(df, x)
  expect_true("x" %in% names(result))

  # Test with numeric vector
  result <- append_column(df, 7:9)
  expect_true(any(grepl("7:9", names(result))))
})
