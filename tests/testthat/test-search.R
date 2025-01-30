# Sample data
df <- data.frame(
  id = 1:5,
  name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  score = c(90, 85, 88, 92, 75),
  row.names = NULL
)

test_that("Filters rows across all columns when column is NULL", {
  result <- search_record(df, where = score > 85)
  rownames(result) <- NULL
  expected <- data.frame(id = c(1, 3, 4), name = c("Alice", "Charlie", "David"), score = c(90, 88, 92))
  expect_equal(result, expected)
})

test_that("Applies the where condition across all columns", {
  result <- search_record(df, where = name == "Alice" & score > 80)
  expected <- data.frame(id = 1, name = "Alice", score = 90)
  expect_equal(result, expected)
})

test_that("Errors when column is out of range", {
  expect_error(search_record(df, column = 10), "Column index is out of range")
})

test_that("Errors when column does not exist", {
  expect_error(search_record(df, column = "nonexistent"), "does not exist in the data frame")
})

test_that("Errors when where condition is invalid", {
  expect_error(search_record(df, column = "name", where = "invalid_condition"),
               "must evaluate to a logical vector")
})

test_that("Returns empty data frame when no rows match where", {
  result <- search_record(df, column = "name", where = score < 70)
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), "name")
})
