
test_that("add_batch_record adds a record to an empty data frame", {
  empty_df <- data.frame(a = numeric(0), b = character(0))
  new_record <- list(a = 1, b = "test")

  result <- add_batch_record(empty_df, new_record)
  expected <- data.frame(a = 1, b = "test", stringsAsFactors = FALSE)

  expect_equal(result, expected)
})

test_that("add_batch_record adds multiple records to a data frame", {
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  new_records <- list(
    list(a = 3, b = "z"),
    list(a = 4, b = "w")
  )

  result <- add_batch_record(df, new_records)
  expected <- data.frame(
    a = 1:4,
    b = c("x", "y", "z", "w"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("add_batch_record handles .before and .after arguments", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  new_record <- list(a = 99, b = "new")

  result_before <- add_batch_record(df, new_record, .before = 2)
  expected_before <- data.frame(
    a = c(1, 99, 2, 3),
    b = c("x", "new", "y", "z"),
    stringsAsFactors = FALSE
  )
  expect_equal(result_before, expected_before)

  result_after <- add_batch_record(df, new_record, .after = 2)
  expected_after <- data.frame(
    a = c(1, 2, 99, 3),
    b = c("x", "y", "new", "z"),
    stringsAsFactors = FALSE
  )
  expect_equal(result_after, expected_after)
})

test_that("add_batch_record returns an error for invalid inputs", {
  df <- data.frame(a = 1, b = "x", stringsAsFactors = FALSE)

  expect_error(add_batch_record("not_a_data_frame", list(a = 2, b = "y")),
               "must be a data frame")

  expect_error(add_batch_record(df, list(a = 2)),
               "Input list length \\(1\\) does not match data frame columns \\(2\\)")

  invalid_df <- data.frame(c = 2, stringsAsFactors = FALSE)
  expect_error(add_batch_record(df, invalid_df),
               "Data frame input must have the same number of columns as .data")
})

test_that("add_batch_record adds an empty row when no arguments are provided", {
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)

  result <- add_batch_record(df)
  expected <- rbind(df, data.frame(a = NA, b = NA, stringsAsFactors = FALSE))

  expect_equal(result, expected)
})
