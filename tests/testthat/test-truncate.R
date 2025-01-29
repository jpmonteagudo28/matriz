test_that("truncate removes all rows by default", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  result <- truncate(df)

  expect_true(nrow(result) == 0)
  expect_identical(names(result), names(df)) # Column names should be preserved
})

test_that("truncate keeps column structure intact", {
  df <- data.frame(a = numeric(3), b = character(3), c = logical(3))
  result <- truncate(df)

  expect_true(nrow(result) == 0)
  expect_identical(names(result), names(df))
  expect_identical(class(result), class(df))
})

test_that("truncate replaces values with NA when keep_rows = TRUE", {
  df <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"), c = c(TRUE, FALSE, TRUE))
  result <- truncate(df, keep_rows = TRUE)

  expect_true(nrow(result) == 3) # Should keep row count
  expect_true(all(is.na(result))) # All values should be NA
})

test_that("truncate handles empty data frames", {
  df <- data.frame(a = numeric(0), b = character(0))
  result <- truncate(df)

  expect_true(nrow(result) == 0) # Should still be empty
  expect_identical(names(result), names(df))
})

test_that("truncate handles single-column data frames", {
  df <- data.frame(a = 1:5)
  result <- truncate(df)

  expect_true(nrow(result) == 0)
  expect_identical(names(result), names(df))
})

test_that("truncate handles single-row data frames", {
  df <- data.frame(a = 1, b = "x")
  result <- truncate(df)

  expect_true(nrow(result) == 0)
  expect_identical(names(result), names(df))
})

test_that("truncate works with factors", {
  df <- data.frame(a = factor(c("low", "medium", "high")), b = 1:3)
  result <- truncate(df, keep_rows = TRUE)

  expect_true(nrow(result) == 3) # Should keep row count
  expect_true(all(is.na(result))) # All values should be NA
})

test_that("truncate does not affect original data frame", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  df_copy <- df
  result <- truncate(df)

  expect_identical(df, df_copy) # Ensure df remains unchanged
})
