
# Test cases for same_length
test_that("same_length works with non-lists", {
  expect_true(same_length(1:5, 6:10))               # Equal-length vectors
  expect_false(same_length(1:5, c(1, 2)))          # Unequal-length vectors
  expect_true(same_length(c("a", "b"), c("x", "y"))) # Equal-length character vectors
})

test_that("same_length works with lists", {
  expect_true(same_length(list(1, 2:3), list(4, 5:6))) # Lists with identical element lengths
  expect_true(same_length(list(1, 2:3), list(4, 5:7))) # Lists with differing element lengths
  expect_true(same_length(list(), list()))              # Empty lists
  expect_false(same_length(list(1, 2), list(1)))        # Lists with different lengths
})

test_that("same_length works with data frames", {
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(a = 7:9, b = 10:12)
  expect_true(same_length(df1, df2)) # Data frames with the same number of columns
  df3 <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  expect_false(same_length(df1, df3)) # Data frames with different numbers of columns
})

test_that("same_length works with list and data frame combinations", {
  df <- data.frame(a = 1:2, b = 3:4)
  expect_true(same_length(list(1, 2), df))           # List matches number of columns in df
  expect_false(same_length(list(1, 2, 3), df))       # List does not match number of columns in df
  expect_true(same_length(data.frame(a = 1, b = 2), list(1, 2))) # Data frame matches list
})

test_that("same_length works with edge cases", {
  expect_true(same_length(c(), c()))                 # Empty vectors
  expect_true(same_length(list(), list()))           # Empty lists
  expect_false(same_length(c(), list(1)))            # Empty vector vs. non-empty list
  expect_true(same_length(list(), data.frame()))    # Empty list vs. empty data frame
  expect_true(same_length(data.frame(), data.frame())) # Empty data frames
  expect_false(same_length(1:5, NULL)) # NULL input
  expect_true(same_length(NULL, NULL))
  expect_false(same_length(list(), NULL))
  })

test_that("same_length handles mixed type edge cases", {
  expect_false(same_length(data.frame(), list(1)))
  df_zero <- data.frame(a=numeric(0), b=character(0))
  expect_false(same_length(df_zero, list()))
  expect_true(same_length(list(NULL,NULL), list(1, 2)))
})

test_that("same_length handles invalid inputs appropriately", {
  f <- function(x) x + 1
  expect_error(same_length(f, 1:3))
})

test_that("same_length works with special objects", {
  expect_true(same_length(factor(1:3), 1:3))
  dates1 <- as.Date(c("2024-01-01", "2024-01-02"))
  dates2 <- as.Date(c("2024-02-01", "2024-02-02"))
  expect_true(same_length(dates1, dates2))
  x <- 1:3
  attr(x, "custom") <- "test"
  y <- 4:6
  expect_true(same_length(x, y))
})

test_that("same_length works with matrices", {
  mat1 <- matrix(1:6, nrow=2)
  mat2 <- matrix(7:12, nrow=3)
  expect_true(same_length(mat1, mat2))

  # Matrix vs data frame
  df <- data.frame(a=1:3, b=4:6)
  expect_false(same_length(mat1, df))
})

test_that("same_length works with nested and complex lists", {
  # Nested lists with different structures
  list1 <- list(a = list(1:2, 3), b = list(4:5))
  list2 <- list(x = list(1, 2:3), y = list(4:5))
  expect_true(same_length(list1, list2))

  # Lists with mixed content types
  list3 <- list(1:3, data.frame(x=1:2), list(a=1))
  list4 <- list(c("a","b","c"), data.frame(y=3:4), list(b=2))
  expect_true(same_length(list3, list4))
})
