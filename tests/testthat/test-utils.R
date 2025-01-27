
# Test cases for same_length
test_that("same_length works with non-lists", {
  expect_true(same_length(1:5, 6:10))               # Equal-length vectors
  expect_false(same_length(1:5, c(1, 2)))          # Unequal-length vectors
  expect_true(same_length(c("a", "b"), c("x", "y"))) # Equal-length character vectors
})

test_that("same_length works with lists", {
  expect_true(same_length(list(1, 2:3), list(4, 5:6))) # Lists with identical element lengths
  expect_false(same_length(list(1, 2:3), list(4, 5:7))) # Lists with differing element lengths
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
  expect_false(same_length(list(), data.frame()))    # Empty list vs. empty data frame
  expect_true(same_length(data.frame(), data.frame())) # Empty data frames
})

test_that("same_length handles invalid inputs gracefully", {
  expect_error(same_length(1:5, NULL), "non-numeric argument") # NULL input
  expect_error(same_length(NULL, NULL), "non-numeric argument")
  expect_error(same_length(list(), NULL), "non-numeric argument")
})
