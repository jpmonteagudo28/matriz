test_that("merge_matrix correctly performs an inner join", {
  df1 <- data.frame(id = c(1, 2, 3), value1 = c("A", "B", "C"))
  df2 <- data.frame(id = c(2, 3, 4), value2 = c("X", "Y", "Z"))

  result <- merge_matrix(df1, df2, by = 'id', all = FALSE)

  expected <- data.frame(id = c(2, 3), value1 = c("B", "C"), value2 = c("X", "Y"))

  expect_equal(result, expected)
})

test_that("merge_matrix correctly performs a full join", {
  df1 <- data.frame(id = c(1, 2, 3), value1 = c("A", "B", "C"))
  df2 <- data.frame(id = c(2, 3, 4), value2 = c("X", "Y", "Z"))

  result <- merge_matrix(df1, df2, by = 'id', all = TRUE)

  expected <- data.frame(
    id = c(1, 2, 3, 4),
    value1 = c("A", "B", "C", NA),
    value2 = c(NA, "X", "Y", "Z")
  )

  expect_equal(result, expected)
})

test_that("merge_matrix throws an error when 'by' column is missing", {
  df1 <- data.frame(id = c(1, 2, 3), value1 = c("A", "B", "C"))
  df2 <- data.frame(other_id = c(2, 3, 4), value2 = c("X", "Y", "Z"))

  expect_error(merge_matrix(df1, df2, by = 'id'), "not found in .data")
})

test_that("merge_matrix removes duplicate columns before merging", {
  df1 <- data.frame(id = c(1, 2, 3), value = c("A", "B", "C"), extra = c(1, 2, 3),extra.1 = c(1,2,3))
  df2 <- data.frame(id = c(2, 3, 4), value = c("X", "Y", "Z"), extra = c(4, 5, 6),extra.1 = c(1,2,3))  # duplicate "extra"

  result <- merge_matrix(df1, df2, by = "id", all = FALSE, remove_dups = TRUE)

  expected <- data.frame(id = c(2, 3), value.x = c("B", "C"), extra.x = c(2, 3), value.y = c("X", "Y"), extra.y = c(4, 5))

  expect_equal(result, expected)
})


test_that("merge_matrix handles unquoted column names (NSE)", {
  df1 <- data.frame(id = c(1, 2, 3), value1 = c("A", "B", "C"))
  df2 <- data.frame(id = c(2, 3, 4), value2 = c("X", "Y", "Z"))

  result <- merge_matrix(df1, df2, by = 'id', all = FALSE)

  expected <- data.frame(id = c(2, 3), value1 = c("B", "C"), value2 = c("X", "Y"))

  expect_equal(result, expected)
})
