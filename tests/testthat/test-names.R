test_that("matriz_name displays default names and classes",{

# Add extra column
 default <- names(init_matrix(test))
 name_test <- rownames(matriz_names(test))

 # Use defaults
 default_cols <- names(init_matrix())
 test2<- rownames(matriz_names())

 expect_equal(default,name_test)
 expect_equal(default_cols,test2)
})

test_that("matriz_names returns a data frame", {
  result <- matriz_names(test)
  expect_s3_class(result, "data.frame")
})

test_that("matriz_names has the correct number of rows", {
  # The number of rows in the output should equal the number of columns in the matrix returned by init_matrix(test)
  test_matrix <- init_matrix(test)
  result <- matriz_names(test)
  expect_equal(nrow(result), length(names(test_matrix)))
})

test_that("matriz_names correctly handles extra arguments", {

  extra_result <- matriz_names(extra_arg = "value")
  test_matrix <- init_matrix(extra_arg = "value")
  expected_classes <- sapply(test_matrix, class)
  class_test <- data.frame(expected_classes,extra_result$class, row.names = NULL)

  expect_equal(class_test[,1],class_test[,2])
})
