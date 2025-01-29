test_that("matriz_message prints expected output", {
  expected_output <- paste0(
    "-------------------------\n",
    "-------------------------\n",
    "Easily generate and manage structured literature review matrices in R.\n",
    "Streamline your research synthesis, track key study details, and organize citations efficiently."
  )

  expect_output(matriz_message(), "-------------------------\n")
  expect_output(matriz_message(), expected_output)
})
