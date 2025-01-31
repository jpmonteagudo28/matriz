# # Mock file creation for testing
# create_mock_bib_file <- function(content, file_name = tempfile(fileext = ".bib")) {
#   writeLines(content, file_name)
#   return(file_name)
# }
#
# test_that("process_batch_citation updates citations correctly", {
#   mock_file <- create_mock_bib_file("@article{key1, author= {Smith J}, title= {Example}, year= {2024} }")
#
#   .data <- data.frame(
#     citation = (NA_character_),
#     keywords = I(list(character(), character())),  # Ensures correct list column initialization
#     year = (NA_integer_),
#     stringsAsFactors = FALSE
#   )
#
#   updated_data <- process_batch_citation(.data, citations = mock_file)
#
#   expect_type(updated_data, "list")
#   expect_equal(updated_data$citation[1], "J Smith. Example Article. Journal One. 2024;10:100-110. doi:10.1234/example")
#   expect_equal(updated_data$keywords[[1]], c("example", "article"))
#   expect_equal(updated_data$year[1], 2024)
# })
#
# test_that("process_batch_citation handles multiple citations", {
#   mock_files <- c(
#     create_mock_bib_file("@article{key1, author= {Smith J}, title= {Example One}, year= {2024} }"),
#     create_mock_bib_file("@book{key2, author= {Jones K}, title= {Example Two}, year= {2023} }")
#   )
#
#   .data <- data.frame(
#     citation = c(NA, NA),
#     keywords = vector("list", 2),
#     year = c(NA, NA),
#     stringsAsFactors = FALSE
#   )
#
#   updated_data <- process_batch_citation(.data, citations = mock_files)
#
#   expect_equal(updated_data$citation[1], "Smith J. Example Article. Journal One. 2024;10:100-110. doi:10.1234/example")
#   expect_equal(updated_data$citation[2], "Jones K. Example Book. 2nd ed. Publisher; 2023")
#   expect_equal(updated_data$keywords[[2]], c("example", "book"))
#   expect_equal(updated_data$year[2], 2023)
# })
#
# test_that("process_batch_citation applies condition from 'where' argument", {
#   mock_file <- create_mock_bib_file("@article{key1, author= {Smith J}, title= {Example}, year= {2024} }")
#
#   .data <- data.frame(
#     citation = c(NA, NA),
#     keywords = vector("list", 2),
#     year = c(NA, NA),
#     stringsAsFactors = FALSE
#   )
#
#   where_condition <- c(FALSE, TRUE)
#   updated_data <- process_batch_citation(.data, citations = mock_file, where = where_condition)
#
#   expect_true(is.na(updated_data$citation[1])) # First row should remain unchanged
#   expect_equal(updated_data$citation[2], "Smith J. Example Article. Journal One. 2024;10:100-110. doi:10.1234/example")
#   expect_equal(updated_data$year[2], 2024)
# })
#
# test_that("process_batch_citation handles missing citation files gracefully", {
#   .data <- data.frame(
#     citation = c(NA, NA),
#     keywords = vector("list", 2),
#     year = c(NA, NA),
#     stringsAsFactors = FALSE
#   )
#
#   expect_error(process_batch_citation(.data, citations = "nonexistent.bib"), "all\\(file\\.exists\\(citations\\)\\) is not TRUE")
# })
