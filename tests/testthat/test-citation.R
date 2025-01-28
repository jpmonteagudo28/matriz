
# Mock BibTeX entries for testing
mock_article <- "@article{key,
  author = {Smith J and Jones K},
  title = {Example Title},
  journal = {Journal Name},
  year = {2024},
  volume = {1},
  pages = {1-10},
  doi = {https://doi.org/10.1234/example}
}"

mock_book <- "@book{key,
  author = {Doe J},
  title = {Example Book Title},
  publisher = {Book Publisher},
  year = {2020},
  edition = {2nd},
  city = {New York},
  state = {NY}
}"

mock_record <-list(
                  citation = NULL,
                  keywords = NULL,
                  year = NULL
                  )

test_that("format_ama_citation processes articles correctly", {
  entry <-  format_ama_citation(mock_article)

  expect_s3_class(entry,  "character")
  expect_equal(entry$year, 2024)
  expect_true(grepl("Smith J, Jones K\\. Example Title\\. Journal Name\\. 2024;1:1-10\\. doi:10\\.1234/example", entry$citation))
  expect_equal(entry$keywords, NULL) # No keywords in the mock_article
})

test_that("format_ama_citation processes books correctly", {
  entry <- format_ama_citation(mock_book)

  expect_s3_class(entry, c("bibentry", "character", "citation"))
  expect_equal(entry$year, 2020)
  expect_true(grepl("Doe J\\. Example Book Title\\. 2nd ed\\. New York, NY: Book Publisher; 2020", entry$citation))
  expect_equal(entry$keywords, NULL) # No keywords in the mock_book
})

test_that("process_citation updates .record correctly", {
  updated_record <- process_citation(mock_record, mock_article)

  expect_equal(updated_record$id, 1)
  expect_equal(updated_record$year, 2024)
  expect_true(grepl("Smith J, Jones K\\. Example Title\\. Journal Name\\. 2024;1:1-10\\. doi:10\\.1234/example", updated_record$citation))
})

test_that("extract_field extracts fields correctly", {
  expect_equal(extract_field(mock_article, "author"), "Smith J and Jones K")
  expect_equal(extract_field(mock_article, "title"), "Example Title")
  expect_equal(extract_field(mock_article, "year"), "2024")
  expect_equal(extract_field(mock_article, "doi"), "https://doi.org/10.1234/example")
})

test_that("parse_citation parses a single BibTeX entry from file", {
  temp_file <- tempfile(fileext = ".bib")
  writeLines(mock_article, temp_file)

  parsed <- parse_citation(temp_file)

  expect_equal(parsed, mock_article)
  unlink(temp_file) # Clean up
})

test_that("parse_batch_citation parses multiple BibTeX entries", {
  temp_file <- tempfile(fileext = ".bib")
  writeLines(c(mock_article, mock_book), temp_file)

  parsed <- parse_batch_citation(temp_file)

  expect_equal(length(parsed), 2)
  expect_equal(parsed[1], mock_article)
  expect_equal(parsed[2], mock_book)
  unlink(temp_file) # Clean up
})
