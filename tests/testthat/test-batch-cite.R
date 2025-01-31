# Mock file creation for testing
create_mock_bib_file <- function(content, file_name = tempfile(fileext = ".bib")) {
  writeLines(content, file_name)
  return(file_name)
}

# Tests for parse_citation
test_that("parse_citation correctly parses a single BibTeX entry", {
  mock_file <- create_mock_bib_file("@article{key1, author= {Smith J}, title= {Example}, year= {2024} }")
  parsed <- parse_citation(mock_file)
  expect_type(parsed, "character")
  expect_match(parsed, "@article")
  expect_match(parsed, "author = \\{Smith J\\}")
})

# Tests for parse_batch_citation
test_that("parse_batch_citation correctly parses multiple BibTeX entries", {
  mock_file <- create_mock_bib_file(
  "@article{key1,
    author= {Smith J},
    title= {Example One},
    year= {2024}
  }

  @book{key2,
    author= {Jones K},
    title= {Example Two},
    publisher= {Publisher},
    year= {2023}
  }
  "
  )
  parsed <- parse_batch_citation(mock_file)
  expect_type(parsed, "character")
  expect_length(parsed, 2)
  expect_match(parsed[1], "@article")
  expect_match(parsed[2], "@book")
})

test_that("parse_batch_citation returns NULL for empty files", {
  mock_file <- create_mock_bib_file("")
  expect_warning(result <- parse_batch_citation(mock_file), "No citations found")
  expect_null(result)
})

test_that("parse_batch_citation warns for unmatched braces", {
  mock_file <- create_mock_bib_file(
    "@article{key1,
    author= {Smith J,
    title= {Example One},
    year= {2024}
  ")
  expect_warning(parse_batch_citation(mock_file), "Unmatched braces")
})

# Tests for format_batch_ama_citation
test_that("format_batch_ama_citation formats article citations correctly", {
  entries <- c(
    "@article{key1,
    author= {Smith J},
    title= {Example Article},
    journal= {Journal One},
    year= {2024},
    volume= {10},
    pages= {100-110},
    doi= {10.1234/example}
  }")

  parsed <- format_batch_ama_citation(entries)
  expect_type(parsed, "list")
  expect_match(parsed$citation, "J Smith. Example Article. Journal One. 2024;10:100-110. doi:10.1234/example")
})

test_that("format_batch_ama_citation formats book citations correctly", {
  entries <- c(
    "@book{key2,
    author= {Jones K},
    title= {Example Book},
    publisher= {Publisher},
    year= {2023},
    edition= {2nd}
  }
  ")
  parsed <- format_batch_ama_citation(entries)
  expect_type(parsed, "list")
  expect_match(parsed$citation, "K Jones. Example Book. 2nd ed. Publisher; 2023")
})

test_that("format_batch_ama_citation handles missing fields gracefully", {
  entries <- c(
    "@article{key3,
    author= {Brown L},
    title= {No Journal Name},
    year= {2022}
  }
  ")
  parsed <- format_batch_ama_citation(entries)
  expect_type(parsed, "list")
  expect_match(parsed$citation, "L Brown. No Journal Name. 2022")
})
