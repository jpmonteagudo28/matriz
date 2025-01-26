#' @importFrom stringr str_replace_all
#'
process_citation <- function(.record,citation,...){

  stopifnot(is.list(.record),
            is.character(citation))

    bib <- suppressWarnings({
      readLines(citation) |>
      stringr::str_replace_all("[^[:graph:]]", " ") |>
      stringr::str_replace_all("=", " = ") |> # add desired spaces
      stringr::str_replace_all("  ", " ")
      })    # remove double spaces in case you have it

    bib <- paste(bib, collapse = " ")

    # I need to extract the year and keywords
    # put together the citation in AMA format

    processed_citation <- format_ama_citation(bib)


    # Update the .record list with processed data
    .record$citation <- processed_citation$citation
    .record$keywords <- processed_citation$keywords
    .record$year <- processed_citation$year

    return(.record)
}


format_ama_citation <- function(bibtex_entry) {

  # Extract fields
  entry_type <- ifelse(grepl("@article", bibtex_entry), "article",
                       ifelse(grepl("@book", bibtex_entry), "book", "misc"))
  title <- extract_field(bibtex_entry, "title")
  year <- extract_field(bibtex_entry, "year")
  authors <- extract_field(bibtex_entry, "author")
  keywords <- extract_field(bibtex_entry, "keywords")

  # Journal-specific fields
  journal <- extract_field(bibtex_entry, "journal")
  volume <- extract_field(bibtex_entry, "volume")
  pages <- extract_field(bibtex_entry, "pages")
  doi <- gsub("https://doi.org/", "", extract_field(bibtex_entry, "doi"))

  # Book-specific fields
  edition <- extract_field(bibtex_entry, "edition")
  publisher <- extract_field(bibtex_entry, "publisher")
  city <- extract_field(bibtex_entry, "city")
  state <- extract_field(bibtex_entry, "state")
  chapter <- extract_field(bibtex_entry, "chapter")

  # Miscellaneous-specific fields
  url <- extract_field(bibtex_entry, "url")

  # Format authors (last name + initials)
  if (!is.na(authors)) {
    authors <- strsplit(authors, " and ")[[1]]
    authors <- sapply(authors, function(x) {
      parts <- strsplit(x, " ")[[1]]
      last_name <- tail(parts, 1)
      initials <- paste(substr(parts[-length(parts)], 1, 1), collapse = "")
      paste(last_name, initials, sep = " ")
    })
    authors <- paste(authors, collapse = ", ")
  }

  # Format citation based on entry type
  if (entry_type == "article") {
    citation <- paste0(authors, ". ", title, ". ", journal, ". ", year, ";", volume, ":", pages, ". doi:", doi)
  } else if (entry_type == "book") {
    citation <- paste0(authors, ". ", title, ". ", edition, " ed. ", city, ", ", state, ": ", publisher, "; ", year,
                       if (!is.na(chapter)) paste0(". Chapter ", chapter, "."), "")
  } else { # Miscellaneous
    citation <- paste0(authors, ". ", title, ". ", year,
                       if (!is.na(url)) paste0(". Available at: ", url, "."), "")
  }

  # Create a vector of keywords
  keywords_vector <- if (!is.na(keywords)) strsplit(keywords, ",\\s*")[[1]] else c()

  # Return results
  citation_object <- structure(
                          list(
                            string = bibtex_entry,
                            year = as.numeric(year),
                            citation = citation,
                            keywords = keywords_vector),
                            class = "citation")

  invisible(citation_object)
}

# Helper function to extract a field
extract_field <- function(entry, field) {
  pattern <- paste0(field, "\\s*=\\s*\\{(.*?)\\},?")
  matches <- regmatches(entry, regexec(pattern, entry))
  if (length(matches[[1]]) > 1) return(matches[[1]][2]) else return(NA)
}
