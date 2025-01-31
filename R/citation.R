#' Process a Citation Record
#'
#' Takes a record list and a citation string, processes the citation into AMA format,
#' and updates the record with the formatted citation, keywords, and year.
#'
#' @param .record A list containing the record to be updated
#' @param citation A character string containing a BibTeX citation
#'
#' @return An updated list containing the original record with added fields:
#'   \item{citation}{The formatted AMA citation}
#'   \item{keywords}{A vector of keywords from the citation}
#'   \item{year}{The publication year}
#'
#' @importFrom stringr str_replace_all
#' @export
process_citation <- function(.record,citation){

  stopifnot(is.list(.record),
            is.character(citation))

  if(is.data.frame(.record) && nrow(.record) > 1){
    warning("The same citation file will be applied across all rows. Provide a record of class 'list' to process one citation or process batch citation using other functions.")
  }

    bib <- parse_citation(citation)

    # I need to extract the year and keywords
    # put together the citation in AMA format

    processed_citation <- format_ama_citation(bib)


    # Update the .record list with processed data
    .record$citation <- processed_citation$citation
    .record$keywords <- processed_citation$keywords
    .record$year <- processed_citation$year

    return(.record)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Format BibTeX Entry to AMA Citation Style
#'
#' Converts a BibTeX entry into AMA (American Medical Association) citation format.
#' Handles article, book, and miscellaneous entry types.
#'
#' @param bibtex_entry A character string containing a single BibTeX entry
#'
#' @return An object of class c("bibentry", "character", "citation") containing:
#'   \item{string}{The original BibTeX entry}
#'   \item{year}{The publication year as numeric}
#'   \item{citation}{The formatted AMA citation string}
#'   \item{keywords}{A vector of keywords}
#'
#' @examples
#' bibtex <- "@article{key,
#'   author = {Smith J and Jones K},
#'   title = {Example Title},
#'   journal = {Journal Name},
#'   year = {2024},
#'   volume = {1},
#'   pages = {1-10}
#' }"
#' citation <- format_ama_citation(bibtex)
#'
#' @keywords internal
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

  # Format authors (last name + first name + initials)
  if (!is.na(authors)) {
    authors <- strsplit(authors, " and ")[[1]]
    authors <- sapply(authors, function(x) {
      parts <- strsplit(x, " ")[[1]]
      last_name <- utils::tail(parts, 1)
      first_names <- paste(parts[-length(parts)], collapse = " ") # Everything before last name as full first name
      paste(last_name, first_names, sep = " ")
    })
    authors <- paste(authors, collapse = ", ")
  }

  citation <- switch(
    entry_type,
    "article" = paste0(
      authors, ". ", title, ". ",
      if (!is.na(journal)) paste0(journal, ". ") else "",
      if (!is.na(year)) paste0(year, ";") else "",
      if (!is.na(volume)) paste0(volume, ":") else "",
      if (!is.na(pages)) paste0(pages) else "",
      if (!is.na(doi)) paste0(". doi:", doi) else ""
    ),
    "book" = paste0(
      authors, ". ", title, ". ",
      if (!is.na(edition)) paste0(edition, " ed. ") else "",
      if (!is.na(city)) paste0(city, ", ") else "",
      if (!is.na(state)) paste0(state, ": ") else "",
      if (!is.na(publisher)) paste0(publisher, "; ") else "",
      if (!is.na(year)) paste0(year) else "",
      if (!is.na(chapter)) paste0(". Chapter ", chapter, ".") else ""
    ),
    paste0( # Default case (miscellaneous)
      authors, ". ", title, ". ",
      if (!is.na(year)) paste0(year) else "",
      if (!is.na(url)) paste0(". Available at: ", url, ".") else ""
    )
  )

  # Create a vector of keywords
  keywords_vector <- if (!is.na(keywords)) strsplit(keywords, ",\\s*")[[1]] else NA_character_

  # Return results
  citation_object <- structure(
                          list(
                            string = bibtex_entry,
                            year = as.numeric(year),
                            citation = citation,
                            keywords = keywords_vector)
                          )

  invisible(citation_object)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Extract field value from BibTeX entry
#'
#' @description
#' Extracts the value of a specified field from a BibTeX entry string using regular expressions.
#' The function is case-insensitive and handles various spacing patterns around the field
#' delimiter.
#'
#' @param entry Character string containing a BibTeX entry
#' @param field Name of the field to extract (e.g., "title", "author", "year")
#'
#' @return Character string containing the field value if found, NA if the field is not present
#'
#' @details
#' The function searches for patterns of the form "field = \{value\}" in the BibTeX entry,
#' ignoring case and allowing for variable whitespace around the equals sign. The value
#' is expected to be enclosed in curly braces.
#'
#' @keywords internal
#'
extract_field <- function(entry, field) {
  pattern <- paste0("(?i)",field, "\\s*=\\s*\\{(.*?)\\},?")
  matches <- regmatches(entry, regexec(pattern, entry, perl = TRUE))[[1]]
  if (length(matches) > 1) matches[2] else NA_character_
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Parse Citation from File
#'
#' Reads and parses a single BibTeX citation from a file, cleaning up whitespace
#' and formatting.
#'
#' @param entry A character string containing the path to a file with a BibTeX citation
#'
#' @return A character string containing the cleaned and parsed BibTeX entry
#'
#' @examples
#' \dontrun{
#' citation <- parse_citation("path/to/citation.bib")
#' }
#'
#' @keywords internal
parse_citation <- function(entry){

  suppressWarnings({
    bib <- readLines(entry) |>
      stringr::str_replace_all("[^[:graph:]]", " ") |>
      stringr::str_replace_all("=", " = ") |> # add desired spaces
      stringr::str_replace_all("  ", " ")
  })

  bib <- paste(bib, collapse = " ")

  return(bib)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Parse Multiple Citations from File
#'
#' Reads and parses multiple BibTeX citations from a file, handling whitespace
#' and formatting while ensuring proper brace matching.
#'
#' @param entry A character string containing the path to a file with BibTeX citations
#'
#' @return If the file contains a single citation, returns a character string.
#'   If multiple citations are present, returns a character vector of citations.
#'   Returns NULL if no citations are found.
#'
#' @examples
#' \dontrun{
#' citations <- parse_batch_citation("path/to/citations.bib")
#' }
#'
#' @keywords internal
parse_batch_citation <- function(entry) {
  # Read the file and clean up whitespace
  suppressWarnings({
    cit_lines <- readLines(entry) |>
      stringr::str_replace_all("[^[:graph:]]", " ") |>
      stringr::str_replace_all("=", " = ") |>
      stringr::str_replace_all("  ", " ")
  })

citation_starts <- which(stringr::str_detect(cit_lines, "^\\s*@"))

# If no citations found, return NULL
if (length(citation_starts) == 0) {
  warning("No citations found in file: ", entry)
  return(NULL)
}

# Add an end marker for the last citation
citation_ends <- c(citation_starts[-1] - 1, length(cit_lines))

# Process each citation
citations <- vector("character", length(citation_starts))
for (i in seq_along(citation_starts)) {
  # Extract the lines for this citation
  citation_lines <- cit_lines[citation_starts[i]:citation_ends[i]]

  # Check for unmatched braces to ensure complete citation
  open_braces <- sum(stringr::str_count(citation_lines, "\\{"))
  close_braces <- sum(stringr::str_count(citation_lines, "\\}"))

  if (open_braces != close_braces) {
    warning("Unmatched braces in citation ", i, " in file: ", entry)
    next
  }

  # Combine lines into a single string
  citations[i] <- paste(citation_lines, collapse = " ")
}

# Remove any NULL entries from failed parses
citations <- citations[!is.na(citations)]

# If processing a single citation, return a single string
# Otherwise return a vector of citations
if (length(citations) == 1) {
  return(citations[1])
} else {
  return(citations)
}
}


#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Format Multiple BibTeX Entries to AMA Citation Style
#'
#' Processes multiple BibTeX entries and converts them to AMA (American Medical Association)
#' citation format. Handles article, book, and miscellaneous entry types.
#'
#' @param bibtex_entries A character vector containing one or more BibTeX entries
#'
#' @return If given a single entry, returns a single citation object. If given multiple
#'   entries, returns a list of citation objects. Each object is of class
#'   c("bibentry", "character", "citation") containing:
#'   \item{string}{The original BibTeX entry}
#'   \item{year}{The publication year as numeric}
#'   \item{citation}{The formatted AMA citation string}
#'   \item{keywords}{A vector of keywords}
#'
#' @examples
#' entries <- c(
#'   "@article{key1,
#'     author = {Smith J},
#'     title = {First Example},
#'     journal = {Journal One},
#'     year = {2024}
#'   }",
#'   "@book{key2,
#'     author = {Jones K},
#'     title = {Second Example},
#'     publisher = {Publisher},
#'     year = {2024}
#'   }"
#' )
#' citations <- format_batch_ama_citation(entries)
#'
#' @keywords internal
format_batch_ama_citation <- function(bibtex_entries) {
  # Ensure input is a vector
  if (!is.vector(bibtex_entries)) {
    stop("Input must be a vector of BibTeX entries")
  }

  # Process each entry and return a list of citation objects
  citation_objects <- lapply(bibtex_entries, function(bibtex_entry) {
    # Extract fields
    entry_type <- ifelse(grepl("@article", bibtex_entry), "article",
                         ifelse(grepl("@book", bibtex_entry), "book", "misc"))

    # Extract all fields at once to improve efficiency
    fields <- list(
      title = extract_field(bibtex_entry, "title"),
      year = extract_field(bibtex_entry, "year"),
      authors = extract_field(bibtex_entry, "author"),
      keywords = extract_field(bibtex_entry, "keywords"),
      # Journal fields
      journal = extract_field(bibtex_entry, "journal"),
      volume = extract_field(bibtex_entry, "volume"),
      pages = extract_field(bibtex_entry, "pages"),
      doi = extract_field(bibtex_entry, "doi"),
      # Book fields
      edition = extract_field(bibtex_entry, "edition"),
      publisher = extract_field(bibtex_entry, "publisher"),
      city = extract_field(bibtex_entry, "city"),
      state = extract_field(bibtex_entry, "state"),
      chapter = extract_field(bibtex_entry, "chapter"),
      # Misc fields
      url = extract_field(bibtex_entry, "url")
    )

    # Clean DOI if present
    if (!is.na(fields$doi)) {
      fields$doi <- gsub("https://doi.org/", "", fields$doi)
    }

    # Format authors
    formatted_authors <- if (!is.na(fields$authors)) {
      author_list <- strsplit(fields$authors, " and ")[[1]]
      formatted <- vapply(author_list, function(x) {
        parts <- strsplit(x, " ")[[1]]
        last_name <- utils::tail(parts, 1)
        first_names <- parts[-length(parts)]
        first_name_initials <- paste(substr(first_names, 1, 1), collapse = " ")
        paste(last_name, first_names, sep = " ")
      }, character(1))
      paste(formatted, collapse = ", ")
    } else {
      NA_character_
    }

    # Format citation based on entry type using switch for better readability
    citation <- switch(
      entry_type,
      "article" = paste0(
        formatted_authors, ". ",
        fields$title, ". ",
        if (!is.na(fields$journal)) paste0(fields$journal, ". ") else "",
        if (!is.na(fields$year)) paste0(fields$year, ";") else "",
        if (!is.na(fields$volume)) paste0(fields$volume, ":") else "",
        if (!is.na(fields$pages)) paste0(fields$pages) else "",
        if (!is.na(fields$doi)) paste0(". doi:", fields$doi) else ""
      ),
      "book" = paste0(
        formatted_authors, ". ",
        fields$title, ". ",
        if (!is.na(fields$edition)) paste0(fields$edition, " ed. ") else "",
        if (!is.na(fields$city)) paste0(fields$city, ", ") else "",
        if (!is.na(fields$state)) paste0(fields$state, ": ") else "",
        if (!is.na(fields$publisher)) paste0(fields$publisher, "; ") else "",
        fields$year,
        if (!is.na(fields$chapter)) paste0(". Chapter ", fields$chapter, ".") else ""
      ),
      # Default case (misc)
      paste0(
        formatted_authors, ". ",
        fields$title, ". ",
        fields$year,
        if (!is.na(fields$url)) paste0(". Available at: ", fields$url, ".") else ""
      )
    )

    # Create keywords vector
    keywords_vector <- if (!is.na(fields$keywords)) {
      strsplit(fields$keywords, ",\\s*")[[1]]
    } else {
      NA_character_
    }

    # Create and return citation object
    structure(
      list(
        string = bibtex_entry,
        year = as.numeric(fields$year),
        citation = citation,
        keywords = keywords_vector
      )
    )
  })

  # If single entry, return single object, otherwise return list
  if (length(citation_objects) == 1) {
    invisible(citation_objects[[1]])
  } else {
    invisible(citation_objects)
  }
}
