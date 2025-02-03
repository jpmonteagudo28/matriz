#' Add Multiple Records to a literature matrix
#'
#' Adds one or more records to a literature matrix at a specified position. Records can be
#' provided as lists or data frames, and can be inserted before or after specific rows.
#'
#' @param .data A data frame to which records will be added
#' @param ... One or more records to add. Each record can be either:
#'   \itemize{
#'     \item A list with the same length as the number of columns in `.data`
#'     \item A data frame with the same column structure as `.data`
#'   }
#' @param .before Row number before which to insert the new records.
#'   If NULL (default), and `.after` is also NULL, records are appended to the end.
#' @param .after Row number after which to insert the new records.
#'   If NULL (default), and `.before` is also NULL, records are appended to the end.
#'
#' @return A data frame with the new records added at the specified position
#'
#' @examples
#' \dontrun{
#' # Create sample data frame
#' df <- data.frame(
#'   name = c("John", "Jane"),
#'   age = c(25, 30)
#' )
#'
#' # Add a single record as a list
#' df <- add_batch_record(df, list(name = "Bob", age = 35))
#'
#' # Add multiple records as data frames
#' new_records <- data.frame(
#'   name = c("Alice", "Charlie"),
#'   age = c(28, 40)
#' )
#' df <- add_batch_record(df, new_records, .before = 2)}
#'
#' @export
add_batch_record <- function(.data,
                      ...,
                      .before = NULL,
                      .after = NULL) {

  if(!is.data.frame(.data)){
    stop("add_record(.data = 'must be a data frame')")
  }

  if (dots_n(...) == 0L) {
    .data <- add_empty_row(.data)
    return(.data)
  }

  dots <- list(...)

    new_records <- do.call(dplyr::bind_rows, lapply(dots, function(dot) {

      if(is_nested_list(dot)){
        if (!same_length(dot,.data)) {
          stop(sprintf(
            "Input list length (%d) does not match data frame columns (%d)",
            length(dot),
            ncol(.data)
          ))
        }

        # Convert vector to data frame while preserving column types
        df_new <- do.call(dplyr::bind_rows,
                          lapply(dot,
                                  as.data.frame,
                                  stringsAsFactors = FALSE)
                          )

      } else if(is.list(dot) && !is.data.frame(dot)) {
              if (!same_length(dot,.data)) {
                stop(sprintf(
                    "Input list length (%d) does not match data frame columns (%d)",
                     length(dot),
                    ncol(.data)
                        )
                    )
        }

        # Convert vector to data frame while preserving column types
        df_new <- as.data.frame(dot,
                                  stringsAsFactors = FALSE
                                )

        colnames(df_new) <- colnames(.data)

      } else if (is.data.frame(dot)) {
        if (!same_column(dot, .data)) {
          stop("Data frame input must have the same number of columns as .data")
        }
        if (!equal_names(dot, .data)) {
          colnames(dot) <- colnames(.data)
        }
        df_new <- dot
      } else {
        stop("Each input must be either a list or a data frame")
      }

      return(df_new)
    }))

    if (!identical(colnames(.data), colnames(new_records))) {
      stop("Column names of new_records do not match .data")
    }

    result <- determine_position(.data, new_records, .before, .after)
    .data <- result$data
    position <- result$position

    if (is.null(position)) {
      return(.data) # Return early if rows were appended
    }

    .data <- dplyr::bind_rows(
      .data[seq_len(position - 1), , drop = FALSE],
      new_records,
      .data[seq(position, nrow(.data)), , drop = FALSE]
    )

    # Reset row names to avoid duplicate or mismatched names
    row.names(.data) <- NULL

    return(.data)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Process Multiple BibTeX Citations and Update Literature Matrix
#'
#' Reads multiple BibTeX citations from files and updates the corresponding rows in a
#' literature matrix with formatted citations, keywords, and years.
#'
#' @param .data A data frame containing at least three columns:
#'   \itemize{
#'     \item citation: Character column for formatted citations
#'     \item keywords: List column for citation keywords
#'     \item year: Numeric column for publication years
#'   }
#' @param citations Character vector of file paths to BibTeX citation files
#' @param where Numeric vector indicating which rows to update. If NULL (default),
#'   all rows will be updated.
#'
#' @return A data frame with updated citation information in the specified rows
#'
#' @examples
#' # Create sample data frame
#' df <- data.frame(
#'   citation = character(2),
#'   keywords = I(list(NULL, NULL)),
#'   year = numeric(2)
#' )
#'
#' \dontrun{
#' # Process citations from files
#' df <- process_batch_citation(
#'   df,
#'   citations = c("citation1.bib", "citation2.bib")
#' )
#'
#' # Update specific rows
#' df <- process_batch_citation(
#'   df,
#'   citations = "new_citation.bib",
#'   where = c(TRUE, FALSE)
#' )
#' }
#'
#' @seealso \code{\link{format_batch_ama_citation}}, \code{\link{parse_batch_citation}}
#' @export
process_batch_citation <- function(.data, citations, where = NULL) {
  # Input validation
  stopifnot(
    is.data.frame(.data),
    is.character(citations),
    all(file.exists(citations))
  )

  bib <- parse_batch_citation(citations)

  # I need to extract the year and keywords
  # put together the citation in AMA format

  processed_citations <- format_batch_ama_citation(bib)

  # Create temporary vectors to hold the new values
  new_citations <- .data$citation
  new_keywords <- .data$keywords
  new_years <- .data$year

  citation_idx <- 1
  where <- if (is.null(where)) rep(TRUE, nrow(.data)) else where

  # Update the vectors where the condition is TRUE
  for (i in seq_len(nrow(.data))) {
    if (where[i]) {
      new_citations[i] <- processed_citations[[citation_idx]]$citation
      new_keywords[i] <- list(processed_citations[[citation_idx]]$keywords)
      new_years[i] <- processed_citations[[citation_idx]]$year
      citation_idx <- citation_idx + 1
    }
  }

  # Update the data frame columns
  .data <- update_record(.data, "citation", where = where, set_to = new_citations)
  .data <- update_record(.data, "keywords", where = where, set_to = new_keywords)
  .data <- update_record(.data, "year", where = where, set_to = new_years)

  return(.data)
}
