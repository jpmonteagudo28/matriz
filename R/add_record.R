#' Add a Record to a Data Frame
#'
#' @description
#' Adds a new row to a data frame at a specified position
#'
#' @param .data A data frame to which a record will be added
#' @param ... New record to be added (vector, list, or data frame)
#' @param .before Optional. Row number before which to insert the new record
#' @param .after Optional. Row number after which to insert the new record
#'
#' @return Modified data frame with the new record inserted
#'
#' @examples
#' df <- data.frame(x = 1:3, y = 4:6)
#' add_record(df, c(4, 7))
#' add_record(df, c(4, 7), .before = 2)
#'
#' @export

add_record <- function(.data,
                       ...,
                       .before = NULL,
                       .after = NULL){

  if(!is.data.frame(.data)){
    stop("add_record(.data = 'must be a data frame')")
  }

  if (dots_n(...) == 0L) {
    .data <- add_empty_row(.data)
    return(.data)
  } else {

    dots <- list(...)

    if(dots_n(...) == 1 && is.vector(dots[[1]]) &&
       length(dots[[1]]) == ncol(.data)){

      df <- data.frame(matrix(dots[[1]], nrow = 1))
      colnames(df) <- colnames(.data)
    }else {
      # Convert dots to data frame
      df <- data.frame(..., stringsAsFactors = FALSE)

      # Ensure the new record has the same number of columns
      if (!same_column(.data,df)) {
        stop("New record must contain the same number of columns as the data frame.")
      }

      # Assign column names from the existing data
      if (!equal_names(.data,df)) {
        colnames(df) <- colnames(.data)
      }
    }

    # Determine the position to insert the new row
    result <- determine_position(.data, df, .before, .after)
    .data <- result$data
    position <- result$position

    if (is.null(position)) {
      return(.data) # Return early if rows were appended
    }

    # Insert the new row at the specified position
    .data <- rbind(
      .data[seq_len(position - 1), , drop = FALSE],
      df,
      .data[seq(position, nrow(.data)), , drop = FALSE]
    )

    return(.data)
  }
}

#' Add an Empty Row to a Data Frame
#'
#' @description
#' Adds a single row of NA values to a data frame
#'
#' @param .data A data frame to which an empty row will be added
#'
#' @return Modified data frame with an additional empty row
#'
#' @export
add_empty_row <- function(.data) {

  if (!is.data.frame(.data)) {
    stop(".data = must be a data frame")
  }
  n_cols <- ncol(.data)
  df <- data.frame(matrix(ncol = n_cols, nrow = 1))
  colnames(df) <- colnames(.data)
  .data <- rbind(.data, df)
  message("A single row containing missing values has been added to the data\n")
  return(.data)
}

#' Determine Row Insertion Position
#'
#' @description
#' Calculates the position for inserting a new row in a data frame
#'
#' @param .data Original data frame
#' @param record New row to be inserted
#' @param .before Optional. Row number before which to insert
#' @param .after Optional. Row number after which to insert
#'
#' @return List containing the potentially modified data frame and insertion position
#'
#' @keywords internal
determine_position <- function(.data, record, .before = NULL, .after = NULL) {

  if (!is.null(.before)) {
    position <- .before
  } else if (!is.null(.after)) {
    # If we're adding after the last row, we should handle it like an append
    if (.after == nrow(.data)) {
      .data <- rbind(.data, record)
      return(list(data = .data, position = NULL))
    }
    position <- .after + 1
  } else {
    # Append the new row to the end if neither .before nor .after is specified
    .data <- rbind(.data, record)
    return(list(data = .data, position = NULL))
  }
  return(list(data = .data, position = position))
}
