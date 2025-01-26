#' Delete Records from a Data Frame
#'
#' Deletes specific rows from a data frame or clears the entire data frame by leveraging the `truncate` function.
#' If no position is provided, it will issue a message and either return the unchanged data or use `truncate`
#' to empty the data frame, depending on additional arguments.
#'
#' @param .data A data frame from which records will be deleted.
#' @param position A numeric vector specifying the row positions to be deleted. If `NULL`, behavior is determined
#'   by the number of rows in the data frame and additional arguments passed to the `truncate` function.
#' @param ... Additional arguments passed to the `truncate` function. Specifically, the `keep_rows` argument
#'   can be used to decide whether non-NA cells in the data frame are cleared when truncating.
#'
#' @return A modified data frame with the specified rows removed. If `position` is `NULL`, the function either
#'   returns the original data frame or an empty data frame, based on the `keep_rows` argument in the `truncate` function.
#'
#' @details
#' - If `position` is `NULL` and the data frame has more than one row, a message is issued, and no records are deleted.
#' - If `position` is a numeric vector, the specified rows are deleted using `dplyr::slice()`.
#' - If `position` is empty or invalid (e.g., not numeric), the function stops with an appropriate error message.
#' - When no rows remain after deletion, the function calls `truncate` to handle the data frame, with behavior
#'   controlled by the `keep_rows` argument passed through `...`.
#'
#' @examples
#' df <- data.frame(A = 1:5, B = letters[1:5])
#'
#' # Delete a specific row
#' delete_record(df, position = 2)
#'
#' # Delete multiple rows
#' delete_record(df, position = c(2, 4))
#'
#' # Use truncate to clear the data frame
#' delete_record(df, position = NULL, keep_rows = FALSE)
#'
#' # Keep non-NA cells but empty rows
#' delete_record(df, position = NULL, keep_rows = TRUE)
#'
#' @importFrom dplyr slice
#' @export
delete_record <- function(.data,position = NULL,...){

  dots <- list(...)
  stopifnot(is.data.frame(.data))

  if (is.null(position)) {
    if (nrow(.data) > 1) {
      message("No records have been deleted. Are you looking to delete all records from literature matrix? Use 'truncate' instead.")
      return(.data)
    } else {
      # Handle truncation for a single-row data frame
      if (is_empty(dots)) {
        .data <- truncate(.data)
      } else {
        .data <- truncate(.data,dots)
      }
      message("Only one row deleted. The result is an empty data frame.")
      return(.data)
    }
  }

  # Check position argument validity
  if (!is.numeric(position)) {
    stop("The 'position' argument must be a numeric vector. Row deletion not performed.")
  }

  if (is_empty(position)) {
    stop("The 'position' is empty and length == 0. Provide a non-empty argument of length >= 1.")
  }

  # Delete specified rows
  .data <- .data |> dplyr::slice(-position)
  return(.data)
}
