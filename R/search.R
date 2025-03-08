#' Search and Filter Records in a literature matrix
#'
#' @description
#' Filters a literature matrix based on a specified condition, with the option to restrict the search
#' to a specific column. The function supports both column names and numeric indices for column selection.
#'
#' @param .data A data frame to search within.
#' @param column Optional. The column to search in, specified either by name or numeric index.
#'              If NULL (default), the search is performed across all columns.
#' @param where A logical expression that defines the search condition.
#'             Must evaluate to a logical vector of the same length as the number of rows in `.data`.
#'
#' @return A filtered data frame containing only the rows that match the search condition.
#'         If a specific column was selected, only that column is returned.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   name = c("John", "Jane", "Bob", "Alice", "John"),
#'   age = c(25, 30, 35, 28, 40)
#' )
#'
#' # Search across all columns where age > 30
#' search_record(df, where = age > 30)
#'
#' # Search only in the name column for "John"
#' search_record(df, column = "name", where = name == "John")
#'
#' # Search using column index
#' search_record(df, column = 2, where = name == "Jane")
#'
#' @export
#'
#' @importFrom rlang enquo eval_tidy quo_get_expr as_name
#' @importFrom dplyr filter
search_record <- function(.data,
                          column = NULL,
                          where = NULL){

  stopifnot(is.data.frame(.data))

  where_quo <- rlang::enquo(where)

  if (!is.null(column)) {

    column_quo <- rlang::enquo(column)

    # If column is specified by index, get column name
    if (is.numeric(rlang::quo_get_expr(column_quo))) {
      column_idx <- rlang::eval_tidy(column_quo)

      if (column_idx > ncol(.data) || column_idx < 1) {
        stop("Column index is out of range.")
      }
      column_name <- names(.data)[column_idx]
    } else {
      # If column is specified by name
      column_name <- rlang::as_name(column_quo)
    }

    if (column_name %!in% names(.data)) {
      stop(paste("Column", column_name, "does not exist in the data frame."))
    }

    # Apply the 'where' condition on the specific column
    condition <- rlang::eval_tidy(where_quo, data = .data)
    if (!is.logical(condition) || length(condition) != nrow(.data)) {
      stop("The `where` condition must evaluate to a logical vector of the same length as the data.")
    }

    # Return filtered data based on column and where condition
     .data <- .data[condition,column_name, drop = FALSE]

    return(.data)

  } else {
    # If no specific column is provided, apply the `where` condition across all columns
    condition <- rlang::eval_tidy(where_quo, data = .data)

    if (!is.logical(condition) || length(condition) != nrow(.data)) {
      stop("The `where` condition must evaluate to a logical vector of the same length as the data.")
    }

    # Filter data based on the condition applied across all columns
    .data <- .data[condition, , drop = FALSE]

    return(.data)
  }
}
