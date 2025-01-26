#' Update Rows in a Data Frame Based on a Condition
#'
#' @description
#' Modifies the values in a specified column of a data frame for rows that meet a given condition.
#'
#' @param .data A data frame. The dataset to modify.
#' @param column A column in the data frame to update. Can be specified as a column name, index, or unquoted column symbol.
#' @param where A condition that determines which rows to update. Must evaluate to a logical vector of the same length as the number of rows in `.data`.
#' @param set_to The value to assign to the rows in the specified column where the `where` condition is `TRUE`.
#' @param ... Additional arguments (currently unused, reserved for future use).
#'
#' @return The modified data frame with updated values.
#'
#' @details
#' This function updates values in a specified column of a data frame for rows that satisfy the given condition.
#' The `column` parameter can be provided as:
#' - A numeric column index (e.g., `2`).
#' - A column name (e.g., `"value"`).
#' - An unquoted column symbol (e.g., `value`).
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   id = 1:5,
#'   value = c(10, 20, 30, 40, 50)
#' )
#'
#' # Update rows where id > 3
#' updated_df <- update_record(df, column = value, where = id > 3, set_to = 100)
#' print(updated_df)
#'
#' # Using column as a string
#' updated_df <- update_record(df, column = "value", where = id == 2, set_to = 99)
#' print(updated_df)
#'
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom rlang eval_tidy
#' @export

update_record <- function(.data,
                          column = NULL,
                          where = NULL,
                          set_to = NULL,
                          ...){

  stopifnot(is.data.frame(.data))

  # Handle column input
  if (is.numeric(column)) {
    # If column is numeric, convert to column name
    if (column > ncol(.data) || column < 1) {
      stop("Column index is out of range.")
    }
    column_name <- names(.data)[column]
  } else {
    # Capture and deparse column name
    column <- rlang::enquo(column)
    column_name <- rlang::as_name(column)
  }


  if (column_name %!in% names(.data)) {
    stop(paste("Column", column_name, "does not exist in the data frame."))
  }

  # Evaluate the `where` condition
  condition <- rlang::eval_tidy(rlang::enquo(where), .data)
  if (!is.logical(condition) || length(condition) != nrow(.data)) {
    stop("The `where` condition must evaluate to a logical vector of the same length as the data.")
  }

  # Update the rows in the specified column where the condition is TRUE
  .data[[column_name]][condition] <- set_to

  # Return the updated data frame
  return(.data)

}
