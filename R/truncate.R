#' Truncate a Data Frame or Matrix
#'
#' @description
#' Remove all rows from a literature matrix but preserve the general structure. Mimics SQL's TRUNCATE operation by clearing data while preserving structure.
#'
#' @param .data A data frame or matrix to be truncated
#' @param keep_rows Logical. If TRUE, replaces non-NA values with NA instead of removing all data
#'
#' @return An empty data frame or matrix with the same structure as the input
#'
#' @examples
#' # Completely empty a data frame
#' df <- data.frame(x = 1:3, y = 4:6)
#' truncate(df)
#'
#' # Replace non-NA values with NA while keeping structure
#' truncate(df, keep_rows = TRUE)
#'
#' @export
truncate <- function(.data, keep_rows = FALSE){

  if(keep_rows){
    .data[which(!is.na(.data), arr.ind = TRUE)] <- NA
    return(.data)
  }
  .data[0, , drop = FALSE]
}
