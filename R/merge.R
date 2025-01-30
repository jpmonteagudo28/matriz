#' Merge Two literature matrices by Common Columns
#'
#' This function merges two literature matrices based on specified key columns, with options for
#' full or inner joins and duplicate column removal.
#'
#' @param .data A data frame to be merged.
#' @param .data2 A second data frame to be merged with `.data`.
#' @param by A character vector specifying the column(s) to merge by. Must exist in both data frames.
#' @param all A logical value indicating whether to perform a full join (`TRUE`) or an inner join (`FALSE`, default).
#' @param remove_dups A logical value indicating whether to remove duplicate columns before merging. Default is `TRUE`.
#' @param suffixes A character vector of length 2 specifying suffixes to apply to overlapping column names
#'        from `.data` and `.data2`, respectively. Default is `c(".x", ".y")`.
#' @param silent A logical value indicating whether to suppress messages about duplicate column removal. Default is `FALSE`.
#'
#' @return A merged data frame with specified join conditions applied.
#'
#' @details
#' The function first ensures that `.data` and `.data2` are valid data frames and checks
#' that the `by` columns exist in both. If `remove_dups = TRUE`, duplicate columns are
#' removed before merging. The function then performs either a full or inner join using
#' `dplyr::full_join()` or `dplyr::inner_join()`, respectively.
#'
#' @examples
#' df1 <- data.frame(id = c(1, 2, 3), value1 = c("A", "B", "C"))
#' df2 <- data.frame(id = c(2, 3, 4), value2 = c("X", "Y", "Z"))
#'
#' # Inner join (default)
#' merge_matrix(df1, df2, by = "id")
#'
#' # Full join
#' merge_matrix(df1, df2, by = "id", all = TRUE)
#'
#' # Remove duplicate columns before merging
#' df3 <- data.frame(id = c(1, 2, 3), value1 = c("A", "B", "C"), extra = c(1, 2, 3))
#' df4 <- data.frame(id = c(2, 3, 4), value2 = c("X", "Y", "Z"), extra = c(4, 5, 6))
#' merge_matrix(df3, df4, by = "id", remove_dups = TRUE)
#'
#' @importFrom dplyr full_join inner_join
#' @export
merge_matrix <- function(.data,
                         .data2,
                         by = NULL,
                         all = FALSE,
                         remove_dups = TRUE,
                         suffixes = c(".x", ".y"),
                         silent = FALSE) {

  stopifnot(is.data.frame(.data),
            is.data.frame(.data2)
            )

  # Convert `by` to character for NSE
  if (!is.null(by)) {
    by_syms <- rlang::ensyms(by)
    by_names <- sapply(by_syms, rlang::as_string, USE.NAMES = FALSE)
  } else {
    stop("Argument 'by' must be specified.")
  }

  # Remove duplicates if requested
  if (remove_dups) {
    if (!silent) message("Removing duplicate columns...")
    .data <- rid_dups(.data)
    .data2 <- rid_dups(.data2)
  }

  # Ensure the specified columns exist in both data frames
  missing_in_data <- setdiff(by_names, names(.data))
  missing_in_data2 <- setdiff(by_names, names(.data2))

  if (length(missing_in_data) > 0) {
    stop(paste("Column(s)", paste(missing_in_data, collapse = ", "), "not found in .data."))
  }

  if (length(missing_in_data2) > 0) {
    stop(paste("Column(s)", paste(missing_in_data2, collapse = ", "), "not found in .data2."))
  }

  # Perform the join
  .data <- if (all) {
    dplyr::full_join(.data, .data2, by = by_names, suffix = suffixes)
  } else {
    dplyr::inner_join(.data, .data2, by = by_names, suffix = suffixes)
  }

  return(.data)
}
