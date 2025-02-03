#' Retrieve Column Classes from deafult literature matrix.
#'
#' This function calls \code{init_matrix()} to obtain a matrix or data frame,
#' then extracts the class of each column. It returns a data frame containing
#' the class information for each column.
#'
#' @param ... extra arguments to pass as column names for the lierature matrix
#'
#' @return A data frame with one column named \code{class} that lists the class
#'   of each column from the matrix or data frame returned by \code{init_matrix()}.
#'
#' @examples
#' \dontrun{
#'   # Assuming init_matrix() is defined and returns a valid data frame or matrix
#'   class_info <- matriz_names()
#'   print(class_info)
#' }
#'
#' @details
#' The purpose of this function is to provide the user with a quick way to check the default names and classes
#' as the matrix is being filled instead of having to type `str(init_matrix())` every time the user forgets a category in the default matrix.
#'
#'
#' @export
matriz_names <- function(...) {

    # Retrieve the data from init_matrix() and store it in a variable

    data <- init_matrix(...)

    # Get the class of each column using sapply
    column_classes <- sapply(data, class)

    # Create and return a data frame with the names and their corresponding classes
    return(data.frame(class = column_classes,
                      stringsAsFactors = FALSE))
}
