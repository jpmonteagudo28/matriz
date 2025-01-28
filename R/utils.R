#' Deparse dots arguments into character vector
#'
#' @description
#' Takes dots arguments and deparses them into a character vector.
#'
#' @param ... Arguments to deparse
#' @return A character vector containing the deparsed expressions
#' @keywords internal
#'
deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}

#' Count number of dots arguments
#'
#' @description
#' Returns the number of arguments passed via ...
#'
#' @param ... Arguments to count
#' @return Integer length of dots arguments
#' @keywords internal
dots_n <- function(...) {
  ...length()
}

#' Check if two objects have the same length
#'
#' @description
#' Compares lengths of two objects, handling special cases for lists and data frames.
#'
#' @param x First object to compare
#' @param y Second object to compare
#' @return Logical indicating if objects have same length
#' @keywords internal

same_length <- function(x, y) {

  if(is.function(x) || is.function(y)){
    stop("Invalid input")
  }

  if (is.null(x) && is.null(y)) {
    return(TRUE)
  }

  if(is.null(x) && is_empty(x) || is.null(y) && is_empty(y)){
    return(FALSE)
  }

  # Case 1: If either input is a list
  if (is.list(x) || is.list(y)) {
    # Both are lists
    if (is.list(x) && !is.data.frame(x) && is.list(y) && !is.data.frame(y)) {
      return(identical(length(x), length(y)))
    }
    # One is a list, the other is a data frame
    if (is.list(x) && !is.data.frame(x) && is.data.frame(y) && !is.list(y)) {
      return(identical(length(x), length(y)))
    }
    if (is.data.frame(x) && !is.list(x) && is.list(y) && !is.data.frame(y)) {
      return(identical(length(x), length(y)))
    }
  }
  # Case 2: Neither is a list; compare overall lengths
  return(length(x) == length(y))
}

#' Check if two objects have the same number of columns
#'
#' @description
#' Compares number of columns between two objects.
#'
#' @param x First object to compare
#' @param y Second object to compare
#' @return Logical indicating if objects have same number of columns
#' @keywords internal
same_column <- function(x,y){
  ncol(x) == ncol(y)
}

#' Check if two objects have identical column names
#'
#' @description
#' Compares column names between two objects element by element.
#'
#' @param x First object to compare
#' @param y Second object to compare
#' @return Logical indicating if objects have identical column names
#' @keywords internal
equal_names <- function(x,y){
  all(colnames(x) == colnames(y))
}

#' Extract elements
#'
#' @description
#' Alias for base R extract operator `[`
#'
#' @keywords internal
extract <- `[`

#' Extract single element
#'
#' @description
#' Alias for base R extract operator `[[`
#'
#' @keywords internal
extract2 <- `[[`

#' Check if object is empty
#'
#' @description
#' Tests if an object has length zero.
#'
#' @param x Object to test
#' @return Logical indicating if object has zero length
#' @keywords internal
is_empty <- function(x) {
  (length(x) == 0)
}


#' Not in operator
#'
#' @description
#' Negation of the base R %in% operator.
#'
#' @keywords internal
#' @noRd
`%!in%` <- Negate(`%in%`)
