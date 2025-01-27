deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}


dots_n <- function(...) {
  ...length()
}


same_length <- function(x, y) {
  # Case 1: If either input is a list
  if (is.list(x) || is.list(y)) {
    # Both are lists
    if (is.list(x) && is.list(y)) {
      return(identical(lengths(x), lengths(y)))
    }
    # One is a list, the other is a data frame
    if (is.list(x) && is.data.frame(y)) {
      return(identical(length(x), length(y)))
    }
    if (is.data.frame(x) && is.list(y)) {
      return(identical(length(x), length(y)))
    }
  }
  # Case 2: Neither is a list; compare overall lengths
  return(length(x) == length(y))
}


same_column <- function(x,y){
  ncol(x) == ncol(y)
}

equal_names <- function(x,y){
  all(colnames(x) == colnames(y))
}

extract <- `[`
extract2 <- `[[`

is_empty <- function(x) {
  (length(x) == 0)
}

`%!in%` <- Negate(`%in%`)

catch_dots <- function(...) {list(...)}
