deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}


dots_n <- function(...) {
  dots <- deparse_dots(...)
  return(length(dots))
}


same_length <- function(x, y) {
  length(x) == length(y)
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
