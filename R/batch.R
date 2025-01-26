add_batch_record <- function(.data,
                      ...,
                      .before = NULL,
                      .after = NULL) {

  if(!is.data.frame(.data)){
    stop("add_record(.data = 'must be a data frame')")
  }

  if (dots_n(...) == 0L) {
    .data <- add_empty_row(.data)
    return(.data)
  }
    dots <- list(...)

    new_records <- do.call(rbind, lapply(dots, function(dot) {

      if (is.vector(dot) || is.list(dot)) {
        if (length(dot) != ncol(.data)) {
          stop(sprintf("Vector input must have length %d (number of columns in test)", ncol(.data)))
        }
        # Convert vector to data frame while preserving column types
        df <- as.data.frame(matrix(dot, nrow = 1), stringsAsFactors = FALSE)
        colnames(df) <- colnames(.data)

      } else if (is.data.frame(dot)) {
        if (!same_column(dot, .data)) {
          stop("Data frame input must have the same number of columns as test")
        }
        if (!equal_names(dot, .data)) {
          colnames(dot) <- colnames(.data)
        }
        df <- dot
      } else {
        stop("Each input must be either a vector or a data frame")
      }
      return(df)
    }))

    result <- determine_position(.data, df, .before, .after)
    .data <- result$data
    position <- result$position

    if (is.null(position)) {
      return(.data) # Return early if rows were appended
    }

    .data <- rbind(
      .data[seq_len(position - 1), , drop = FALSE],
      new_records,
      .data[seq(position, nrow(.data)), , drop = FALSE]
    )

    return(.data)
}
